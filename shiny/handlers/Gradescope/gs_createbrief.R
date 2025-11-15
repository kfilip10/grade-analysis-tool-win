

# UI ----

gs_createbrief_ui <- function(){
  
  div(
    fluidPage(
        textInput(inputId="gs_courseTitle", label=HTML("<b>Course name:</b>"), value = "PH2X1"),
        textInput(inputId="gs_eventTitle", label=HTML("<b>Event name:</b>"), value = "WPR "),
        p("Use the slider below to set a threshold for the percentage of cuts applied to the brief. For example choosing 15% means that any cuts applied to at least 15% or more of the test population will be included and cuts that are not applied widely will be filtered out. If you choose 100% there will be practically 0 cuts in the brief. Choose 0% for all cuts to be included, which you can manually delete."),
        sliderInput("gs_cut_filter_threshold",
                    label = "Set Cut Filter Threshold (%)",
                    min = 0, 
                    max = 100,
                    value = 15,  # Default value
                    step = 1),
        br(),
        checkboxInput("scores_in_canvas", "Check this box if scores are already in canvas", value = FALSE),
        br(),
        
        textOutput("scores_in_canvas_state"),
        h3("Generate Brief"),
        actionButton("gs_createbriefbutton", "Generate Brief",style = "width: 400px;"),
        br(),
        h3("Download Brief"),
        
        shinyjs::disabled(downloadButton("gs_briefdownload", "Download Brief",
                                style = "background-color: #28a745; color: #ffffff;width: 400px;")),
        h3("Proceed to Canvas Grades Upload"),
        actionButton("gs_brief_next", label = HTML("Confirm and Proceed <span class='arrow-icon'>&rarr;</span>")), 
                     
  
  )
)
}

# Server ----

gs_createbrief_server <- function(input, output, session,gs_data,gs_wizard_status){
  
  #reactive for Course Title
  course_title <- reactive({
    req(input$gs_courseTitle)
    return(input$gs_courseTitle)
  })
  
  
  #reactive for Event Title
  event_title <- reactive({
    req(input$gs_eventTitle)
    return(input$gs_eventTitle)
  })
  
  #reactive for cut threshold
  cut_filter_threshold <- reactive({
    req(input$gs_cut_filter_threshold)
    return(input$gs_cut_filter_threshold / 100)  # Convert to decimal if needed
    })
  
  output$scores_in_canvas_state <- renderText({
    if (input$scores_in_canvas) {
      "You said your grades are in Canvas already, this will be used to calculate the pre/post score distribution."
    } else {
      "You said your grades are NOT in Canvas already, this will be used to calculate the pre/post score distribution."
    }
  })
  #create logic to merge the canvas and gs rosters
  #gs_data$upload_roster
  df_canvas <- reactiveVal()
  
  observe({    
    req(gs_data$canvas_roster, gs_data$gs_roster)
    #browser()
    
    df_canvas_roster <- gs_data$canvas_roster %>% mutate(email=(tolower(email)))
    
    #if error lowering email then report to check there is email in gradescope data
    df_gs <- gs_data$gs_roster %>%
      dplyr::rename(email = !!names(.)[tolower(names(.)) == "email"]) %>%
      dplyr::mutate(email = tolower(email))
    
    df_missing <- tolower(gs_data$missing_roster$email)
    # remove the missing roster from the df_canvas
    df_canvas_roster <- df_canvas_roster[!df_canvas_roster$email %in% gs_data$missing_roster$email,]
    
    
    
    #combine df_canvas with df_gs
    #emails should be joined now based on standardized function call and storage earlier
    df_canvas_adj <- full_join(df_canvas_roster, df_gs, by ="email")
    
    #remove incomplete rows (i.e. if they selected fewer entries)
    df_canvas_adj <- df_canvas_adj %>% filter(!is.na(user_id))
    
    #browser()
    
    df_canvas_adj <- df_canvas_adj %>% 
      rename(pre_points = grades.unposted_current_points,
             pre_percent = grades.unposted_current_score,
             grade = grades.unposted_current_grade)
    
    df_canvas_adj <- df_canvas_adj %>% 
      mutate(current_max=ifelse(is.na(pre_percent),0,pre_points/pre_percent*100))
    
    df_canvas(df_canvas_adj)
  })
    
  observeEvent(input$gs_createbriefbutton, {

      req(event_title(), course_title())
      
      tryCatch({
        
        withProgress(message = 'Generating brief', value = 0,min=0,max=1, {
          #pdf <- cutSheet()
          
          ## Canvas roster prep ----
          #DEBUG - loads a completed gs_data object for testing
          #test  <- reactiveValuesToList(gs_data)
          #saveRDS(test, "test/gs_data_complete.rds")
          #gs_data <- readRDS("test/gs_data_complete.rds")

          #data frame of grades from canvas
          ##DEBUG
          
          #browser()
          
          df_canvas_adj <- df_canvas()
          
          # Validate required data exists
          if(is.null(df_canvas_adj) || nrow(df_canvas_adj) == 0) {
            stop("Canvas roster data is missing or empty. Please ensure data is loaded properly.")
          }
          
          ## Scores in canvas, back calc ----
          if(input$scores_in_canvas){
            tryCatch({
            #if scores are in canvas adjust the roster to subtract the `score`
            #TODO: It would be nice to have a way for the user to specify the column names for the total score and max points
          #browser()
          score_col <- grep(gs_data$score_column, 
                            colnames(df_canvas_adj), 
                            value = TRUE, 
                            ignore.case = TRUE)
          
          total_col <- grep(gs_data$max_column, 
                            colnames(df_canvas_adj), 
                            value = TRUE, 
                            ignore.case = TRUE)
          
          # Validate columns were found
          if(length(score_col) == 0) {
            stop(paste0("Score column '", gs_data$score_column, "' not found in the data. Please check the column name."))
          }
          
          if(length(total_col) == 0) {
            stop(paste0("Max points column '", gs_data$max_column, "' not found in the data. Please check the column name."))
          }
          
          df_canvas_adj <- df_canvas_adj %>%
            mutate(pre_points = pre_points - .data[[score_col]],
                  current_max = current_max - .data[[total_col]])
          df_canvas_adj %>% group_by(grade) %>% summarise(n=n())
          df_canvas_adj %>% filter(grade=="F")
          
          df_canvas_adj <- df_canvas_adj %>% 
            mutate(pre_percent=pre_points/current_max)
          
          df_canvas_adj$grade <- sapply(
            df_canvas_adj$pre_percent,letter_grade,breaks,grades)
          
          df_canvas_adj$pre_percent <- df_canvas_adj$pre_percent*100
          
            }, error = function(e) {
              # More specific error handling for scores in canvas section
              error_msg <- paste0("Error processing Canvas scores: ", e$message)
              
              if (grepl("score_col|Score column", e$message)) {
                error_msg <- paste0(error_msg, "\n\nThe 'Total Score' column was not found. Please verify the column name matches exactly.")
              }
              if (grepl("total_col|Max points column", e$message)) {
                error_msg <- paste0(error_msg, "\n\nThe 'Max Points' column was not found. Please verify the column name matches exactly.")
              }
              
              stop(error_msg)
            })
          
          #df_canvas <- df_canvas_adj %>% mutate
          }
          else{
            #leave canvas scores as they are
          }
          
          # Validate required columns exist for post-processing
          required_cols <- c("Total Score", "Max Points")
          missing_cols <- required_cols[!required_cols %in% colnames(df_canvas_adj)]
          if(length(missing_cols) > 0) {
            stop(paste0("Missing required columns: ", paste(missing_cols, collapse = ", "), ". Please check your Gradescope data."))
          }
          #browser()
          df_canvas_adj <- df_canvas_adj %>% 
            mutate(post_points = pre_points+`Total Score`,
                  post_max = current_max+`Max Points`,
                  post_percent = post_points/post_max,
                  mge_percent = `Total Score`/`Max Points`)%>%
            rename(pre_grade = grade)
          
          #saveRDS(df_canvas_adj, "df.rds") #debugging 14NOV
          
          df_canvas_adj$mge_grade <- sapply(
            df_canvas_adj$mge_percent,letter_grade,breaks,grades)
          
          df_canvas_adj$post_grade <- sapply(
            df_canvas_adj$post_percent,letter_grade,breaks,grades)
          
          df_canvas_adj$post_percent <- df_canvas_adj$post_percent*100
          df_canvas_adj$mge_percent <- df_canvas_adj$mge_percent*100
          #browser()
          
          # Validate gs_data components exist
          if(is.null(gs_data$gs_question_groups) || length(gs_data$gs_question_groups) == 0) {
            stop("Question groups data is missing. Please ensure Gradescope data is properly loaded.")
          }
          
          if(is.null(gs_data$cuts_df)) {
            stop("Cuts data is missing. Please ensure Gradescope data is properly processed.")
          }
          
          # rename gs_data$df_scoregroups to gs_data$gs_question_groups
          question_df_list <- gs_data$gs_question_groups

          file_name <- paste0("Grade Brief - ", course_title(), " - ", event_title(), ".pptx")
          
          progress.tot <- length(question_df_list)+3 #title,version summary, and ppt creation
          
          incProgress(1/progress.tot, detail = paste("Calling Function"))

          # Validate function exists
          if(!exists("gs_makebriefmain")) {
            stop("Brief generation function 'gs_makebriefmain' not found. Please check if all required functions are loaded.")
          }
          gs_data$ppt <- gs_makebriefmain(
            question_df_list,
            df_canvas_adj,
            gs_data$missing_roster, # data frame of missing grades
            #DEBUG gs_data$cuts_df[[1]], # used if reading from RDS
            gs_data$cuts_df, # data frame of cuts
            cut_filter_threshold(),
            progress.tot,
            course_title(),
            event_title(),
            file_name
          )
          
          # Validate PPT was created successfully
          if(is.null(gs_data$ppt)) {
            stop("Failed to generate PowerPoint presentation. Please check your data and try again.")
          }
          
          ##DEBUG
          #browser()
          #print(gs_data$ppt, target= "test/ppt.pptx")
          
          setProgress(1, detail = paste("Complete!"))
        })
        
        # Success actions
        shinyjs::enable("gs_briefdownload")
        
        # Success modal
        showModal(modalDialog(
          title = HTML("<span style='color: green;'>✓ Brief Created Successfully</span>"),
          HTML(paste0(
            "<p><strong>Brief generated successfully!</strong></p>",
            "<p>Course: ", course_title(), "</p>",
            "<p>Event: ", event_title(), "</p>",
            "<p>You may now download the brief using the download button.</p>"
          )),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close"),
            downloadButton("modal_download", "Download Now", 
                          style = "background-color: #28a745; color: white;")
          )
        ))
        
      }, error = function(e) {
        # Error modal with detailed information
        error_details <- e$message
        
        # Categorize error types for user-friendly messages
        if(grepl("Canvas roster data is missing", error_details)) {
          user_message <- "Canvas roster data is not loaded properly."
          suggestion <- "Please go back to the Canvas integration tab and ensure your roster data is loaded correctly."
        } else if(grepl("column.*not found", error_details, ignore.case = TRUE)) {
          user_message <- "Required column not found in your data."
          suggestion <- "Please check that your Gradescope export and Canvas data have the expected column names."
        } else if(grepl("Question groups.*missing", error_details)) {
          user_message <- "Gradescope question data is missing."
          suggestion <- "Please ensure you have properly uploaded and processed your Gradescope data."
        } else if(grepl("gs_makebriefmain.*not found", error_details)) {
          user_message <- "Brief generation function is not available."
          suggestion <- "This appears to be a system error. Please restart the application and try again."
        } else {
          user_message <- "An unexpected error occurred during brief generation."
          suggestion <- "Please check your data inputs and try again. If the problem persists, contact support."
        }
        
        showModal(modalDialog(
          title = HTML("<span style='color: red;'>⚠ Error Generating Brief</span>"),
          HTML(paste0(
            "<p><strong>", user_message, "</strong></p>",
            "<p><em>Suggestion:</em> ", suggestion, "</p>",
            "<hr>",
            "<details>",
            "<summary style='cursor: pointer;'>Technical Details (click to expand)</summary>",
            "<pre style='background-color: #f8f9fa; padding: 10px; margin-top: 10px; white-space: pre-wrap; font-size: 12px;'>",
            htmltools::htmlEscape(error_details),
            "</pre>",
            "</details>"
          )),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        
        # Log the error for debugging
        cat("Brief generation error:", error_details, "\n")
      })
  })
# Add download handler for the modal download button
output$modal_download <- downloadHandler(
  filename = function() {
    paste0("Brief - ", course_title(), " - ", event_title(), ".pptx")
  },
  content = function(file) {
    if (is.null(gs_data$ppt)) {
      return(NULL)
    }
    print(gs_data$ppt, target = file)
  }
)

## Download Brief ----
output$gs_briefdownload <- downloadHandler(
  filename = function() {
    paste0("Brief - ", course_title(), " - ", event_title(), ".pptx")
  },
  content = function(file) {
    try
    if (is.null(gs_data$ppt)) {
      showModal(modalDialog(
        title = "No Brief Created",
        "Please create a brief before downloading.",
        easyClose = TRUE
      ))
      return(NULL)  # Stop execution to prevent download
    }
    print(gs_data$ppt,target = file)
  }
)
  
  ## 'Next' Button ----
  observeEvent(input$gs_brief_next, {
    #req(question_groups())
    #compile_questions()
    showModal(
      modalDialog(
        title = "Confirmation",
        "Proceed to the canvas upload tab? (clicking yes will not upload the roster yet)",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_gs_brief", "Yes, Proceed")
        )
      )
    )
  })
  
  observeEvent(input$confirm_gs_brief, {

    gs_data$upload_roster <- df_canvas()
    
    #DEBUG: save below
    #saveRDS(reactiveValuesToList(gs_data),"test/gs_data_upload_test.rds")
    
    
    removeModal()
    
    
  
  })  
  
}
