

# UI ----

gs_createbrief_ui <- function(){
    mainPanel(
      fluidRow(
        textInput(inputId="gs_courseTitle", label=HTML("<b>Course name:</b>"), value = "PH2X1"),
        textInput(inputId="gs_eventTitle", label=HTML("<b>Event name:</b>"), value = "WPR "),
        p("Use the slider below to set a threshold for the percentage of cuts applied to the brief. For example choosing 15% means that any cuts applied to at least 15% or more of the test population will be included and cuts that are not applied widely will be filtered out. If you choose 100% there will be practically 0 cuts in the brief. Choose 0% for all cuts to be included, which you can manually delete."),
        sliderInput("gs_cut_filter_threshold",
                    label = "Set Cut Filter Threshold (%)",
                    min = 0, 
                    max = 100,
                    value = 15,  # Default value
                    step = 1),
        h2("Generate Brief"),
        actionButton("gs_createbriefbutton", "Generate Brief"),
        disabled(downloadButton("gs_briefdownload", "Download Brief")),
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
  
  cut_filter_threshold <- reactive({
    req(input$gs_cut_filter_threshold)
    return(input$gs_cut_filter_threshold / 100)  # Convert to decimal if needed
    })
  
  observeEvent(input$gs_createbriefbutton, {

    req(event_title(), course_title())
    
    tryCatch({
      
      withProgress(message = 'Generating brief', value = 0,min=0,max=1, {
        #pdf <- cutSheet()
        
        
        
        #DEBUG Step
        #gs_data <- readRDS("gs_data.rds")
        
        #click the gs_data.rds to open it
        #saveRDS(question_df, "test/question_df.rds")
        question_df_list <- gs_data$gs_scoregroups

        cuts <- gs_data$cuts_df
        
        file_name <- paste0("Grade Brief - ", course_title(), " - ", event_title(), ".pptx")
        
        progress.tot <- length(question_df_list)
        
        incProgress(1/progress.tot, detail = paste("Calling Function"))

                gs_data$ppt <- gs_makebriefmain(
                  question_df_list,
                  cuts,
                  cut_filter_threshold(),
                  file_name
                )
              
        
        setProgress(1, detail = paste("Complete!"))
      })
      # Automatically trigger download after creation
      shinyjs::runjs("$('#download_ppt').trigger('click');")
      

      # Define the download handler
      output$download_ppt <- downloadHandler(
        filename = function() {
          file_name
        },
        content = function(file) {
          file.copy(file_name, file)  # Copy generated file to download location
        }
      )
      
    }, error = function(e) {
      # Show error modal in case of failure
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        footer = modalButton("Close")
      ))
    })
  })
  
  #### Download Brief ####
  output$gs_briefdownload <- downloadHandler(
    filename = function() {
      paste0("Brief - ", course_title(), " - ", event_title(), ".pptx")
    },
    content = function(file) {print(ppt,target= file)
    }
  )
}
  