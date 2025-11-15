# UI ----

gs_canvas_ui <- function() {
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 2,
      withSpinner(uiOutput("gs_connectionstatus_ui")),
      shiny::tags$p("If you don't see your section listed, click the 'Refresh Section Data' button:"),
      actionButton(inputId = "gs_refreshsectiondata", label = "Refresh Section Data"),
      shiny::h4("Courses Selected:"),
      shiny::uiOutput("gs_selectedcourses"),
    ),
    shiny::mainPanel(
      h2("Collect Grades from Canvas"),
      actionButton("gs_canvas_next",
        label = HTML("Confirm and Proceed <span class='arrow-icon'>&rarr;</span>"),
        class = "custom-button"
      ),
      div(
        style = "margin-top: 0.5in;",
        tabsetPanel(
          tabPanel(
            "Select courses",
            br(),
            p("Select your courses from the list below. Click the button to load
                   grade data for the selected courses."),
            actionButton("gs_loadgrades", "Load Roster for Selected Courses"),
            div(
              id = "gs_coursetable", # id is for toggling, but not needed on this tab.
              dataTableOutput("gs_coursetable")
            ),
          ),
          tabPanel(
            "Canvas Course Roster",
            br(),
            p("This is the full enrollment roster for validation and download."),
            div(
              id = "gs_rostertable", # id is for toggling, but not needed on this tab.
              dataTableOutput("gs_canvasrostertable")
            ),
          ),
          tabPanel(
            "Cadets from Canvas not found in Gradescope",
            br(),
            p("This is the list of cadets not found in gradescope Data."),
            div(
              id = "gs_cadeterrorlist", # id is for toggling, but not needed on this tab.
              dataTableOutput("gs_missinglist")
            ),
          )
        )
      )
    )
  )
}

# Server ----

gs_canvas_server <- function(input, output, session, gs_data, gs_wizard_status, canvas_api_token) {
  ## Connection Status----
  gs_connectionstatus <- reactive({
    if (!is.null(canvas_api_token())) {
      set_canvas_token(canvas_api_token())
      set_canvas_domain(api_domain)
      api_test()
    } else {
      NULL
    }
  })
  # If the connection is successful, render the UI for the page
  # This is the main UI for API UI
  output$gs_connectionstatus_ui <- renderUI({
    tagList(
      tags$h3("Canvas API Connection"),
      create_connection_ui(gs_connectionstatus())
    )
  })


  
  # Add a reactive trigger for the refresh button
  gs_refresh_trigger <- reactiveVal(0)
  
  # observe actionbutton to load the section data
  observeEvent(input$gs_refreshsectiondata, {
    gs_load_section_start()
    
    gs_refresh_trigger(gs_refresh_trigger() + 1) # Increment to trigger reactive
  })
  
  gs_load_section_start <- function() {
    # course_list_df has the courses
    # includes: sections - dataframe of sections in each course
    # total students: total number of students in each course
    # open a box that says 'Loading course data, please wait' that also has a with progress
    # Open a modal dialog before loading data
    showModal(modalDialog(
      title = "Loading",
      "Loading course data, please wait...",
      footer = NULL,
      easyClose = FALSE
    ))
    withProgress(message = "Loading course data, please wait...", value = 0, {
      course_list_df <- get_course_list(include = c("sections", "total_students", "term"))
      
      # browser()
      sections <- lapply(course_list_df$id, function(x) {
        incProgress(1 / length(unique(course_list_df$id)),
                    detail = paste("Loading course data for course", x)
        )
        get_section_info(x, include = "total_students")
      })
      
      
      # end_time <- Sys.time()
      # end_time - start_time
      sections.comb <- bind_rows(sections)
      
      # filter no student courses and non-USMA courses
      sections.comb <- sections.comb %>% filter(!total_students == 0, !is.na(sis_section_id))
      # browser()
      ## Creating dataframe of sections##
      #' name' - section name
      #' total_students' - total number of students enrolled in the section
      #' section_id' section id from get_section_info 'id'
      # rename id in sections to section_id
      names(sections.comb)[names(sections.comb) == "id"] <- "section_id"
      #' #'id' course id - from id
      names(sections.comb)[names(sections.comb) == "course_id"] <- "id"
      
      #' course' course name extracted from name
      sections.comb <- sections.comb %>% mutate(course = str_extract(sis_section_id, "[A-Z]{2}[0-9]{3}"))
      
      #' # 'term' - from course_list_df term.name
      # Extract the term from course_list_df term.name by matching id
      sections.comb <- sections.comb %>% left_join(course_list_df %>% select(id, term.name), by = "id")
      names(sections.comb)[names(sections.comb) == "term.name"] <- "term"
      
      hours <- sections.comb$name %>% str_extract("\\b[A-Z0-9]{4}\\b\\s+\\d+")
      section <- str_extract(hours, "\\d+\\s*$")
      hours <- str_extract(hours, "\\b[A-Z0-9]{4}\\b")
      # If section is 'NA' then label as 'NA'
      # 'section_hour' - extracted hour from section name 'A1B1'
      # 'section' - section number from section name '3' or '4'
      sections.comb <- sections.comb %>% mutate(section_hour = hours, section = section)
      sections.comb <- sections.comb %>% mutate(instr_name = str_extract(name, "(?<=-)[^-]+$"))
      # returns sections.comb
      #' name' - section name
      #' id' course id - from id
      #' course' course name extracted from name
      #' total_students' - total number of students enrolled in the section
      #' section_id' section id from get_section_info 'id'
      #' # 'term' - from course_list_df term.name
      # 'section_hour' - extracted hour from section name 'A1B1'
      # 'section' - section number from section name '3' or '4'
      # 'instr_name' - name of instructor from section name
    })
    removeModal()
    # save sections.comb to section_defaults_path defined in global.r
    saveRDS(sections.comb, SECTION_DEFAULTS_PATH)
    return(sections.comb)
  }
  
  ## Canvas Courses
  ##   #reactive value for the course list only if the connection is successful
  gs_courselist <- reactive({
    # Add dependency on refresh trigger
    gs_refresh_trigger()
    
    if (input$navbarID == "gradescope_panel") {  # Update this to match your navbar panel ID
      if (!is.null(gs_connectionstatus()) &&
          gs_connectionstatus() == "Success" &&
          gs_wizard_status$gs_scores_completed == TRUE) {
        
        # Check if there is a file at SECTION_DEFAULTS_PATH
        if (file.exists(SECTION_DEFAULTS_PATH)) {
          # Check the date the file was written
          file_date <- file.info(SECTION_DEFAULTS_PATH)$mtime
          file_date <- as.Date(file_date)
          
          # If it is older than 10 days then ask if they want to reload the data
          if (Sys.Date() - file_date > 10) {
            cat("File is older than 10 days, reloading section data\n")
            return(gs_load_section_start())
          } else {
            # If it is within 10 days then load the data from file
            cat("Loading section data from recent file\n")
            return(readRDS(SECTION_DEFAULTS_PATH))
          }
        } else {
          # If there isn't a file then get the section list data
          cat("No section defaults file found, loading fresh data\n")
          return(gs_load_section_start())
        }
      } else {
        return(NULL)
      }
    }
    return(NULL) # Return NULL if not in the gs_canvas panel or connection failed
  })



  #### Course Selection ####
  gs_selectedcourses <- reactiveVal(data.frame())


  #### Course Selection - Datatable w. Filters ####
  output$gs_coursetable <- DT::renderDataTable(
    {
      table_courses(gs_courselist()) # Retrieve reactive
    },
    options = list(
      pageLength = 50 # Set default rows per page to 50
    )
  )

  observeEvent(input$gs_coursetable_rows_selected, {
    selected_indices <- input$gs_coursetable_rows_selected
    selected_courses_df <- gs_courselist()[selected_indices, , drop = FALSE]
    gs_selectedcourses(selected_courses_df)

    # print(selected_items)
  })
  output$gs_selectedcourses <- renderUI({
    selected_data <- gs_selectedcourses()
    if (nrow(selected_data) > 0) {
      tags$ul(
        # Iterate over each row of the selected data frame
        lapply(1:nrow(selected_data), function(i) {
          course <- selected_data[i, "name"]
          tags$li(paste(course))
        })
      )
    }
    # browser()
    else {
      h5("No courses selected")
    }
  })

  ## Load Canvas Roster ----
  gs_canvasroster <- reactiveVal()
  gs_cadetsmissing <- reactiveVal()
  gs_errorlist <- reactiveVal()

  observeEvent(input$gs_loadgrades, {
    # Check 1 if courses are selected
    if (is.null(gs_selectedcourses()) || nrow(gs_selectedcourses()) == 0) {
      showModal(modalDialog(
        title = HTML("<span style='color: orange;'>⚠ No Courses Selected</span>"),
        HTML(paste0(
          "<p><strong>Please select courses before loading roster data.</strong></p>",
          "<p>Go to the 'Select courses' tab and click on the courses you want to include, then try again.</p>"
        )),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return() # Stop execution here
    }
    
    # Check 2: Gradescope roster data exists
    if (is.null(gs_data$gs_roster) || nrow(gs_data$gs_roster) == 0) {
      showModal(modalDialog(
        title = HTML("<span style='color: red;'>⚠ No Gradescope Data Found</span>"),
        HTML(paste0(
          "<p><strong>You must upload and process Gradescope score files first.</strong></p>",
          "<p>Please go back to the 'Upload Scores' tab and complete the Gradescope data upload before loading Canvas roster data.</p>",
          "<p>The Canvas roster is used to match against your Gradescope data, so Gradescope data must be loaded first.</p>"
        )),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return() # Stop execution here
    }
    
    
    
    # Existing code continues here...
    if (!is.null(gs_selectedcourses())) {
      showModal(modalDialog(
        title = "Loading Progress",
        "Please wait while we load the data...",
        div(id = "progress-container", style = "width: 100%;"),
        easyClose = FALSE,
        footer = NULL
      ))

      # DEBUG - reads canvas data from file for testing
      # canvas_roster <- readRDS("test/canvas_roster.rds")
      # saveRDS(canvas_roster, "test/canvas_roster.rds")

      canvas_roster <- get_student_roster(gs_selectedcourses(), gs_errorlist(), INSTRUCTOR_SEARCH_KEY)
      
      canvas_roster <- standardize_email_column(canvas_roster)
      
      #set all emails to lower case

      gs_canvasroster(canvas_roster)

      # DEBUG - reads gradescope data from file for testing
      # saveRDS(gs_roster, "test/gs_roster.rds")
      # gs_roster <- readRDS("test/gs_roster.rds")
      
      #browser() #DEBUGGING 14NOV
      
      gs_roster <- gs_data$gs_roster


      # compare canvas_roster to gs_roster to find missing cadets
      # finds cadets in canvas_roster that are not in gs_roster
      # Check if SID exists in gs_roster
      # make gs_roster all lower case column names

      #gs_roster <- gs_roster %>% rename_all(tolower)
      #browser()
      if ("email" %in% colnames(gs_roster)) {
        # Fallback to comparison by email
        df_missing <- canvas_roster %>%
          anti_join(gs_roster, by = "email")
        
        matching_method <- "Email (no SID column found)"
        
      } else {
        # No SID or email columns
        showModal(modalDialog(
          title = "Error: Missing Columns",
          "The Gradescope Roster does not contain the email column (labelled 'email' or 'Email'). Please check that your gradescope roster has an email column to check for matches to Canvas. This error does not stop you from proceeding but may cause data loss.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        df_missing <- canvas_roster # Assume all missing
        matching_method <- "Error"
      }
      
      # Show user which matching method was used
      cat("Matching method used:", matching_method, "\n")
      cat("Students matched:", nrow(canvas_roster) - nrow(df_missing), "\n")
      cat("Students missing from Gradescope:", nrow(df_missing), "\n")

      if (nrow(df_missing) > 0) {
        gs_cadetsmissing(df_missing)
      } else {
        gs_cadetsmissing(NULL)
      }
    }
    removeModal()
    if (nrow(gs_roster) > nrow(canvas_roster)) {
      showModal(modalDialog(
        title = "Note: The Selected Canvas Roster is Small",
        "The Canvas Roster is smaller than the Gradescope Roster. Please check that you have the correct courses selected in Canvas. This error does not stop you from proceeding but is for your consideration.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      # return() # Stop further execution
    } else {
      showModal(modalDialog(
        title = "Roster Loaded",
        "Courses have been successfully loaded!",
        footer = modalButton("Close"),
        easyClose = TRUE
      ))
    }
  })

  #### Canvas Roster - Datatable w. Filters ####
  output$gs_canvasrostertable <- DT::renderDataTable({
    table_canvasRoster(gs_canvasroster()) # Retrieve reactive
  })


  ## Missing from Gradescope ----
  output$gs_missinglist <- DT::renderDataTable(
    {
      gs_cadetsmissing()
    },
    filter = "top", # Enable column-based filtering
    extensions = "Buttons", # Enable the Buttons extension
    options = list(
      dom = "Bfrtip", # Define the table control elements
      buttons = list(
        list(
          extend = "excel",
          text = "Download Excel (All)",
          exportOptions = list(
            modifier = list(page = "all") # Export all rows, not just visible
          )
        )
      ),
      pageLength = 100, # Set the number of rows per page
      autoWidth = TRUE,
      columnDefs = list(
        list(width = "40px", targets = c(2, 3)) # Adjust specific column width if needed
      )
    )
  )





  ## Next button ----
  observeEvent(input$gs_canvas_next, {
    # Ensure `df_missing` is calculated beforehand
    if (is.null(gs_canvasroster())) {
      showModal(
        modalDialog(
          title = "Error",
          "You must load the Canvas course roster before proceeding.",
          footer = modalButton("Close")
        )
      )
      return()
    }

    # browser()
    # Get the number of missing cadets
    # if gs_
    if (is.null(gs_cadetsmissing())) {
      # Show the modal dialog
      showModal(
        modalDialog(
          title = "Confirmation",
          tagList(
            p("All Cadets have exams in gradescope, yay."),
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_gs_canvas", "Yes, Proceed")
          )
        )
      )
    } else {
      numMissing <- nrow(gs_cadetsmissing())
      showModal(
        modalDialog(
          title = "Confirmation",
          tagList(
            p("There are ", numMissing, " cadets with no exam data in Gradescope, for your information. You can still proceed.")
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_gs_canvas", "Yes, Proceed")
          )
        ),
      )
    }
  })
  #

  observeEvent(input$confirm_gs_canvas, {
    gs_data$canvas_roster <- gs_canvasroster()
    gs_data$missing_roster <- gs_cadetsmissing()
  })
}
