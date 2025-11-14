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





  observeEvent(input$gs_refreshsectiondata, {
    if (!is.null(gs_connectionstatus()) && gs_connectionstatus() == "Success") {
      gs_load_courses(gs_connectionstatus())
    } else {
      NULL
    }
  })


  ## Canvas Courses
  ##   #reactive value for the course list only if the connection is successful
  gs_courselist <- reactive({
    if (!is.null(gs_connectionstatus()) &&
      gs_connectionstatus() == "Success" &&
      gs_wizard_status$gs_scores_completed == TRUE) {
      gs_load_courses(gs_connectionstatus())
    } else {
      NULL
    }
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
    # Check if courses are selected
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

      gs_roster <- gs_roster %>% rename_all(tolower)

      if ("sid" %in% colnames(gs_roster)) {
        # Check if SID column is complete (no null/NA/empty values)
        sid_na_count <- sum(is.na(gs_roster$sid) | gs_roster$sid == "" | is.null(gs_roster$sid))
        
        if (sid_na_count == 0) {
          # SID column is complete - use SID for matching
          df_missing <- canvas_roster %>%
            anti_join(gs_roster, by = c("sis_user_id" = "sid"))
          
          matching_method <- "SID (complete)"
          
        } else {
          # SID column has missing values - use email instead
          df_missing <- canvas_roster %>%
            anti_join(gs_roster, by = "email")
          
          matching_method <- paste0("Email (", sid_na_count, " missing SID values detected)")
        }
        
      } else if ("email" %in% colnames(gs_roster)) {
        # No SID column - use email
        df_missing <- canvas_roster %>%
          anti_join(gs_roster, by = "email")
        
        matching_method <- "Email (no SID column found)"
        
      } else {
        # No SID or email columns
        showModal(modalDialog(
          title = "Error: Missing Columns",
          "The Gradescope Roster does not contain a cadet ID (SID) or email column. Please check your roster format.",
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
