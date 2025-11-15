# UI -------------------------------------------------------

## Drag and Drop JS ----
# Inject JavaScript for drag-and-drop


gs_scores_ui <- function() {
  tagList(
    tags$style(HTML("
  .remove-button {
    background-color: #ff6666;
    border: none;
    color: white;
    font-size: 8px;        /* Even smaller for compact layout */
    font-weight: bold;
    cursor: pointer;
    padding: 1px 4px;      /* Very small padding */
    border-radius: 2px;
    line-height: 1;
  }
  .remove-button:hover {
    background-color: #cc0000;
  }
  
  /* Compact draggable items */
  .draggable {
    transition: background-color 0.2s ease;
  }
  .draggable:hover {
    background-color: #f0f8ff !important;
    border-color: #007bff !important;
  }
  
  /* Scrollbar styling for better UX */
  .compact-question-list::-webkit-scrollbar {
    width: 6px;
  }
  .compact-question-list::-webkit-scrollbar-track {
    background: #f1f1f1;
    border-radius: 3px;
  }
  .compact-question-list::-webkit-scrollbar-thumb {
    background: #888;
    border-radius: 3px;
  }
")),
    tags$div(
      id = "floating_group_panel",
      style = "
    position: fixed;
    top: 100px;
    right: 10px;
    width: 240px;
    background-color: #f3f3f3;
    border: 1px solid #ccc;
    border-radius: 6px;
    z-index: 9999;
    box-shadow: 0 2px 6px rgba(0,0,0,0.2);
  ",
      tags$div(
        id = "floating_group_header",
        style = "display: flex; justify-content: space-between; align-items: center; background: #E3D4BA; padding: 6px; cursor: move;",
        tags$span("Question Groups"),
        tags$button(
          id = "collapse_btn",
          onclick = "
        var content = document.getElementById('floating_group_content');
        content.style.display = (content.style.display === 'none') ? 'block' : 'none';
      ",
          style = "font-size: 16px; padding: 2px 6px;",
          "⯆"
        )
      ),
      p(HTML(
        "<span style='font-size: 11pt;'>
          Drag questions here.<br>
          Group together.<br>
          Drag panel as needed.
        </span>"
      )),
      tags$div(
        id = "floating_group_content",
        style = "padding: 10px; max-height: 70vh; overflow-y: auto;",
        uiOutput("floating_groups_ui")
      )
    ),



    ## Top Section: File Upload and Proceed Button -------------------------
    fluidRow(
      column(
        width = 6,
        h3("1. Upload Version Scores"),
        p("Upload the .xlsx files from Gradescope containing the scores for each version. Don't use the CSV format, it is not supported."),
        p(HTML("<b>This file is labelled 'Assignment_name_scores.xlsx'</b>")),
        fileInput("gs_score_files",
          "Upload all _score.xlsx files at once",
          accept = c(".xlsx"), multiple = TRUE
        )
      ),
      column(
        width = 6,
        h3("3. Proceed to Next Step"),
        p("After grouping your questions click the button to proceed to the next step."),
        actionButton("gs_scores_next",
          label = HTML("Confirm and Proceed <span class='arrow-icon'>&rarr;</span>"),
          class = "custom-button"
        )
      )
    ),

    ## Main Section: Question Groups and Uploaded Files ---------------------
    div(
      style = "margin-top: 0.5in;",
      h3("2. Create Question Groups"),
      
      tabsetPanel(
        tabPanel(
          "Create Question Groups",
          fluidRow(
            column(
              width = 12,
              p("Drag and drop questions to create groups for analysis."),
              p("After adding a group rename it by clicking the group name and typing in the question concept being analyzed."),
            )
          ),
          sidebarLayout(
            sidebarPanel(
              h4("Question Groups"),
              uiOutput("groups_ui"), # ← your group containers go here
              width = 4 # optional: sidebar width (default is 4)
            ),
            mainPanel(
              fluidRow(
                style = "display: flex; align-items: center;",
                column(3, textInput("filter_keyword", "Enter keyword to filter:")),
                column(2, actionButton("filter_button", "Filter Questions")),
                column(3, selectInput("select_group", "Select Group", choices = NULL)),
                column(2, actionButton("add_to_group", "Add Questions to Group"))
              ),
              fluidRow(
                dataTableOutput("filtered_table"),
              ),
              fluidRow(
                actionButton("add_group", "Add Question Group"),
                actionButton("auto_group_v1", "Auto-Group Version 1 Questions", class = "btn-info"),
              ),
              fluidRow(
                h3("Questions from Excel, Grouped by Version"),
                uiOutput("excel_questions_ui")
              ),
            ),
          ),
        ),
        tabPanel(
          "Exam Roster",
          h4("Check Exam Roster"),
          p("Check the uploaded roster for correctness."),
          div(
            id = "gs_examroster", # id is for toggling, but not needed on this tab.
            dataTableOutput("gs_examroster")
          ),
        ),
      ),
      tags$script(HTML("
    function allowDrop(event) {
      event.preventDefault();
      console.log('Allow drop: ', event.target.id); // Log the ID of the target where the item is dragged over
    }

    function drag(event) {
      var draggedId = event.target.id;
      event.dataTransfer.setData('text', draggedId);
      console.log('Drag start: ', draggedId); // Log the ID of the element being dragged
    }

    function drop(event, targetId) {
      event.preventDefault();
      var draggedId = event.dataTransfer.getData('text');
      console.log('Dropped: ', draggedId, ' on: ', targetId); // Log the ID of the dragged element and the target
      var targetDiv = document.getElementById(targetId);
      var draggedElement = document.getElementById(draggedId);
      if (!targetDiv.contains(draggedElement)) {
        targetDiv.appendChild(draggedElement);
        // Send the header's new location to Shiny
        Shiny.setInputValue('dropped_question', {header: draggedId, target: targetId}, {priority: 'event'});
      } else {
        console.log('Element is already in the target container'); // Log if the element is already in the target
      }
    }
  $(document).on('shiny:value', function() {
    console.log('Rebinding event handlers to dynamically created <details> elements.');

    // Unbind existing click events to prevent multiple bindings
    $('details > summary').off('click').on('click', function() {
      var details = $(this).parent(); // Get the parent <details> element
      var id = details.attr('id'); // Get the ID of the details element
      var isOpen = details.prop('open'); // Check if it's open
      console.log('Sending to Shiny: ID:', id, 'Is Open:', isOpen);

      // Collect all current states
      var allDetails = {};
      $('details').each(function() {
        allDetails[$(this).attr('id')] = $(this).prop('open');
      });

      console.log('Sending all details states:', allDetails);

      // Send the entire state of all details to Shiny
      Shiny.setInputValue('details_state_groups', allDetails, {priority: 'event'});
    });
  });

    // Make the floating panel draggable
  dragElement(document.getElementById('floating_group_panel'));

  function dragElement(elmnt) {
    var pos1 = 0, pos2 = 0, pos3 = 0, pos4 = 0;
    var header = document.getElementById('floating_group_header');
    if (header) {
      header.onmousedown = dragMouseDown;
    }

    function dragMouseDown(e) {
      e = e || window.event;
      e.preventDefault();
      // Get the mouse cursor position at startup:
      pos3 = e.clientX;
      pos4 = e.clientY;
      document.onmouseup = closeDragElement;
      document.onmousemove = elementDrag;
    }

    function elementDrag(e) {
      e = e || window.event;
      e.preventDefault();
      // Calculate the new cursor position:
      pos1 = pos3 - e.clientX;
      pos2 = pos4 - e.clientY;
      pos3 = e.clientX;
      pos4 = e.clientY;
      // Set the element's new position:
      elmnt.style.top = (elmnt.offsetTop - pos2) + 'px';
      elmnt.style.left = (elmnt.offsetLeft - pos1) + 'px';
    }

    function closeDragElement() {
      // Stop moving when mouse button is released:
      document.onmouseup = null;
      document.onmousemove = null;
    }
  }

"))
    )
  )
}

# Server ----

gs_scores_server <- function(input, output, session, gs_data, gs_wizard_status) {
  ## Reactives ----
  gs_score_files <- reactiveVal(list())
  question_titles <- reactiveVal(list(pool = list(), groups = list()))

  # Reactive to store dynamic group names
  group_names <- reactiveVal()
  # Reactive list to store question groups
  question_groups <- reactiveVal(list())

  folder_versions <- reactiveValues(
    path = NULL,
    name = NULL,
    versions = NULL
  )
  file_versions <- reactiveValues(
    files = NULL, # Store file names needing version selection
    versions = list() # Store selected versions for each file
  )

  # matches of admin info between files that don't contain a number
  admin_matches <- reactiveVal()

  # roster data from excel
  gs_roster <- reactiveVal()

  # score column from user selection
  score_column <- reactiveVal()
  max_column <- reactiveVal()
  # Top-level observers for column selection (move these OUTSIDE of other observeEvent blocks)
  score_col_observer <- NULL
  max_col_observer <- NULL
  upload_session_id <- reactiveVal(NULL)
  
  
  ## Reset Reactives function ----
  reset_score_data <- function() {
    upload_session_id(paste0("session_", Sys.time(), "_", runif(1)))
    
    # Destroy existing observers
    if (!is.null(score_col_observer)) {
      score_col_observer$destroy()
      score_col_observer <- NULL
    }
    if (!is.null(max_col_observer)) {
      max_col_observer$destroy() 
      max_col_observer <- NULL
    }
    
    # Clear reactive values
    gs_score_files(list())
    question_titles(list(pool = list(), groups = list()))
    question_groups(list())
    gs_roster(NULL)
    score_column(NULL)
    max_column(NULL)
    admin_matches(NULL)
    details_state_groups(list())
    
    # Reset file version tracking
    file_versions$files <- NULL
    file_versions$versions <- list()
    
    # Clear any existing modals
    removeModal()
    
    # Show notification
    showNotification("Previous data cleared. Ready for new upload.", type = "message")
  }
  
  ## Input Excel File ----
  observeEvent(input$gs_score_files, {
    #clear reactives 
    reset_score_data()
    
    req(input$gs_score_files)
    file_paths <- input$gs_score_files$datapath
    file_names <- input$gs_score_files$name

    file_versions$files <- input$gs_score_files$name
    file_versions$versions <- list() # Reset versions

    # open a modal that displays each file name and has a drop down next to it to select the version
    if (length(file_versions$files) > 0) {
      modal_excelversionselector(1)
    }

    # Read each file and extract headers
    files_data <- lapply(seq_along(file_paths), function(i) {
      data <- read_excel(file_paths[i])
      list(name = file_names[i], headers = colnames(data), data = data)
    })
    # browser()
    # Update uploaded files
    gs_score_files(files_data)

    # Initialize header locations
    initial_pool <- setNames(
      lapply(files_data, function(f) f$headers),
      sapply(files_data, function(f) f$name)
    )

    # filter out headers that start with a number
    admin_pool <- lapply(initial_pool, function(x) x[!grepl("^[0-9]", x)])
    # combine admin pool for those that match, this is used later
    admin_matches(reduce(admin_pool, intersect))


    # filter out headers that don't start with a number, gradescope Qs start with a number
    question_pool <- lapply(initial_pool, function(x) x[grepl("^[0-9]", x)])
    df <- question_titles()
    # list of the question titles (from the headers)
    question_titles(list(pool = question_pool, groups = list()))
  })

  ## Excel version selector ----
  # Function to show a modal for selecting a version
  modal_excelversionselector <- function(index) {
    file_name <- file_versions$files[index]
    # read the excel file for htis index
    # browser()
    if (is_empty(file_versions$versions)) {
      versionchoices <- 1:9
    } else {
      versionchoices <- setdiff(1:9, file_versions$versions)
    }
    showModal(modalDialog(
      title = paste("Select Version for File:", file_name),
      selectInput(
        inputId = "selected_version",
        label = "Version:",
        choices = versionchoices,
        selected = NULL
      ),
      footer = tagList(
        actionButton("confirm_excel_version", "Confirm")
      ),
      easyClose = FALSE
    ))
  }


  observeEvent(input$confirm_excel_version, {
    current_index <- length(file_versions$versions) + 1
    file_name <- file_versions$files[current_index]
    df <- read_excel(input$gs_score_files$datapath[current_index])
    df <- df %>% mutate(version = input$selected_version)
    
    matches <- as.character(admin_matches())
    valid_matches <- intersect(matches, colnames(df))
    df2 <- df %>% select(all_of(valid_matches), version)
    
    if (current_index == 1) {
      col_options <- colnames(df2)
      
      # Store df2 so the top-level observers can access it
      session$userData$current_df <- df2
      session$userData$current_index <- current_index
      
      showModal(modalDialog(
        title = "Select the 'Total Score' or 'Score' Column",
        selectInput("score_col", "Select the column header which contains the score of the individual...", choices = col_options),
        actionButton("confirm_score_col", "Confirm Selection"),
        footer = NULL,
        easyClose = FALSE
      ))
      
      # REMOVE the nested observeEvent blocks - they're now at the top level
      
    } else {
      process_with_score_col(df2, current_index)
    }
  })
  
  get_current_df <- function(index) {
    return(session$userData$current_df)
  }
  # TOP-LEVEL observers (not nested inside other observers)
  observeEvent(input$confirm_score_col, {
    # Validate this is the current upload session
    if (is.null(upload_session_id()) || is.null(input$gs_score_files)) {
      removeModal()
      return()
    }
    
    score_column(input$score_col)
    removeModal()
    
    # Get current file being processed
    current_index <- length(file_versions$versions) + 1
    col_options <- colnames(get_current_df(current_index))  # You'll need to make this accessible
    
    showModal(modalDialog(
      title = "Select the 'Max Points' Column",
      selectInput("max_col", "Select the column header which contains the maximum points for the exam.", choices = col_options),
      actionButton("confirm_max_col", "Confirm Selection"),
      footer = NULL,
      easyClose = FALSE
    ))
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$confirm_max_col, {
    # Validate this is the current upload session
    if (is.null(upload_session_id()) || is.null(input$gs_score_files)) {
      removeModal()
      return()
    }
    
    max_column(input$max_col)
    removeModal()
    
    # Get current file being processed and continue
    current_index <- length(file_versions$versions) + 1
    df2 <- get_current_df(current_index)  # You'll need to make this accessible
    process_with_score_col(df2, current_index)
  }, ignoreInit = TRUE)
  
  
  
  process_with_score_col <- function(df, current_index) {
    file_name <- file_versions$files[current_index]
    # remove NA entries in the score column

    df_NA_filtered <- df %>% filter(!is.na(!!sym(score_column())))
    
    # sym(score_col): Converts the column name (as a string) into a symbol that dplyr can interpret as a column reference.
    # !!: Unquotes the symbol, allowing it to be evaluated as a column name within filter().

    # try catch to handle if there are mismatches.
    # A likely error is if the rbind fails because the columns are not the same
    # then I need to prompt the user to check that they uploaded the unaltered and matching files from gradescope
    tryCatch(
      {
        gs_roster(rbind(gs_roster(), df_NA_filtered))
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Error",
          "There was an error processing the files. Please ensure that the files uploaded were the correct score file from Gradescope, you did not edit the file, and that both files came from the same assignment.",
          easyClose = FALSE
        ))
      }
    )

    # browser()
    file_versions$versions[[file_name]] <- input$selected_version
    removeModal()

    # Show the modal for the next file, or finish if all files are processed
    if (current_index < length(file_versions$files)) {
      modal_excelversionselector(current_index + 1)
    } else {
      # All versions selected, do any post-processing if needed
      # print("All versions selected:")
      # print(file_versions$versions)
    }
  }

  ## Excel Question UI ----
  # This is paired with a javascript function in the UI
  details_state <- reactiveVal(list())

  observeEvent(input$details_state, {
    current_state <- details_state()
    current_state[[input$details_state$id]] <- input$details_state$open
    details_state(current_state)
  })

  output$excel_questions_ui <- renderUI({
    req(gs_score_files()) # Ensure files are uploaded
    if (length(gs_score_files()) == 0) {
      return(NULL) # Exit if no files are uploaded
    }

    current_questions <- question_titles()
    open_states <- details_state() # Get the current open states
    num_files <- length(names(current_questions$pool))
    column_width <- max(12 %/% num_files, 1) # Calculate column width (max 12 columns total)

    tags$div(
      fluidRow(
        lapply(names(current_questions$pool), function(file_name) {
          column(
            width = column_width,
            tags$details(
              id = paste0("details_", file_name), # Assign a unique ID to each details element
              open = if (!is.null(open_states[[paste0("details_", file_name)]])) {
                open_states[[paste0("details_", file_name)]]
              } else {
                NA
              }, # Default to closed if state is not tracked
              style = "margin-bottom: 10px; border: 1px solid #ccc; padding: 10px; background: #f9f9f9;",
              tags$summary(
                style = "font-weight: bold; cursor: pointer;",
                str_c(file_name, " : Version ", file_versions$versions[[file_name]])
              ),
              tags$div(
                id = paste0("header_pool_", file_name),
                ondrop = "allowDrop(event)",
                ondragover = "allowDrop(event)",
                style = "min-height: 50px;",
                lapply(current_questions$pool[[file_name]], function(header) {
                  tags$div(
                    id = paste(file_name, header, sep = "::"),
                    class = "draggable",
                    draggable = "true",
                    ondragstart = "drag(event)",
                    style = "padding: 5px; margin: 5px; background: #fff; border: 1px solid #ddd;",
                    header
                  )
                })
              )
            )
          )
        })
      )
    )
  })




  ## Question drops ----
  # Track Question drops
  observeEvent(input$dropped_question, {
    drop_info <- input$dropped_question
    current_questions <- question_titles()

    # Parse header and file name from the dropped ID
    question_info <- strsplit(drop_info$header, "::")[[1]]
    file_name <- question_info[1]
    q_title <- question_info[2]
    target <- drop_info$target

    # Retrieve the version for the file
    # browser()
    version <- file_versions$versions[[file_name]]
    current_questions$versions[[file_name]] <- version
    version_label <- if (!is.null(version)) paste0("v", version, "") else ""

    if (target == "header_pool") {
      # Move back to the pool
      current_questions$pool[[file_name]] <- unique(c(current_questions$pool[[file_name]], q_title))
      for (group in names(current_questions$groups)) {
        current_questions$groups[[group]] <- setdiff(current_questions$groups[[group]], paste(file_name, q_title, version_label, sep = "::"))
      }
    } else {
      # Move to a group
      current_questions$pool[[file_name]] <- setdiff(current_questions$pool[[file_name]], q_title)
      current_questions$groups[[target]] <- unique(c(current_questions$groups[[target]], paste(file_name, q_title, version_label, sep = "::")))
    }

    # Include the selected version for the file

    question_titles(current_questions)
  })


  ## Filter add in groups ----
  # TODO: Add a functionality to add questions to a group by keyword (like units)
  # Observe the button click
  # Observe the button click
  # Observe the button click
  observeEvent(input$filter_button, {
    req(input$filter_keyword) # Ensure keyword is entered

    # Get the current questions and the keyword
    keyword <- input$filter_keyword
    current_questions <- question_titles()

    # Filter questions in the pool based on the keyword
    filtered_questions <- lapply(names(current_questions$pool), function(file_name) {
      questions <- current_questions$pool[[file_name]]
      matched_questions <- questions[grepl(keyword, questions, ignore.case = TRUE)]
      if (length(matched_questions) > 0) {
        version <- file_versions$versions[[file_name]]
        version_label <- if (!is.null(version)) paste0("v", version, "") else ""
        paste(file_name, matched_questions, version_label, sep = "::")
      } else {
        NULL
      }
    })

    # Flatten the filtered results
    filtered_questions <- unlist(filtered_questions, use.names = FALSE)

    # Display filtered questions in a DataTable for user confirmation
    output$filtered_table <- renderDataTable({
      if (length(filtered_questions) == 0) {
        return(data.frame(Questions = character(0))) # Empty table if no match
      }
      # Split into components for display
      datatable(
        data.frame(
          File = sapply(strsplit(filtered_questions, "::"), `[`, 1),
          Question = sapply(strsplit(filtered_questions, "::"), `[`, 2),
          Version = sapply(strsplit(filtered_questions, "::"), `[`, 3)
        ),
        selection = "multiple" # Allow multiple row selection
      )
    })

    # Update the dropdown of groups dynamically
    updateSelectInput(session, "select_group", choices = names(question_groups()))
  })

  # Observe when the user selects a group and confirms adding questions
  observeEvent(input$add_to_group, {
    req(input$select_group) # Ensure a group is selected
    req(input$filtered_table_rows_selected) # Ensure rows are selected

    # Get the selected rows
    selected_rows <- input$filtered_table_rows_selected
    if (length(selected_rows) == 0) {
      showNotification("No questions selected.", type = "warning")
      return()
    }

    # Get the filtered questions and their components
    current_questions <- question_titles()
    filtered_questions <- lapply(names(current_questions$pool), function(file_name) {
      questions <- current_questions$pool[[file_name]]
      matched_questions <- questions[grepl(input$filter_keyword, questions, ignore.case = TRUE)]
      if (length(matched_questions) > 0) {
        version <- file_versions$versions[[file_name]]
        version_label <- if (!is.null(version)) paste0("v", version, "") else ""
        paste(file_name, matched_questions, version_label, sep = "::")
      } else {
        NULL
      }
    })

    # Flatten and get selected rows
    filtered_questions <- unlist(filtered_questions, use.names = FALSE)
    selected_questions <- filtered_questions[selected_rows]

    # Add the selected questions to the chosen group
    selected_group <- input$select_group
    current_questions$groups[[selected_group]] <- unique(c(
      current_questions$groups[[selected_group]],
      selected_questions
    ))

    # Remove the selected questions from the pool
    for (question in selected_questions) {
      parts <- strsplit(question, "::")[[1]]
      file_name <- parts[1]
      q_title <- parts[2]
      current_questions$pool[[file_name]] <- setdiff(current_questions$pool[[file_name]], q_title)
    }

    # Update the reactive value
    question_titles(current_questions)

    # Notify the user
    showNotification("Selected questions added to the group and removed from the pool.")
  })



  ## Question Grouping -------------------------------------------------------
  ##
  output$floating_groups_ui <- renderUI({
    req(question_groups())

    lapply(names(question_groups()), function(group) {
      tags$div(
        id = paste0("floating_", group),
        ondrop = paste0("drop(event, '", group, "')"),
        ondragover = "allowDrop(event)",
        style = "margin: 6px 0; padding: 8px; background: #E3D4BA; border: 3px solid #000000; border-radius: 4px; font-size: 16px; text-align: center; cursor: pointer;",
        group
      )
    })
  })

  observeEvent(input$add_group, {
    # Generate a default unique name
    current_groups <- question_groups()
    base_name <- "Question Group"
    counter <- 1
    
    # Find next available number
    while (paste(base_name, counter) %in% names(current_groups)) {
      counter <- counter + 1
    }
    
    new_group_name <- paste(base_name, counter)
    
    # Add the new group immediately
    current_groups[[new_group_name]] <- character(0)
    question_groups(current_groups)
    
    # Preserve existing states and add the new group (open by default)
    current_states <- details_state_groups()
    current_states[[paste0("details_", new_group_name)]] <- TRUE
    details_state_groups(current_states)
    
    # Optional: Show a brief notification
    showNotification(paste("Added:", new_group_name), type = "message", duration = 2)
  })

  observeEvent(input$confirm_new_group, {
    req(input$new_group_name) # Ensure a name is provided
    # I need to clean this of special characters
    new_group_name <- input$new_group_name
    new_group_name <- gsub("[^[:alnum:]]", " ", new_group_name) # Replace special chars with spaces
    new_group_name <- gsub("\\s+", " ", new_group_name) # Collapse multiple spaces
    new_group_name <- trimws(new_group_name) # Remove leading/trailing spaces

    # Check if empty after cleaning
    if (new_group_name == "") {
      showModal(modalDialog(
        title = "Invalid Group Name",
        "Group name cannot be empty after removing special characters. Please provide a valid name.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }

    # Validate the group name
    current_groups <- question_groups()
    if (new_group_name %in% names(current_groups)) {
      showModal(modalDialog(
        title = "Duplicate Group Name",
        "A group with this name already exists. Please choose a unique name.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }

    # Add the new group
    current_groups[[new_group_name]] <- character(0)
    question_groups(current_groups)

    # Preserve existing states and add the new group
    current_states <- details_state_groups()
    current_states[[paste0("details_", new_group_name)]] <- TRUE # Default to open
    details_state_groups(current_states)

    # Close the modal
    removeModal()
  })


  observe({
    print(details_state_groups())
  })

  details_state_groups <- reactiveVal(list()) # Reactive to store all group states

  observeEvent(input$details_state_groups, {
    current_states <- details_state_groups() # Retrieve current states
    updated_states <- modifyList(current_states, input$details_state_groups)
    details_state_groups(updated_states) # Update reactive value with new states
  })


  ### Question Group UI ----
  output$groups_ui <- renderUI({
    req(question_groups()) # Ensure question groups are available
    current_questions <- question_titles() # Retrieve current questions
    open_states <- details_state_groups() # Retrieve stored states
    num_groups <- length(names(question_groups())) # Number of groups
    column_width <- max(12 %/% num_groups, 3) # Calculate column width (minimum width of 3)
    
    tags$div(
      style = "display: flex; flex-direction: column; gap: 16px;",
      # for each question group
      lapply(names(question_groups()), function(group) {
        tags$div(
          id = paste0("details_", group),
          style = "border: 1px solid #333; padding: 8px; background-color: #ccc4ad; border-radius: 6px;",
          
          # FLEX container for input + delete button
          tags$div(
            style = "display: flex; align-items: center; gap: 6px; margin-bottom: 3px;",
            div(
              style = "flex-grow: 1;",
              textInput(
                inputId = paste0("edit_group_name_", group),
                label = NULL,
                value = group,
                width = "100%"
              )
            ),
            tags$button(
              class = "remove-button",
              onclick = paste0("Shiny.setInputValue('trigger_delete_group', '", group, "', {priority: 'event'});"),
              "X"
            )
          ),
          tags$div(
            id = group,
            ondrop = paste0("drop(event, '", group, "')"),
            ondragover = "allowDrop(event)",
            # UPDATED STYLE - Compact table-like layout
            style = "padding: 8px; margin: 3px 0; background-color: #eef2f7; border: 2px dashed #999; border-radius: 4px; min-height: 50px;",
            # Add placeholder text for empty groups
            if (length(current_questions$groups[[group]]) == 0) {
              tags$div(
                style = "color: #666; font-style: italic; text-align: center; padding: 15px; font-size: 12px;",
                "Drop questions here"
              )
            },
            # COMPACT question list - table-like rows
            tags$div(
              style = "display: flex; flex-direction: column; gap: 2px;",
              lapply(current_questions$groups[[group]], function(header_info) {
                header_parts <- strsplit(header_info, "::")[[1]]
                file_name <- header_parts[1]
                header <- header_parts[2]
                version <- header_parts[3]
                tags$div(
                  id = header_info,
                  class = "draggable",
                  draggable = "true",
                  ondragstart = "drag(event)",
                  # COMPACT STYLING - table row like
                  style = "display: flex; align-items: center; justify-content: space-between; padding: 3px 6px; margin: 1px 0; background: #fff; border: 1px solid #ddd; border-radius: 3px; font-size: 11px; line-height: 1.2; min-height: 24px; cursor: move;",
                  # Question text (left side)
                  tags$div(
                    style = "flex-grow: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; padding-right: 4px;",
                    paste(header, version)
                  ),
                  # Remove button (right side)
                  tags$button(
                    class = "remove-button",
                    onclick = paste0("Shiny.onInputChange('remove_question', {header: '", header_info, "'})"),
                    style = "font-size: 8px; padding: 1px 4px; margin-left: 4px; flex-shrink: 0;",
                    "×"
                  )
                )
              })
            )
          )
        )
      })
    )
  })

  #observe key strokes to update group names
  
  observe({
    req(question_groups()) # Ensure groups exist
    
    # Get all current group names
    current_group_names <- names(question_groups())
    
    # Create observers for each group if they don't already exist
    for (group in current_group_names) {
      local({
        current_group <- group  # Capture the current group name in local scope
        input_id <- paste0("edit_group_name_", current_group)
        
        # Only create observer if it doesn't exist for this group
        if (!exists(paste0("observer_", gsub("[^[:alnum:]]", "_", current_group)), envir = .GlobalEnv)) {
          
          # Debounced reactive for this input
          debounced_input <- debounce(reactive(input[[input_id]]), 3000)  # 3 second delay
          
          # Create observer that watches the debounced input
          assign(paste0("observer_", gsub("[^[:alnum:]]", "_", current_group)), 
                 observeEvent(debounced_input(), {
                   req(debounced_input()) # Ensure the input is not NULL
                   
                   updated_groups <- question_groups()
                   updated_questions <- question_titles()
                   new_name <- debounced_input()
                   
                   # Clean the new name
                   new_name <- gsub("[^[:alnum:]]", " ", new_name)
                   new_name <- gsub("\\s+", " ", new_name)
                   new_name <- trimws(new_name)
                   
                   # Validate the new group name
                   if (new_name != "" && new_name != current_group && !(new_name %in% names(updated_groups))) {
                     # Transfer the questions to the new group name
                     updated_questions$groups[[new_name]] <- updated_questions$groups[[current_group]]
                     updated_questions$groups[[current_group]] <- NULL # Remove the old group name
                     
                     # Update the group names
                     names(updated_groups)[names(updated_groups) == current_group] <- new_name
                     
                     # Update states
                     current_states <- details_state_groups()
                     current_states[[paste0("details_", new_name)]] <- current_states[[paste0("details_", current_group)]]
                     current_states[[paste0("details_", current_group)]] <- NULL
                     details_state_groups(current_states)
                     
                     # Save updates
                     question_groups(updated_groups)
                     question_titles(updated_questions)
                     
                     # Remove the old observer reference
                     if (exists(paste0("observer_", gsub("[^[:alnum:]]", "_", current_group)), envir = .GlobalEnv)) {
                       rm(list = paste0("observer_", gsub("[^[:alnum:]]", "_", current_group)), envir = .GlobalEnv)
                     }
                     
                     # Show success notification
                     showNotification(paste("Group renamed to:", new_name), type = "message", duration = 2)
                     
                   } else if (new_name == "") {
                     showModal(modalDialog(
                       title = "Invalid Group Name",
                       "Group name cannot be empty. Please provide a valid name.",
                       easyClose = TRUE,
                       footer = modalButton("OK")
                     ))
                   } else if (new_name %in% names(updated_groups) && new_name != current_group) {
                     showModal(modalDialog(
                       title = "Duplicate Group Name", 
                       "A group with this name already exists. Please choose a unique name.",
                       easyClose = TRUE,
                       footer = modalButton("OK")
                     ))
                   }
                 }, ignoreInit = TRUE, ignoreNULL = TRUE),
                 envir = .GlobalEnv)
        }
      })
    }
  })
  
  
  ## Remove Question Group ----
  observeEvent(input$trigger_delete_group, {
    group_name <- input$trigger_delete_group
    req(group_name)

    group_questions <- question_titles()$groups[[group_name]]
    num_questions <- if (is.null(group_questions)) 0 else length(group_questions)

    msg <- if (num_questions > 0) {
      paste0("This group contains ", num_questions, " question(s). Deleting it will return them to the pool. Are you sure?")
    } else {
      "Are you sure you want to delete this empty group?"
    }

    showModal(modalDialog(
      title = paste("Delete Group:", group_name),
      p(msg),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_group", "Delete", class = "btn-danger")
      )
    ))

    # Store the group name temporarily
    session$userData$pending_delete_group <- group_name
  })

  ## Confirm Delete Group ----
  observeEvent(input$confirm_delete_group, {
    group_to_delete <- session$userData$pending_delete_group
    req(group_to_delete)

    updated_groups <- question_groups()
    updated_titles <- question_titles()
    updated_states <- details_state_groups()

    # Move questions back to pool
    if (!is.null(updated_titles$groups[[group_to_delete]])) {
      for (q in updated_titles$groups[[group_to_delete]]) {
        parts <- strsplit(q, "::")[[1]]
        file_name <- parts[1]
        q_title <- parts[2]
        updated_titles$pool[[file_name]] <- unique(c(updated_titles$pool[[file_name]], q_title))
      }
    }

    # Remove group
    updated_groups[[group_to_delete]] <- NULL
    updated_titles$groups[[group_to_delete]] <- NULL
    updated_states[[paste0("details_", group_to_delete)]] <- NULL

    question_groups(updated_groups)
    question_titles(updated_titles)
    details_state_groups(updated_states)

    removeModal()
  })


  ## Remove question ----
  observeEvent(input$remove_question, {
    req(input$remove_question)
    header_info <- input$remove_question$header
    # current_questions == current_locations
    # question_titles ==header_locations
    current_questions <- question_titles()

    # Parse header and file name from the clicked ID
    header_parts <- strsplit(header_info, "::")[[1]]
    file_name <- header_parts[1]
    header <- header_parts[2]

    # Move the header back to its original pool
    current_questions$pool[[file_name]] <- unique(c(current_questions$pool[[file_name]], header))
    for (group in names(current_questions$groups)) {
      current_questions$groups[[group]] <- setdiff(current_questions$groups[[group]], header_info)
    }

    question_titles(current_questions)
  })

  ## Roster Table output----
  output$gs_examroster <- DT::renderDataTable({
    # browser()
    req(gs_roster())
    df <- gs_roster()
    df
  })


  observeEvent(input$auto_group_v1, {
    req(gs_score_files()) # Ensure files are uploaded

    current_questions <- question_titles()

    # Find the file with version 1
    v1_file <- NULL
    for (file_name in names(file_versions$versions)) {
      if (file_versions$versions[[file_name]] == "1") {
        v1_file <- file_name
        break
      }
    }

    if (is.null(v1_file)) {
      showModal(modalDialog(
        title = "No Version 1 Found",
        "No file with Version 1 was found. Please ensure you have uploaded and assigned Version 1 to a file.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }

    # Get questions from version 1 file that are still in the pool
    v1_questions <- current_questions$pool[[v1_file]]

    if (length(v1_questions) == 0) {
      showModal(modalDialog(
        title = "No Version 1 Questions Available",
        "All Version 1 questions have already been grouped.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }

    # Get current groups to avoid duplicates
    current_groups <- question_groups()

    # Create groups for each V1 question
    new_groups <- list()
    updated_questions <- current_questions

    for (question in v1_questions) {
      # Create a clean group name from the question
      group_name <- question
      group_name <- gsub("(?<=\\d)\\.0\\b", "", group_name, perl = TRUE) # Remove .0 after digits
      group_name <- gsub("[^[:alnum:] ]+", "", group_name) # Keep alnum + space
      group_name <- gsub("\\s+", " ", group_name) # Collapse whitespace
      group_name <- trimws(group_name) # Remove leading/trailing spaces

      # Ensure unique group name
      original_name <- group_name
      counter <- 1
      while (group_name %in% names(current_groups) || group_name %in% names(new_groups)) {
        group_name <- paste0(original_name, "_", counter)
        counter <- counter + 1
      }

      # Create the question identifier with version
      version_label <- "v1"
      question_id <- paste(v1_file, question, version_label, sep = "::")

      # Add to new groups
      new_groups[[group_name]] <- question_id

      # Remove from pool
      updated_questions$pool[[v1_file]] <- setdiff(updated_questions$pool[[v1_file]], question)

      # Add to groups
      updated_questions$groups[[group_name]] <- question_id
    }

    # Update reactives
    combined_groups <- c(current_groups, new_groups)
    question_groups(combined_groups)
    question_titles(updated_questions)

    # Set all new groups to open by default
    current_states <- details_state_groups()
    for (group_name in names(new_groups)) {
      current_states[[paste0("details_", group_name)]] <- TRUE
    }
    details_state_groups(current_states)

    # Show success message
    showModal(modalDialog(
      title = "Auto-Grouping Complete",
      paste("Successfully created", length(new_groups), "question groups from Version 1 questions."),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })







  ## 'Next' button ----
  observeEvent(input$gs_scores_next, {
    # TODO: Check there are question groups before proceeding

    # req(question_groups())
    # compile_questions()
    showModal(
      modalDialog(
        title = "Confirmation",
        "Are you sure you have all your question grouped?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_gs_scores", "Yes, Proceed")
        )
      )
    )
  })

  observeEvent(input$confirm_gs_scores, {
    files_data <- gs_score_files()
    locations <- question_titles()
    current_groups <- question_groups()
    # browser()


    # locationas$groups has the groups of questions
    # so I need to get the data for each group of questions
    # I could do an lapply
    #
    question_list <- setNames(
      lapply(names(locations$groups), function(group) {
        lapply(locations$groups[[group]], function(question_info) {
          # Print question info for debugging

          # Extract file name and header
          header_parts <- strsplit(question_info, "::")[[1]]
          file_name <- header_parts[1]
          question <- header_parts[2]
          version <- header_parts[3]
          # browser()
          # remove .xlsx from question_info
          question_info <- gsub(".xlsx", "", question_info)
          # print(question_info)

          # Extract data
          df <- lapply(files_data, function(file) {
            if (file$name == file_name) file$data[[question]]
          }) %>%
            Filter(Negate(is.null), .) %>% # Remove NULLs
            `[[`(1) # Extract the first matching data frame

          # Combine data with question_info as label
          if (!is.null(df)) {
            data.frame(
              value = df,
              label = question_info, # Retain the question_info string

              stringsAsFactors = FALSE
            )
          } else {
            NULL # Handle cases where no data is found
          }
        }) %>%
          do.call(rbind, .) # Combine dataframes within each group
      }),
      names(locations$groups)
    ) # Assign names from locations$groups
    # remove NA from question_list

    # remove NAs
    question_list <- lapply(question_list, function(df) na.omit(df))
    question_list <- question_list[!sapply(question_list, is.null)]

    gs_data$gs_question_groups <- question_list
    
    gs_roster_df <- gs_roster()
    
    
    gs_roster_df <- standardize_email_column(gs_roster_df)
    
    #Here I want to first check if the SID column is present in the gs_roster before renaming
    # 14NOV I removed this to track by email since GS DOES require an email.
    # There is likely an edge case where a student has a mismatched email, but very unlikely
    # if(!"SID" %in% colnames(gs_roster_df)) {
    #     cat("No 'SID' column in gs_roster()\n")
    #     gs_data$gs_roster <- gs_roster_df
    # }
    # else{
    #   # remove SID column
    #   gs_data$gs_roster <- gs_roster_df %>% select(-SID)
    # }
    gs_data$gs_roster <- gs_roster_df
    gs_data$versions_selected <- file_versions$versions
    gs_data$score_column <- score_column()
    gs_data$max_column <- max_column()
  })
}
