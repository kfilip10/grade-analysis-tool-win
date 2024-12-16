
# UI -------------------------------------------------------
tags$style(HTML("
  .remove-button {
    background-color: #ff6666;
    border: none;
    color: white;
    font-weight: bold;
    cursor: pointer;
    padding: 2px 8px;
    border-radius: 4px;
  }
  .remove-button:hover {
    background-color: #cc0000;
  }
"))
## Drag and Drop JS ----
# Inject JavaScript for drag-and-drop


gs_scores_ui <- function(){
  tagList(
    ## Top Section: File Upload and Proceed Button -------------------------
    fluidRow(
      column(
        width = 6,
        h3("1. Upload Version Scores"),
        p("Upload the .xlsx files from Gradescope containing the scores for each version. Don't use the CSV format, it is not supported."),
        p(HTML("<b>This file is labelled 'Assignment_name_scores.xlsx'</b>")),
        fileInput("gs_score_files", 
                  "Upload all _score.xlsx files at once", accept = c(".xlsx"), multiple = TRUE)
      ),
      column(
        width = 6,
        h3("3. Proceed to Next Step"),
        p("After grouping your questions click the button to proceed to the next step."),
        actionButton("gs_scores_next", label = HTML("Confirm and Proceed <span class='arrow-icon'>&rarr;</span>"), 
                     class = "custom-button")
      )
    ),
    
    ## Main Section: Question Groups and Uploaded Files ---------------------
    div(style = "margin-top: 0.5in;",
        
    tabsetPanel(
      tabPanel("Create Question Groups", 
               
        fluidRow(
          column(
            width=12,
            h4("Create Question Groups"),
            p("Drag and drop questions to create groups for analysis."),
            p("After adding a group rename it by clicking the group name and typing in the question concept being analyzed."),
          )
          ),
        fluidRow(
          actionButton("add_group", "Add Question Group"),
          br()
        ),
          fluidRow(        
            uiOutput("groups_ui")
    ),
        fluidRow(
            h3("Questions from Excel, Grouped by Version"),
            uiOutput("excel_questions_ui")
        ),
      ),
    tabPanel("Exam Roster",
             h4("Check Exam Roster"),
             p("Check the uploaded roster for correctness."),
            
            div(id="gs_examroster", #id is for toggling, but not needed on this tab.
                dataTableOutput("gs_examroster")
            ),
    
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

")
))
  
}

# Server ----

gs_scores_server <- function(input, output, session,gs_data,gs_wizard_status){
  
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
    files = NULL,      # Store file names needing version selection
    versions = list()  # Store selected versions for each file
  )
  
  #matches of admin info between files that don't contain a number
  admin_matches <- reactiveVal()
  
  #roster data from excel
  gs_roster <- reactiveVal()
  
  #score column from user selection
  score_column <- reactiveVal()
  max_column <- reactiveVal()
  
  ## Input Excel File ----
  observeEvent(input$gs_score_files, {
    # browser()
    req(input$gs_score_files)
    file_paths <- input$gs_score_files$datapath
    file_names <- input$gs_score_files$name
    
    file_versions$files <- input$gs_score_files$name
    file_versions$versions <- list()  # Reset versions
    
    #open a modal that displays each file name and has a drop down next to it to select the version
    if (length(file_versions$files) > 0) {
      modal_excelversionselector(1)
    }
    
    # Read each file and extract headers
    files_data <- lapply(seq_along(file_paths), function(i) {
      data <- read_excel(file_paths[i])
      list(name = file_names[i], headers = colnames(data), data = data)
    })
    #browser()
    # Update uploaded files
    gs_score_files(files_data)
    
    # Initialize header locations
    initial_pool <- setNames(lapply(files_data, function(f) f$headers), 
                             sapply(files_data, function(f) f$name))
    
    #filter out headers that start with a number
    admin_pool <- lapply(initial_pool, function(x) x[!grepl("^[0-9]", x)])
    #combine admin pool for those that match, this is used later
    admin_matches(reduce(admin_pool,intersect))
    
    
    #filter out headers that don't start with a number, gradescope Qs start with a number
    question_pool <- lapply(initial_pool, function(x) x[grepl("^[0-9]", x)])
    df <- question_titles()
    #list of the question titles (from the headers)
    question_titles(list(pool = question_pool, groups = list()))
  })
  
  ## Excel version selector ----
  # Function to show a modal for selecting a version
  modal_excelversionselector <- function(index) {
    
    file_name <- file_versions$files[index]
    #read the excel file for htis index
    #browser()
    if(is_empty(file_versions$versions)){
      versionchoices <- 1:9
    }
    else{
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
    # Get the current file index needing version selection
    current_index <- length(file_versions$versions) + 1
    
    file_name <- file_versions$files[current_index]
    
    df <- read_excel(input$gs_score_files$datapath[current_index])
    
    df <- df %>% mutate(version=input$selected_version)
    #browser()
    #I want to select the columnnames that match the admin_matches
    matches <- as.character(admin_matches())
    
    valid_matches <- intersect(matches, colnames(df))
    
    df2 <- df %>% select(all_of(valid_matches),version)
    
    #if the score column is not selected, then have the user select it
    
    if(current_index == 1){
      ### Identify Score Column ----
      
      col_options <- colnames(df2)
      
      showModal(modalDialog(
        title = "Select the 'Total Score' or 'Score' Column",
        selectInput("score_col", "Select the column header which contains the score of the individual. This is likely called 'Total Score'. DO NOT SELECT the MAX points column.", choices = col_options),
        actionButton("confirm_score_col", "Confirm Selection"),
        footer = NULL,
        easyClose = FALSE
      ))
      
      observeEvent(input$confirm_score_col, { 
        score_column(input$score_col)
        removeModal()
        
        ### Identify Max Column ----
        
        showModal(modalDialog(
          title = "Select the 'Max Points'  Column",
          selectInput("max_col", "Select the column header which contains the maximum points for the exam.", choices = col_options),
          actionButton("confirm_max_col", "Confirm Selection"),
          footer = NULL,
          easyClose = FALSE
        ))
        
        observeEvent(input$confirm_max_col, { 
          max_column(input$max_col)
          removeModal()
          process_with_score_col(df2,current_index)
        })
      })
    }
    else{
      process_with_score_col(df2,current_index)
    }

    
  })

  process_with_score_col <- function(df,current_index){
  
    file_name <- file_versions$files[current_index]
    #remove NA entries in the score column

    df_NA_filtered <- df %>% filter(!is.na(!!sym(score_column())))
    
    #sym(score_col): Converts the column name (as a string) into a symbol that dplyr can interpret as a column reference.
    #!!: Unquotes the symbol, allowing it to be evaluated as a column name within filter().
    
    #try catch to handle if there are mismatches. 
    # A likely error is if the rbind fails because the columns are not the same
    # then I need to prompt the user to check that they uploaded the unaltered and matching files from gradescope
    tryCatch({
      gs_roster(rbind(gs_roster(),df_NA_filtered))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        "There was an error processing the files. Please ensure that the files uploaded were the correct score file from Gradescope, you did not edit the file, and that both files came from the same assignment.",
        easyClose = FALSE
      ))
    })

    #browser()
    file_versions$versions[[file_name]] <- input$selected_version
    removeModal()
    
    # Show the modal for the next file, or finish if all files are processed
    if (current_index < length(file_versions$files)) {
      modal_excelversionselector(current_index + 1)
    } else {
      # All versions selected, do any post-processing if needed
      #print("All versions selected:")
      #print(file_versions$versions)
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
    req(gs_score_files())  # Ensure files are uploaded
    if (length(gs_score_files()) == 0) {
      return(NULL)  # Exit if no files are uploaded
    }
    
    current_questions <- question_titles()
    open_states <- details_state()  # Get the current open states
    num_files <- length(names(current_questions$pool))
    column_width <- max(12 %/% num_files, 1)  # Calculate column width (max 12 columns total)
    
    tags$div(
      fluidRow(
        lapply(names(current_questions$pool), function(file_name) {
          column(
            width = column_width,
            tags$details(
              id = paste0("details_", file_name),  # Assign a unique ID to each details element
              open = if (!is.null(open_states[[paste0("details_", file_name)]])) 
                open_states[[paste0("details_", file_name)]]
              else 
                NA,  # Default to closed if state is not tracked
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
    #browser()
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
  
  
  
  
  
  
  ## Question Grouping -------------------------------------------------------
  ## 
  observeEvent(input$add_group, {
    # Show a modal to name the new group
    showModal(modalDialog(
      title = "Name the Question Grouping",
      textInput(
        inputId = "new_group_name",
        label = "Enter a unique name for the question grouping, this can be changed later.",
        value = ""
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_new_group", "Add Group")
      )
    ))
  })
  
  observeEvent(input$confirm_new_group, {
    req(input$new_group_name) # Ensure a name is provided
    new_group_name <- input$new_group_name
    
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
    current_states <- details_state_groups()  # Retrieve current states
    updated_states <- modifyList(current_states, input$details_state_groups)
    details_state_groups(updated_states)      # Update reactive value with new states
    
  })
  
  
  ### Question Group UI ----
  output$groups_ui <- renderUI({
    req(question_groups()) # Ensure question groups are available
    current_questions <- question_titles() # Retrieve current questions
    open_states <- details_state_groups()  # Retrieve stored states
    num_groups <- length(names(question_groups())) # Number of groups
    column_width <- max(12 %/% num_groups, 3) # Calculate column width (minimum width of 3)
    
    tags$div(
      fluidRow(
        lapply(names(question_groups()), function(group) {
          column(
            width = column_width,
            tags$details(
              id = paste0("details_", group),
              open = if (!is.null(open_states[[paste0("details_", group)]])) 
                open_states[[paste0("details_", group)]]
              else 
                TRUE, # Default to open if no state is tracked
              style = "margin-bottom: 10px; border: 1px solid #333; padding: 10px; background: #f9f9f9;",
              tags$summary(
                style = "font-weight: bold; cursor: pointer;",
                textInput(
                  inputId = paste0("edit_group_name_", group),
                  label = NULL,
                  value = group,
                  width = "100%"
                )
              ),
              # Droppable area for the group
              tags$div(
                id = group,
                ondrop = paste0("drop(event, '", group, "')"),  # Drop handler
                ondragover = "allowDrop(event)",                # Allow drag over
                style = "border: 1px dashed #333; padding: 10px; min-height: 50px;",
                # List of questions in the group
                lapply(current_questions$groups[[group]], function(header_info) {
                  header_parts <- strsplit(header_info, "::")[[1]]
                  file_name <- header_parts[1]
                  header <- header_parts[2]
                  version <- header_parts[3]
                  tags$div(
                    id = header_info,
                    class = "draggable",
                    draggable = "true",
                    ondragstart = "drag(event)", # Drag handler
                    style = "padding: 5px; margin: 5px; background: #fff; border: 1px solid #ddd; display: flex; align-items: center; justify-content: space-between;",
                    tags$span(paste(header, version)),
                    tags$button(
                      class = "remove-button",
                      onclick = paste0("Shiny.onInputChange('remove_question', {header: '", header_info, "'})"),
                      "X"
                    )
                  )
                })
              )
            )
          )
        })
      )
    )
  })
  
  
  # output$groups_ui <- renderUI({
  #   req(question_groups()) # Ensure question groups are available
  #   current_questions <- question_titles() # Retrieve current questions
  #   
  #   lapply(names(question_groups()), function(group) {
  #     tags$div(
  #       id = group,
  #       ondrop = paste0("drop(event, '", group, "')"),
  #       ondragover = "allowDrop(event)",
  #       style = "border: 1px solid #333; padding: 10px; margin: 10px; min-height: 50px;",
  #       # Editable group name
  #       textInput(
  #         inputId = paste0("edit_group_name_", group),
  #         label = NULL,
  #         value = group,
  #         width = "100%"
  #       ),
  #       # List of questions in the group with versions
  #       lapply(current_questions$groups[[group]], function(header_info) {
  #         header_parts <- strsplit(header_info, "::")[[1]]
  #         file_name <- header_parts[1]
  #         header <- header_parts[2]
  #         version <- file_versions$versions[[file_name]]
  #         version_label <- if (!is.null(version)) paste0("v", version) else ""
  #         tags$div(
  #           id = header_info,
  #           class = "draggable",
  #           draggable = "true",
  #           ondragstart = "drag(event)",
  #           style = "padding: 5px; margin: 5px; background: #f9f9f9; border: 1px solid #ddd; display: flex; align-items: center; justify-content: space-between;",
  #           tags$span(paste(header, version_label)),
  #           tags$button(
  #             class = "remove-button",
  #             onclick = paste0("Shiny.onInputChange('remove_question', {header: '", header_info, "'})"),
  #             "X"
  #           )
  #         )
  #       })
  #     )
  #   })
  # })
  
  observe({
    req(question_groups()) # Ensure groups exist
    current_groups <- question_groups()
    current_questions <- question_titles()
    
    for (group in names(current_groups)) {
      input_id <- paste0("edit_group_name_", group)
      
      observeEvent(input[[input_id]], {
        req(input[[input_id]]) # Ensure the input is not NULL
        
        updated_groups <- question_groups()
        updated_questions <- question_titles()
        new_name <- input[[input_id]]
        
        # Validate the new group name
        if (new_name != "" && !(new_name %in% names(updated_groups))) {
          # Transfer the questions to the new group name
          updated_questions$groups[[new_name]] <- updated_questions$groups[[group]]
          updated_questions$groups[[group]] <- NULL # Remove the old group name
          
          # Update the group names
          names(updated_groups)[names(updated_groups) == group] <- new_name
          
          # Save updates
          question_groups(updated_groups)
          question_titles(updated_questions)
        } else if (new_name == "") {
          showModal(modalDialog(
            title = "Invalid Group Name",
            "Group name cannot be empty. Please provide a valid name.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
        } 
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    }
  })
  
  
  
  ## Remove question ----
  observeEvent(input$remove_question, {
    req(input$remove_question)
    header_info <- input$remove_question$header
    #current_questions == current_locations
    #question_titles ==header_locations
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
    #browser()
    req(gs_roster())
    df <- gs_roster()
    df
    
  })
  
  
  
  
  
  
  
  
  
  
  ## 'Next' button ----
  observeEvent(input$gs_scores_next, {
    #TODO: Check there are question groups before proceeding
    
    #req(question_groups())
    #compile_questions()
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
    #browser()
    
    
    #locationas$groups has the groups of questions
    #so I need to get the data for each group of questions
    # I could do an lapply
    # 
    question_list <- setNames(lapply(names(locations$groups), function(group) {
      lapply(locations$groups[[group]], function(question_info) {
        # Print question info for debugging
        
        # Extract file name and header
        header_parts <- strsplit(question_info, "::")[[1]]
        file_name <- header_parts[1]
        question <- header_parts[2]
        version <- header_parts[3]
        #browser()
        #remove .xlsx from question_info
        question_info <- gsub(".xlsx", "", question_info)
        #print(question_info)
        
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
    #remove NA from question_list
    
    #remove NAs
    question_list <- lapply(question_list, function(df) na.omit(df))
    question_list <- question_list[!sapply(question_list, is.null)]
    
    gs_data$gs_question_groups <- question_list
    
    gs_data$gs_roster <- gs_roster()%>% rename(sis_user_id = SID)
    gs_data$versions_selected <- file_versions$versions
    gs_data$score_column <- score_column()
    gs_data$max_column <- max_column()
  })
}


