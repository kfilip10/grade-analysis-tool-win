

# UI ----

gs_cuts_ui <- function(){
  sidebarLayout(
    ## Sidebar -------------------------------------------------------
    
    sidebarPanel(
      h3("Evaluations (Cut Data) Zip File"),
      p("Upload all .zip files from Gradescope containing the cut data."),
      p(HTML("<b>This will be labelled 'Assignment_name_evaluations.zip</b>")),
      fileInput("cutZipFiles", "Upload all zip files at once.", accept = c(".zip"), multiple = TRUE),
    ),
    
    
    ## Main panel -------------------------------------------------------
    
    mainPanel(
      h3("Inspect Cuts and Continue"),
      p("Inspect the cut data below or click the button to continue."),
      actionButton("gs_cuts_next", label = HTML("Confirm and Proceed <span class='arrow-icon'>&rarr;</span>"), 
                   class = "custom-button"),
      
      h4("Loaded Cut data for reference"),
      DTOutput("summaryTable")      #plotOutput("dist_plot")
    )
  )
}

# Server ----


gs_cuts_server <- function(input, output, session,gs_data,gs_wizard_status){
  folder_versions <- reactiveValues(
    path = NULL,
    name = NULL,
    versions = NULL
  )
  cuts_df <- reactiveVal(data.frame())
  
  
  ##Zip-Modal version ----
  # Modal to have user select version associated with uploads
  # Handle ZIP file upload and extract folder names
  observeEvent(input$cutZipFiles, {
    req(input$cutZipFiles)
    # delete previous data
    folder_versions$path <- NULL
    folder_versions$name <- NULL
    folder_versions$versions <- NULL
    cuts_df(data.frame())
    
    tempDir <- tempdir() # Temporary directory for ZIP files
    folder_names <- character() # Store folder names
    folder_paths <- character()
    # Unzip each file and collect folder names
    for (i in seq_along(input$cutZipFiles$datapath)) {
      unzip_dir <- file.path(tempDir, paste0("zip_", i))
      dir.create(unzip_dir)
      unzip(input$cutZipFiles$datapath[i], exdir = unzip_dir)
      folder_names <- c(folder_names, list.dirs(unzip_dir, recursive = FALSE, full.names = FALSE))
      folder_paths <- c(folder_paths, unzip_dir)
    }
    #browser()
    # Store folder names and initialize versions as NA
    folder_versions$folders <- folder_names
    folder_versions$path <- folder_paths
    folder_versions$versions <- rep(NA, length(folder_names))
    
    # Trigger modal for the first folder
    if (length(folder_names) > 0) {
      show_modal_for_folder(1)
    }
    #browser()
  })
  
  # Function to show a modal for version selection
  show_modal_for_folder <- function(folder_index) {
    showModal(modalDialog(
      title = paste("Select Version for Folder:", folder_versions$folders[folder_index]),
      selectInput(
        inputId = "selected_version",
        label = "Version:",
        choices = 1:9
      ),
      footer = tagList(
        actionButton("confirm_cut_version", "Confirm")
      ),
      easyClose = FALSE
    ))
  }
  
  observeEvent(input$confirm_cut_version, {
    # Identify the current folder being processed
    current_index <- which(is.na(folder_versions$versions))[1]
    
    # Save the selected version for the current folder
    folder_versions$versions[current_index] <- input$selected_version
    
    # Get the folder name and version
    folder <- folder_versions$path[current_index]
    version <- input$selected_version
    
    # Remove the modal
    removeModal()
    # Process CSVs for the current folder
    csv_results <- process_all_csvs(folder)
    csv_results$version <- version  # Assign the version to all processed rows
    
    csv_results <- csv_results %>% 
      mutate(number = as.numeric(str_extract(question, "^[0-9]+\\.?[0-9]*")))
    
    #remove the question number from the queston column
    #find the first underscore and delete everyhing before and including it
    csv_results <- csv_results %>% 
      mutate(question = str_remove(question, "^[0-9]+\\.?[0-9]*_"))
    
    #remove all underscores from the question column and replace with ""
    csv_results <- csv_results %>% 
      mutate(question = str_replace_all(question, "_", " "))
    #reorder so it goes version, number then all else
    csv_results <- csv_results %>% 
      select(version, number, question, everything())
    
    # Append the results to the reactive results dataframe
    current_results <- cuts_df()
    updated_results <- bind_rows(current_results, csv_results)
    
    #browser()
    #add logic to clean up the data
    
    cuts_df(updated_results)
    
    
    # Show modal for the next folder or finish processing
    if (any(is.na(folder_versions$versions))) {
      next_index <- which(is.na(folder_versions$versions))[1]
      show_modal_for_folder(next_index)
    }
  })
  
  
  ## DataTable ----
  # Display the summarized data
  output$summaryTable <- DT::renderDT({
    
    req(cuts_df())
    req(nrow(cuts_df()) > 0)
    
    cuts_sorted <- cuts_df()%>%arrange(version,question,desc(cut_percent_true))
    DT::datatable(
      cuts_sorted,
      extensions = 'Buttons',  # Enable the Buttons extension
      options = list(
        dom = 'Bfrtip',        # Define the table control elements
        buttons = list(
          list(
            extend = 'csv',
            text = 'Download CSV (All)',
            exportOptions = list(
              modifier = list(page = 'all') # Export all rows, not just visible
            )
          ),
          list(
            extend = 'excel',
            text = 'Download Excel (All)',
            exportOptions = list(
              modifier = list(page = 'all') # Export all rows, not just visible
            )
          ),
          list(
            extend = 'pdf',
            text = 'Download PDF (All)',
            exportOptions = list(
              modifier = list(page = 'all') # Export all rows, not just visible
            )
          )
        ),
        pageLength = -1,        # Show all rows by default
        # Set initial sorting: version (col 0), then question (col 1)
        order = list(list(0, 'asc'), list(1, 'asc'),list(2,'desc')),
        search = list(
          regex = TRUE,         # Enable regex search
          smart = TRUE          # Enable smart search (ignores accents, etc.)
        ),
        ordering = TRUE         # Enable sorting
      ),
      rownames = FALSE           # Hide row numbers
    )
  })
  
  ## 'Next' Button ----
  observeEvent(input$gs_cuts_next, {
    #req(question_groups())
    #compile_questions()
    showModal(
      modalDialog(
        title = "Confirmation",
        "Are you sure you have all your cuts uploaded?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_gs_cuts", "Yes, Proceed")
        )
      )
    )
  })
  observeEvent(input$confirm_gs_cuts, {
    #TODO: Logic to check data after gs_cuts
    #req(question_groups())
    #compile_questions()
    removeModal()
    
    gs_data$cuts_df <- cuts_df()

    
    #DEBUG data before going into brief
    #
    #saveRDS(reactiveValuesToList(gs_data), "test/gs_data_tolist.rds")
    #
    #df <- gs_data$canvas_roster
    #df_missing <- gs_data$missing_roster
    #gs_roster <- gs_data$gs_roster
    #cuts_df <- gs_data$cuts_df
    #df_question_groups <- gs_data$df_question_groups
    #saveRDS(reactiveValuesToList(gs_data), "test/gs_data_complete.rds")
    
  })
  
  #### CSV process Functions ----
  # Function to process individual CSV files
  process_csv <- function(file_path, folder_name) {
    csv_data <- read_csv(file_path, col_types = cols(.default = "c"))
    #browser()
    #find 'point values' row in first column
    point_values_row <- csv_data[csv_data[[1]] == "Point Values", ]
    point_values_row_number <- which(csv_data[[1]] == "Point Values")
    cut_values <- as.numeric(point_values_row[1, -1])
    
    #first one is point value for question
    cut_values <- cut_values[!is.na(cut_values)]
    point_value <- cut_values[1]
    cut_values <- cut_values[-1]
    
    
    # Identify columns corresponding to cuts
    start_col <- which(names(csv_data) == "Submission Time")
    end_col <- which(names(csv_data) == "Adjustment") - 1
    cut_columns <- names(csv_data)[(start_col + 1):end_col]
    
    csv_data <- csv_data[1:point_values_row_number-1,]
    
    # Calculate the percent TRUE for each cut
    cut_summary <- sapply(cut_columns, function(cut_col) {
      cut_data <- csv_data[[cut_col]]
      sum(cut_data == "true", na.rm = TRUE) / (length(cut_data) - 1)
    })
    
    question_name <- basename(file_path)
    question_name <- substr(question_name, 1, nchar(question_name) - 4) # Remove .csv extension
    
    # Build a dataframe with the results
    result <- data.frame(
      version = folder_name,  # Placeholder, will be updated with selected version
      question = question_name,
      points = cut_values,
      cut_label = cut_columns,
      cut_percent_true = cut_summary,
      stringsAsFactors = FALSE
    )
    rownames(result) <- NULL
    return(result)
  }
  
  # Function to process all CSV files in a directory
  process_all_csvs <- function(base_dir) {
    all_files <- list.files(base_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
    results <- do.call(rbind, lapply(all_files, function(file) {
      folder_name <- dirname(file)
      process_csv(file, folder_name)
    }))
    return(results)
  }
  
  
}