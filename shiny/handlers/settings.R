#create a settings page for editing grade bin data
createSettingsPage <- function() {
  div(
    h3("Settings"),
    #display grade bin data for dynamic editing
    #h4("Edit Grade Thresholds"),
    actionButton("editGradeBtn", "Edit Grade Thresholds"),
  )
  
}

readGrades <- function() {
  if (file.exists(grades.path)) {
    csv <- read.csv(grades.path)
    csv[2] <- csv[2]*100
    return(csv)
  } else {
    stop("Cannot find the grades threshold file. Please close and reopen the program and try again.")  # Empty data frame if the file doesn't exist
  }
}

settings_Handler <- function(input, output, session) {
  
  #### Settings #### 
  
  #### Grade Bins ####
  data <- reactiveVal(readGrades())
  
  # Function to read and write CSV file

  

  
  observeEvent(input$editGradeBtn, {
    showModal(modalDialog(
      title = "Edit Grade Thresholds",
      p("Input the cutoff for achieving that score. For example a D is at a minimum 60% 
    in the course and so it is logged as 60%."),
      DTOutput("settings.grades"),
      footer = tagList(
        actionButton("saveBtn", "Save Changes"),
        modalButton("Close")
      )
    ))
  })
  
  output$settings.grades <- renderDT({
    datatable(data(), editable = TRUE,
              options = list(
                  ordering = FALSE  # Disabling sorting
                  )
    )
  })
  
  observeEvent(input$settings.grades_cell_edit, {
    info <- input$settings.grades_cell_edit
    modified_data <- data()
    modified_data[info$row, info$col] <- info$value
    data(modified_data)
  })
  
  observeEvent(input$saveBtn, {
    #find the last row of the data frame and set it to F,0
    gr.csv <- data()
    gr.csv[nrow(gr.csv),] <- c("F",0)
    gr.csv[2] <- as.numeric(gr.csv[[2]])/100
    #gr.csv[2] <- gr.csv[2]/100
    if(max(gr.csv[[2]])>1|min(gr.csv[[2]]<0)){
      showModal(modalDialog(
        title = "Error",
        "Please ensure that all values are between 0 and 100, values will not be saved.",
        easyClose = TRUE
      )
      )
    }
    else{
      write.csv(gr.csv,grades.path, row.names = FALSE)
      showModal(modalDialog(
        title = "File Saved",
        "Changes have been saved to the file."
      ))
    }
  })
 
  
  
  
   
}
  