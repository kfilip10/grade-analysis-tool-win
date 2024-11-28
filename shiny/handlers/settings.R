

#### UI ####

#create a settings page for editing grade bin data
createSettingsPage <- function() {
  div(
    h2("Settings"),
    h3("Canvas API Token"),
    actionButton("editToken", "Edit API Token"),
    h3("Grade Thresholds"),
    #display grade bin data for dynamic editing
    #h4("Edit Grade Thresholds"),
    p("Note: You will need to restart the app for grade changes to take effect."),
    actionButton("editGradeBtn", "Edit Grade Thresholds"),
    actionButton("restoreDefaultGrades", "Restore Default Grade Thresholds"),
  )
  
}

readGrades <- function() {
  if (file.exists(grade_csv_path)) {
    csv <- read.csv(grade_csv_path)
    csv[2] <- csv[2]*100
    return(csv)
  } else {
stop("Cannot find the grades threshold file. Please close and reopen the program and try again. If this error persists please contact the POC.")  # Empty data frame if the file doesn't exist
  }
}

#### Server ####

settings_Handler <- function(input, output, session,canvas_api_token) {
  
  #### Settings #### 
  
  #### Grade Bins ####
  data <- reactiveVal(readGrades())
  
  # Function to read and write CSV file

  

  #### Edit Grades ####
  observeEvent(input$editGradeBtn, {
    showModal(modalDialog(
      title = "Edit Grade Thresholds",
      p("Input the cutoff for achieving that score. For example a D is at a minimum 60% 
    in the course and so it is logged as 60%."),
      DTOutput("settings.grades"),
      footer = tagList(
        actionButton("saveBtnGrade", "Save Changes"),
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
  
  observeEvent(input$saveBtnGrade, {
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
      write.csv(gr.csv,grade_csv_path, row.names = FALSE)
      showModal(modalDialog(
        title = "File Saved",
        "Changes have been saved to the file."
      ))
    }
    
  })
  
  #### Edit API Token ####
  # check if canvas_api_token is null
  token <- reactiveVal(  
    if(is.null(readRDS(canvas_api_token_path))){
    return("")
  }else{
    readRDS(canvas_api_token_path)
   })

  #default reactiveVal is the token loaded by global
  #if(is.null(canvas_api_token)){canvas_api_token <- ""}else{
  
  saveStatus <- reactiveVal("") # Reactive value to store the save status
  
  observeEvent(input$editToken, {
    saveStatus("")
    showModal(modalDialog(
      title = "Edit API Key",
      tagList(
        tags$ol(
          tags$li("Navigate to your profile page on Canvas (left side panel that says 'account'."),
          tags$li("Click on the 'Settings' link in the sidebar that pops up."),
          tags$li("Click on the 'New Access Token' button."),
          tags$li("Enter a purpose for the token (e.g. 'Grade Tool')."),
          tags$li("If you want the token to expire, enter an expiration date. Otherwise, leave it blank."),
          tags$li("Click on the 'Generate Token' button."),
          tags$li("Copy the token and paste it into the box below.")
        ),
        tags$p("Follow ",         
               tags$a(href = "https://community.canvaslms.com/t5/Admin-Guide/How-do-I-manage-API-access-tokens-as-an-admin/ta-p/89", "this link", target = "_blank"),
                "for additional instructions, if needed"),
      tags$p("Input your Canvas API token below:"),
        ),
      textInput(inputId="token", label=HTML("<b>API Token</b>"), value = token(), width = "100%"),
      tags$p(textOutput("saveStatus")), # Display the save status
      footer = tagList(
      actionButton("saveBtnToken", "Save Changes"),
      modalButton("Close")
    )
    ))
  })
  
  #from GPT, edited to fit this app
  observeEvent(input$saveBtnToken, {
    # Attempt to save the file and update the save status
    tryCatch({
      saveRDS(input$token, canvas_api_token_path)
      token(input$token) # Update the token value
      saveStatus("Token successfully saved. You may attempt to connect.") # Update status on success
    }, error = function(e) {
      saveStatus(paste("Error:", e$message)) # Update status on error
    })
    canvas_api_token( readRDS(canvas_api_token_path) )
    
    # Close the modal dialog after a delay to allow user to read the message
    #invalidateLater(2000) # 2 seconds delay
    #removeModal()
  })
  
  output$saveStatus <- renderText({
    saveStatus() # Render the save status text
  })

}
  