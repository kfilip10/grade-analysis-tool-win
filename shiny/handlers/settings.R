

#### UI ####

#create a settings page for editing grade bin data
createSettingsPage <- function() {
  div(
    h3("Canvas API Token"),
    actionButton("editToken", "Edit API Token"),
    br(),br(),
    #display grade bin data for dynamic editing
    h3("Edit Grade Thresholds"),
    p("Note: You will need to restart the app for grade changes to take effect."),
    actionButton("editGradeBtn", "Edit Grade Thresholds"),
    actionButton("restoreDefaultGrades", "Restore Default Grade Thresholds"),
    # Add restart status display
    uiOutput("restartStatus"),
    br(),
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
  # Track if restart is needed
  restart_needed <- reactiveVal(FALSE)
  
  
  
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
      restart_needed(TRUE) # Mark that restart is needed
      
      
      showModal(modalDialog(
        title = "Grade Thresholds Saved",
        HTML(paste0(
          "<p><strong>Changes have been saved successfully!</strong></p>",
          "<p>The application needs to be restarted for the grade threshold changes to take effect.</p>"
        )),
        footer = tagList(
          actionButton("restartAppGrades", "Restart Application", 
                       style = "background-color: #dc3545; color: white;"),
          actionButton("continueWithoutRestart", "Continue Without Restart", 
                       style = "background-color: #6c757d; color: white;")
        ),
        easyClose = FALSE
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
  
  observeEvent(input$saveBtnToken, {
    # Attempt to save the file and update the save status
    tryCatch({
      old_token <- token()
      new_token <- input$token
      
      saveRDS(new_token, canvas_api_token_path)
      token(new_token) # Update the token value
      canvas_api_token(readRDS(canvas_api_token_path))
      
      # Check if token actually changed
      if (old_token != new_token) {
        restart_needed(TRUE) # Mark that restart is needed
        
        showModal(modalDialog(
          title = "API Token Saved",
          HTML(paste0(
            "<p><strong>API Token successfully saved!</strong></p>",
            "<p>The application should be restarted to ensure the new API token is properly loaded.</p>"
          )),
          footer = tagList(
            actionButton("restartAppToken", "Restart Application", 
                         style = "background-color: #dc3545; color: white;"),
            actionButton("continueWithoutRestart", "Continue Without Restart", 
                         style = "background-color: #6c757d; color: white;")
          ),
          easyClose = FALSE
        ))
      } else {
        saveStatus("Token saved (no changes detected).")
      }
      
    }, error = function(e) {
      saveStatus(paste("Error:", e$message)) # Update status on error
    })
  })
  
  output$saveStatus <- renderText({
    saveStatus() # Render the save status text
  })
  
  
  
  #### Added 14OCT for restarting####
  observeEvent(input$continueWithoutRestart, {
    removeModal()
  })
  
  # Restart handlers
  observeEvent(input$restartAppGrades, {
    if (exists("restartApp") && is.function(restartApp)) {
      restartApp()
    } else {
      # Fallback using JavaScript if electron API is available
      session$sendCustomMessage("restartApp", list())
    }
  })
  
  observeEvent(input$restartAppToken, {
    if (exists("restartApp") && is.function(restartApp)) {
      restartApp()
    } else {
      # Fallback using JavaScript if electron API is available
      session$sendCustomMessage("restartApp", list())
    }
  })
  
  # Add manual restart button to main settings UI
  output$restartStatus <- renderUI({
    if (restart_needed()) {
      div(
        style = "background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 10px; margin: 10px 0; border-radius: 5px;",
        HTML("<strong>⚠ Restart Recommended</strong><br>"),
        p("You have made changes that require an application restart to take full effect."),
        actionButton("manualRestart", "Restart Application Now", 
                     style = "background-color: #dc3545; color: white; margin-top: 5px;")
      )
    }
  })
  
  observeEvent(input$manualRestart, {
    if (exists("restartApp") && is.function(restartApp)) {
      restartApp()
    } else {
      session$sendCustomMessage("restartApp", list())
    }
  })
  

}
  