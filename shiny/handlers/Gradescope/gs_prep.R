

# UI ----

gs_prep_ui <- function(){
     mainPanel(
       h2("Info"),
       p("This page describes what files you need and how to access them in order
         to generate a grades brief with this app and Gradescope."),
       h4("Files Needed"),
       p("You will need the following files from Gradescope to generate a grades brief:"),
       tags$ul(
         tags$li("Score data: The output excel with question-wise score data from Gradescope. You will have one copy for each version. Do not change the name of the file."),
         tags$li("Cut Data: The zip file containing the cut data for all questions. 
                 This file will have a subfolder for each version of your assignment and each subfolder will have csv's for each question. Do not change the name of the file."),
       ),
       h4("Set up Canvas API Token"),
       p("In the settings of this app check you have a current and valid Canvas API token."),
       h4("Click the button to proceed."),
       actionButton("gs_prep_next", "Confirm and Proceed"),
       
       
       #### DEBUG STEP
       #h2("Debug shortcut:"),
       #p("This button will load pre-canned data for testing purposes."),
       #actionButton("gs_brief_shortcut", "Brief Shortcut")
       
     )
}


# Server ----

gs_prep_server <- function(input, output, session,gs_data,gs_wizard_status){
   
   observeEvent(input$gs_prep_next, {
      showModal(
         modalDialog(
            title = "Confirmation",
            "Are you sure you have all your files?",
            footer = tagList(
               modalButton("Cancel"),
               actionButton("confirm_gs_prep", "Yes, Proceed")
            )
         )
      )
   })
   
   observeEvent(input$gs_brief_shortcut, {
      #browser()
      gs_data = readRDS("gs_data.rds")
      
      
      gs_wizard_status$gs_prep <- TRUE
      gs_wizard_status$gs_canvas <- TRUE
      gs_wizard_status$gs_cuts <- TRUE
      gs_wizard_status$gs_brief <- TRUE
   })
}
