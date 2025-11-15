

# UI ----

gs_prep_ui <- function(){
     mainPanel(
       h2("Info"),
       p("This page describes what files you need and how to access them in order
         to generate a grades brief with this app and Gradescope."),
       h4("Files Needed"),
       p("In gradescope go into the 'review grades' tab."),
       img(src=str_c("gs photos/1-review.png"), height = "200px"),
       p("From there you need to download the score data (xlsx) and evaluations data (zip) FOR EACH VERSION."),
       img(src=str_c("gs photos/2-download.png"), height = "200px"),
       tags$ul(
         tags$li("Score data: The output excel with question-wise score data from Gradescope. You will have one copy for each version. Do not change the name of the file."),
         
         tags$li("Cut Data: The zip file containing the cut data for all questions. 
                 This file will have a subfolder for each version of your assignment and each subfolder will have csv's for each question. Do not change the name of the file."),
       ),
       h4("Set up Canvas API Token"),
       p("In the settings of this app check you have a current and valid Canvas API token."),
       h4("Click the button to proceed."),
       actionButton("gs_prep_next",label = HTML("Confirm and Proceed <span class='arrow-icon'>&rarr;</span>"), 
                    class = "custom-button"),
       br(),
       br()
       
       
     )
}


# Server ----

gs_prep_server <- function(input, output, session,gs_data,gs_wizard_status){
   
   observeEvent(input$gs_prep_next, {
     #TODO: Check there is a canvas_api_token before proceeding
     
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
   
}
