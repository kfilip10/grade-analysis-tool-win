


# UI ----

gs_updategrades_ui <- function(){
  sidebarLayout(
    sidebarPanel(
      #tags$br(),
      #uiOutput("tokenStatus"),  # withSpinner(Placeholder for token status
      #withSpinner(uiOutput("connectionStatus")),
      tags$p("If you don't see your section listed, click the 'Refresh Section Data' button:"),
      #actionButton(inputId="refreshSectionData",label="Refresh Section Data"),
      h4("Courses Selected:"),
    ),
    mainPanel(
      h3("1. Select Courses"),
    ),
  )
}

# Server ----


gs_updategrades_server <- function(input, output, session,gs_data,gs_wizard_status){
  
  
}
