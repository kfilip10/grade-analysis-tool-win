# UI ----

gs_canvas_ui <- function() {
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::tags$p("If you don't see your section listed, click the 'Refresh Section Data' button:"),
      # actionButton(inputId="refreshSectionData",label="Refresh Section Data"),
      shiny::h4("Courses Selected:"),
    ),
    shiny::mainPanel(
      actionButton("gs_canvas_next", "Confirm Canvas Data and Proceed")
      
    ),
  )
}

# Server ----

gs_canvas_server <- function(input, output, session, gs_data,gs_wizard_status) {

  observeEvent(input$gs_canvas_next, {
    showModal(
      modalDialog(
        title = "Confirmation",
        "Are you sure you have all your files?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_gs_canvas", "Yes, Proceed")
        )
      )
    )
  })
}
