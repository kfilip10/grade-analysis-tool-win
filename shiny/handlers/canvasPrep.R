# 
# 
# #### UI ####
# 
# 

createCanvasPrepPage <- function() {

  fluidRow(
    column(width = 10,
           h3("Canvas Data and Grading Template Generator"),
           p("This page allows you to access your Canvas Course Data from the API and generate the grading template"),
           uiOutput("tokenStatus"),  # Placeholder for token status
           withSpinner(uiOutput("connectionStatus")),  # Placeholder for connection status
    )
  )
}

canvasPrep_Handler <- function(input, output, session,canvas_api_token) {

  # Check the API connection status
  connection_status <- reactive({
    if (!is.null(canvas_api_token())) {
      set_canvas_token(canvas_api_token())
      set_canvas_domain(api_domain)
      api_test()
    } else {
      NULL
    }
  })

  # Render the token status UI
  output$tokenStatus <- renderUI({
    if (is.null(canvas_api_token())) {
      tags$p(style = "color: red;", "You do not have a Canvas API token set. Please go to the settings page to set up your Canvas Token to be able to use this page.")
    } else {
      tags$p("Your Canvas API token was found in settings.")
    }
  })
  

  # Render the connection status UI
  output$connectionStatus <- renderUI({
     if(!is.null(connection_status()) && connection_status()!="Success"){
       tagList(
       tags$p(style = "color: white;font-weight: bold;background-color: #781d1d;
                        text-align: center; border-radius: 10px; padding: 5px;",
              "Connection Failed:",connection_status()),
       tags$p("Please check your API token is correct in settings, that your token is not expired (Canvas settings), and that Canvas is operational.")
       )
     }
    else if (!is.null(connection_status()) && connection_status() == "Success") {
             tagList(
             tags$p(style = "color: white;font-weight: bold;background-color: #6fbd7a;
                              text-align: center; border-radius: 10px; padding: 5px;",
                    "API Connection Successful!"),
             h4("1. Select the courses you want to collect data for using the button below:"),
             actionButton("selectCoursesBtn", "Select Courses"),
             h4("2. Select what you want to do from the available options"),
             selectInput("actionSelect", "Choose an Action:",
                         choices = c("Download Course Gradebook", 
                                     "Download Assignment listings", 
                                     "Create WPR Template")),
             HTML("<p></p><br><p></p>"),
             actionButton("actionDo", "Do it!"),
             #disabled(downloadButton("download_template_btn", "Download Grading Template")),
             p("After downloading the template copy the 'v1' sheet for the number of versions you have."),
             #disabled(downloadButton("download_canvas_btn", "Download Combined Canvas Roster"))
             )
           }
  })
  
  
  #reactive value for the course list only if the connection is successful
  course_list <- reactive({
    if (!is.null(connection_status()) && connection_status() == "Success") {
      get_course_list()
    } else {
      NULL
    }
  })
  
  #selected courses
  selected_courses <- reactiveVal()
  
  observeEvent(input$selectCoursesBtn, {
    courses <- get_course_list()
    showModal(modalDialog(
      title = "Select the Courses to collect data for:",
      tags$style("input[type=checkboxGroupInput] {
                    transform: scale(0.2);
           }"),
      checkboxGroupInput("selectedCourses", "Courses:",
                         choices = courses$name,
                         selected = courses$name,
                         inline = FALSE,
                         width='400px'),
      footer = tagList(
        modalButton("Close")
      ),
      size = "l"  # Large modal
    ))
    selected_courses(courses %>% filter(name %in% input$selectedCourses))
  })
  
  #this could then render the filtererd courses as a table
  output$filteredCoursesTable <- renderTable({
    filtered_courses_df()  # Renders the filtered courses in a table
  })
  
  
  observeEvent(input$goButton, {
    # Get the current selection
    selectedAction <- input$actionSelect
    
    # Execute different functions based on the selection
    if (selectedAction == "Download Course Gradebook") {
      function1()  # Replace with your actual function call
    } else if (selectedAction == "Download Assignment listings") {
      function2()  # Replace with your actual function call
    } else if (selectedAction == "Create WPR Template") {
      function3()  # Replace with your actual function call
    }
  })
  
}




