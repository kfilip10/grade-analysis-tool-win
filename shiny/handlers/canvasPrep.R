# 
# 
# #### UI for Canvas Page####
# 
# The first functions below are called as tabset panels in app.R
#this is the first tabset, it runs on loading the tab.
createCanvasPrepPage <- function() {
  tabPanel("Canvas Data",
           fluidRow(
             column(width = 10,
                    h3("Canvas Data and Grading Template Generator"),
                    p("This page allows you to access your Canvas Course Data from the API and generate the grading template"),
                    uiOutput("tokenStatus"),  # Placeholder for token status
                    withSpinner(uiOutput("connectionStatus")),  # Placeholder for connection status
             )
           ))
}
course_graph_viewer <- function() {
  fluidPage(
    titlePanel("Gradebook Plot with Filtering"),
    sidebarLayout(
      sidebarPanel(
        selectInput("group_id", "Assignment Group ID",
                    choices = NULL)
      ),
      mainPanel(
        plotOutput("plot")
      )
    )
  )
}

course_table_viewer<- function() {
  
  fluidRow(
    column(width = 10,
           h3("This will be a tabular viewer of assignment level info"),
           DTOutput("course_table")
    )
  )
}

canvasPrep_Handler <- function(input, output, session,canvas_api_token) {
  
  #### API Load, check, and display UI ####
  # Check the API connection status and logs in
  connection_status <- reactive({
    if (!is.null(canvas_api_token())) {
      set_canvas_token(canvas_api_token())
      set_canvas_domain(api_domain)
      api_test()
    } else {
      NULL
    }
  })
  
  # Render the token status on the UI
  output$tokenStatus <- renderUI({
    if (is.null(canvas_api_token())) {
      tags$p(style = "color: red;", "You do not have a Canvas API token set. Please go to the settings page to set up your Canvas Token to be able to use this page.")
    } else {
      #I could put text here if it was needed but I don't think it is
      #tags$p("Your Canvas API token was found in settings.")
    }
  })
  
  
  # If the connection is successful, render the UI for the page
  #This is the main UI for API UI
  output$connectionStatus <- renderUI({
    #If it doesn't work then show an error message
    if(!is.null(connection_status()) && connection_status()!="Success"){
      tagList(
        tags$p(style = "color: white;font-weight: bold;background-color: #781d1d;
                        text-align: center; border-radius: 10px; padding: 5px;",
               "Connection Failed:",connection_status()),
        tags$p("Please check your API token is correct in settings, that your token is not expired (Canvas settings), and that Canvas is operational.")
      )
    }
    #If it does work then show the main UI for Canvas API access
    else if (!is.null(connection_status()) && connection_status() == "Success") {
      #make the list on the page       
      tagList(
        #show the success message
        tags$p(style = "color: white;font-weight: bold;background-color: #6fbd7a;
                              text-align: center; border-radius: 10px; padding: 5px;",
               "API Connection Successful!"),
        #show the course selection button
        h4("1. Select the Courses to collect data:"),
        #show the button to show/hide the course selection
        actionButton(inputId = "courseShowHide",label = "Show/Hide Course Selection"),
        div(id="course_checkbox_div", style = "display: none;",
            uiOutput("courseCheckboxUI")),
        h4("2. Select the Assignment data you want:"),
        tags$p("Generating the assignment listing will take approximately 2 seconds per course selected."),
        disabled(actionButton(inputId = "assignmentShowHide",label = "Show/Hide Gradebook Download")),
        div(id="assignment_div", style = "display: none;",
            uiOutput("assignmentUI")),
        h4("3. Load the assignment data for the selected courses:"),
        disabled(actionButton(inputId = "load_canvas_data",label = "Automatically Load Data from Canvas")),
        h4("4. Select what you want to do from the available options or use the graph tabs on this page."),
        disabled(downloadButton("download_template_canvas", "Download WPR Grading Template")),
        disabled(downloadButton("download_gradebook_canvas", "Download Canvas Gradebook"))
      )
    }
  })
  
  
  #### Course Selection ####
  #hide(id = "div_b")
  #This is to show/hide the courses
  observeEvent(input$courseShowHide,{
    toggle(id = "course_checkbox_div",anim = T)
  })
  
  #reactive value for the course list only if the connection is successful
  course_list_df <- reactive({
    if (!is.null(connection_status()) && connection_status() == "Success") {
      get_course_list()
    } else {
      NULL
    }
  })
  
  #assignment group lookup table
  assign.group.comb <- reactive({
    req(course_list_df())
    if (!is.null(connection_status()) && connection_status() == "Success") {
      assign.group.comb <- course_list_df() %>% group_by(id) %>% 
        group_split %>% purrr::map_df(~ get_assignment_groups(.x, unique(.x$id)))
      assign.group.comb <-  assign.group.comb %>% rename(assignment_group_id = id)
      assign.group.comb <-  assign.group.comb %>% rename(assignment_group_name = name)
      return(assign.group.comb)
    } else {
      NULL
    }
  })
  
  
  #render the course list as a checkbox
  output$courseCheckboxUI <- renderUI({
    tagList(
    checkboxGroupInput("courseCheckbox", "Select only the courses that will share the same assignment names:", 
                       choices = course_list_df()$name,
                       selected=NULL,
                       inline = FALSE,
                       width='400px'),
    #add a select all button,
    actionButton("courseCheckboxselectAll", "Select All"),
    actionButton("courseCheckboxdeselectAll", "Deselect All")
    )
    })
  
  # JavaScript to select all checkboxes
  observeEvent(input$courseCheckboxselectAll, {
    runjs('$("#courseCheckbox input[type=checkbox]").prop("checked", true);')
  })
  
  # JavaScript to deselect all checkboxes
  observeEvent(input$courseCheckboxdeselectAll, {
    runjs('$("#courseCheckbox input[type=checkbox]").prop("checked", false);')
  })
  
  
  #selected courses reactive used to filter the courses
  selected_courses <- reactiveVal()
  
  #observer to save the checkbox options into a reactive expression
  observeEvent(input$courseCheckbox, {
    selected_courses(input$courseCheckbox)
    
    enable("assignmentShowHide")
    # Update the reactive value when the selection changes
  })
  

  
  #### Load Course Data ####
  
  #This is to show/hide the courses
  observeEvent(input$assignmentShowHide,{
    toggle(id = "assignment_div",anim = T)
  })
  
  assignment_list_df <- reactive({
    
    tryCatch({
      if (!is.null(connection_status()) && connection_status() == "Success") {
        get_like_assignments(course_list_df()%>%filter(name %in% selected_courses()))
        
      } else {
        NULL
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error has occurred, please check your course selections. You may have to restart the app.", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  output$assignmentUI <- renderUI({
    fluidPage(
      checkboxGroupInput("assignmentCheckbox", 
                         HTML("<b>Select the assignments you would like to include 
                         in your gradebook pull:</b>"), 
                         choices = unique(assignment_list_df()$name),
                         #selected=course_list_df()$name,
                         inline = FALSE,
                         width='600px'),
      #add a button to download the assignment roster using the selected courses
      #if assignmentCheckbox has at least one selection then enable load_canvas_data
      #else disable load_canvas_data
      enable("load_canvas_data")
    )
  })
  
  #student roster with instructor info, called by the download gradebook btn
  roster_course_df <- reactiveVal()
  #This can be used in the plotting function maybe
  course_gradebook <- reactiveVal()
  
  observeEvent(input$load_canvas_data, {
      req(input$assignmentCheckbox)
      #get the rosters for all courses if it hasn't been already
      if(is.null(roster_course_df())){
        roster_course_df(get_student_roster(course_list_df()%>%filter(name %in% selected_courses()),"instr"))
        #optional parameter to search for instructors by a string
      }

      if(is.null(assignment_list_df())){
        assignment_list_df(get_like_assignments(course_list_df()%>%filter(name %in% selected_courses())))
      }

      roster.course <- roster_course_df()
      roster.course <- roster.course %>% mutate(`Max Points`=grades.unposted_current_points/grades.unposted_current_score*100)
      names(roster.course)[5:7] <- c("Score","Grade","Points")
      roster_course_df(roster.course)

      #get the assignment list reactive value
      assign.df <- assignment_list_df()

      #get the selected assignments from the checkbox reactive
      selectedAssignments <- input$assignmentCheckbox

      #filter the assignment list to just the selected assignments
      assign.df <- assign.df %>% filter(name %in% selectedAssignments)

      #get the number of students in each course
      course.enrollment <- roster.course %>% group_by(course_id) %>% summarise(n = n())

      #join enrollment to assign.lookup
      assign.df <- assign.df %>% left_join(course.enrollment, by = "course_id")

      withProgress(message = "Gathering Assignment Data by User for All Courses", value=0, {
        gb.comb <- assign.df %>% group_by(course_id) %>%
          group_split %>% purrr::map_df(function(x,i){
            incProgress(1/length(unique(assign.df$course_id)))
            get_bulk_assignments(x, unique(x$course_id))},i=1:length(unique(assign.df$course_id)))
      })

      gb.comb <- gb.comb %>% filter(user_id %in% roster.course$user_id)

      #match the assignment id to an assignment name
      #add a name column that is based on the name match in assign.lookup by id
      #change the name of id in assign.lookup to assignment_id
      assign.df <-  assign.df %>% rename(assignment_id = id)

      gb.comb.parse <- gb.comb %>%
        left_join(assign.df, by = "assignment_id")%>%
        select(user_id,course_id,name,score,points_possible,assignment_group_id)%>%
        mutate(percent = score/points_possible)

      #rename name to assignment_name
      gb.comb.parse <-  gb.comb.parse %>% rename(assignment_name = name)
      #reactive value that may be useful in the plotting function
      # find the name in assign.group.comb and add it to the gb.comb.parse by assign_group_id
      gb.comb.parse <- gb.comb.parse %>% left_join(assign.group.comb(), by = "assignment_group_id")

      #I may need to get the assignment group name
      course_gradebook(gb.comb.parse)

      #Pivot wider to get assignment id as columns names
      gb.comb.parse <- gb.comb.parse %>% pivot_wider(names_from = assignment_name,
                                                     values_from = c(score,points_possible,percent),
                                                     names_glue = "{assignment_name}_{.value}")

      #now join the user_id and roster.course to get the student names
      gb.xl <- roster.course  %>% left_join(gb.comb.parse, by = "user_id")

      #download gb.xl as an xlsx file
      #write.xlsx(gb.xl, file)
    })
  
  
  #### Download Buttons ####
  
  #download the WPR grading template, to be used to process the WPR brief
  output$download_template_canvas <- downloadHandler(
    filename = function() {
      "gradebook.xlsx"
    },
    content = function(file) {
      
      
    })
  
  #download the canvas data in a familiar gradebook excel format
   output$download_gradebook_canvas <- downloadHandler(
     filename = function() {
       "gradebook.xlsx"
     },
    content = function(file) {
      req(input$assignmentCheckbox)
      #make sure course_gradebook() is not null
      req(!is.null(course_gradebook()))

      roster.course <- roster_course_df()

      gb.comb.parse <- course_gradebook()
      #Pivot wider to get assignment id as columns names
      gb.comb.parse <- gb.comb.parse %>% pivot_wider(names_from = assignment_name,
                                                     values_from = c(score,points_possible,percent),
                                                     names_glue = "{assignment_name}_{.value}")

      #now join the user_id and roster.course to get the student names
      gb.xl <- roster.course  %>% left_join(gb.comb.parse, by = "user_id")

      #download gb.xl as an xlsx file
      write.xlsx(gb.xl, file)
    })


      
      
  #### tab for data visualization ####
  
  
  filteredPlotData <- reactive({
    course_gradebook() %>% group_by(course_id, assignment_group_id) %>%
      summarise(n=n(),mean=mean(percent,na.rm=TRUE),sd=sd(percent,na.rm=TRUE))
  })
  
  
  observe({
    updateSelectInput(session, "group_id",
                      choices = unique(course_gradebook()$assignment_group_id))
  })
  
  output$plot <- renderPlot({
    ggplot(filteredPlotData(), aes(x = course_id, y = mean,fill=assignment_group_id)) +
      geom_bar(stat = "identity") +
      theme_minimal()
  })
  
  
  #### tab for table viz####
  # AS OF 14JAN basic functionality works below
  # the below shows the course table filtered to just id and name
  filteredCourseData <- reactive({
    if (length(selected_courses) > 0) {
      course_list_df() %>% filter(name %in% selected_courses()) %>%
        select(id,name)
      #subset(course_list_df(), name %in% selected_courses())
    } else {
      if(!is.null(course_list_df())){
        course_list_df()
      } else {
        NULL
      }
      course_list_df()  # If no selection, show the entire dataframe
    }
  })
  
  output$course_table <- renderDT({
    filteredCourseData()
  }, options = list(pageLength = 5))
  
  
  
  
  
}
