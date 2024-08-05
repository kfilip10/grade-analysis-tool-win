# 
# 
# #### UI and server logic for Canvas API Integration####
# 
# The first functions below are called as tabset panels in app.R
#this is the first tabset, it runs on loading the tab.

#this is the function that is called from app.R
#This triggers the check for the API connection
createCanvasPrepPage <- function() {
  # Custom CSS to change the highlight color of selected row
  
  tabPanel("Canvas Data",
           sidebarLayout(
             sidebarPanel(
                    tags$br(),
                    uiOutput("tokenStatus"),  # withSpinner(Placeholder for token status
                    withSpinner(uiOutput("connectionStatus")),
                    h4("Courses Selected:"),
                    uiOutput("selected_courses_list"),
             ),
             mainPanel(
               h3("1. Select Courses"),
               actionButton(inputId = "courseShowHide",label = "Show/Hide Course Selection"),
               actionButton("saveCourseList", "Save Selected Courses to File"),
               actionButton("loadCourseList", "Load Courses from File"),
               
               div(id="course_checkbox_div",#style="display:none",
                   dataTableOutput("courseCheckboxUITable")
                ),
               
               h3("2. Load and Select Assignments"),
               actionButton(inputId = "loadAssignmentsCanvas",label = "Load Assignments From Canvas",
                            style="color: black; background-color: #cfbb34"),
               actionButton(inputId = "loadAssignmentsFromFile",label = "Load Assignments From Saved Data",
                            style="color: white; background-color: #31464f"),
               actionButton("saveAssignmentList", "Save Assignment List to File",
                            style="color: white; background-color: #31464f"),
               tags$br(),
               tags$br(),
               actionButton(inputId = "assignmentShowHide",label = "Show/Hide Assignment List"),
              tags$p("Loading the assignment listing will take approximately 2 seconds per course selected."),
               div(id="assignment_div",
                   uiOutput("assignmentUI")
               ),
                h3("3. Load the assignment data for the selected courses:"),
               disabled(actionButton(inputId = "load_canvas_data",label = "Load Grade and Roster Data from Canvas",
                                      style="color: black; background-color: #cfbb34")),
                tags$hr(),
                h3("4. Download Options"),
                h4("4a. Download a gradebook of the checkbox assignments above:"),
                disabled(downloadButton("download_gradebook_canvas", label="Download Canvas Gradebook",
                                        style="color: black; background-color: #cfbb34")
                ),
                h4("4b. Download a grading template:"),
                h5("Select which assignment to generate a grading template for:"),
                uiOutput("templateAssignmentUI"),
                disabled(downloadButton("download_template_canvas", label = "Download WPR Grading Template",
                         style="color: black; background-color: #cfbb34"))
               
               
          
        )
      )
)
  
  
}
# JavaScript to toggle the display property of the div


#UI element for assignment grouping data visualizer
#IN PROGRESS - LOW PRIORITY
canvas_assignment_viewer <- function() {
  fluidPage(

    titlePanel("Assignment Group Data with Filtering"),
    mainPanel(
        selectInput("group_id", "Assignment Group ID",
                    choices = NULL),

        plotOutput("plotByAssignmentGroup")
    ),
    fluidRow(
      column(width = 10,
             h3("This will be a tabular viewer of assignment level info"),
             DTOutput("course_table")
      )
    )
  )
}

#plot and table viewer similar to above for overall grades in the course by section
#IN PROGRESS - LOW PRIORITY

canvas_gradebook_viewer<- function() {
  #Build in gradbook viewer plot and table.

}

# function to take the template excel file and upload grades to canvas
canvas_grade_upload <- function (){
  sidebarLayout(
    sidebarPanel(
      p("This page allows you to upload your Canvas Grade Data from the grading template"),
      #uiOutput("tokenStatus"),  # Placeholder for token status
      h3("Instructions"),
      #browser(),
      tags$ol(
        tags$li("Upload the template file with your grade data. The program will automatically check for duplicate entries and other errors."),
        tags$li("If errors are present fix any duplicate entries or errors and reupload."),
        tags$li("If no errors are present click the 'Upload Grade Data' button to push the grades to Canvas"),
        tags$li("To check if the upload was succesful return to the Canvas Data tab and download a gradebook using that assignment."),
      ),
      # upload
      h4("Upload the template file with your grade data:"),
      fileInput("WPRGradesUpload", HTML("<b>WPR Grade Data (.xlsx)</b>"), accept = c(".xlsx")),
      uiOutput("checkboxUpload"),
      #action button to load the data from the template
      actionButton("load_excel_upload", "Load Data from Template"),
      h3("Upload grades to canvas:"),
      disabled(actionButton("upload_grades_canvas", "Upload Grade Data"))
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Duplicate Entries",
                 h4("Duplicate Entries"),
                 p("The following duplicate entries (one cadet with grades in multiple versions:) were found that you should investigate:"),
                 DTOutput("duplicateTableUpload")
                 ),
        tabPanel("No Exam Entry Records",
                 h4("Cadets with no Exam Entry found in the uploaded data"),
                 DTOutput("noEntryTableUpload"),
                ),
        tabPanel("Exam Data",
                 h4("Data to be Uploaded to Canvas"),
                 DTOutput("dataTableUpload")
        )
      )
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
          h3("Instructions"),
          tags$ol(
            tags$li("First Select the courses you want to collect data from."),
            tags$li("NOTE: The courses you select must have assignments with the same name (it is designed to work with Canvas blueprint)"),
            tags$li("Click the 'show/hide' Assignment Listing to generate list of assignments to choose from"),
            tags$li("Click the 'automatically load data from canvas' button"),
            tags$li("Now you are able to view graph data (other tabs), download a grading template, and download a gradebook."),
          ),
          tags$hr(),
          )
      loadSelections()
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
      course_list_df <- get_course_list()
      #finds the year within the parenthesis
      course_list_df <-  course_list_df%>%mutate(term = str_extract(name, "\\b\\d{4}-[12](?=-)"))
      
      #finds the name right after the parenthesis
      course_list_df <-  course_list_df%>%  mutate(course = str_extract(name, "^[^(]+"))
      
      #finds the four digit hours and section number
      hours <- course_list_df$name %>% str_extract("\\b[A-Z0-9]{4}\\b\\s+\\d+")
      # extract the section from hours, which is the ending numeric
      section <- str_extract(hours, "\\d+\\s*$")
      
      course_list_df <- course_list_df %>%
        mutate(section = section)
      
      #extract just the hours from the full string
      hours <- str_extract(hours, "\\b[A-Z0-9]{4}\\b")
      
      course_list_df <- course_list_df %>%
        mutate(section_hour = hours)
      course_list_df
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
      
      #check if course_list_default is all listed in the course_list_df
      #if(!all(course_list_default() %in% course_list_df()$name)){
      #  course_list_default("")
      #}
      
      return(assign.group.comb)
    } else {
      NULL
    }
  })
  
  #### Course Selection ####
  selected_courses <- reactiveVal(data.frame())
  

  #### Course Selection - Datatable w. Filters ####
  output$courseCheckboxUITable <- DT::renderDataTable({
    datatable(
      course_list_df() %>%select(course,section_hour,section,term,id),
      colnames = c(
        "Course" = "course",
        "Hours" = "section_hour",
        "Section" = "section",
        "Term" = "term",
        "ID" = "id"
      ),
      selection = 'multiple',  # Enable multiple selection
      filter = 'top',          # Enable column-based filtering
      options = list(pageLength = 10,autoWidth = TRUE,
                     columnDefs = list(
        list(width = '40px', targets = c(2,3)) # Adjust specific column width if needed
      ))
       # Enable automatic column width adjustment
    )
  })
  
  observeEvent(input$courseCheckboxUITable_rows_selected, {
    selected_indices <- input$courseCheckboxUITable_rows_selected
    selected_courses_df <- course_list_df()[selected_indices, , drop = FALSE]
    selected_courses(selected_courses_df)  

        #print(selected_items)
  })
  
    
  output$selected_courses_list <- renderUI({
    selected_data <- selected_courses()
    if (nrow(selected_data) > 0) {
      tags$ul(
        # Iterate over each row of the selected data frame
        lapply(1:nrow(selected_data), function(i) {
          course <- selected_data[i, "name"]
          tags$li(paste(course))
        })
      )}
      #browser()
     else {
      h5("No courses selected")
    }
  })
  
    #### Save/Load Default Courses ####
  observeEvent(input$saveCourseList, {
    #browser()
    selected_indices <- input$courseCheckboxUITable_rows_selected
    selected_courses_df <- course_list_df()[selected_indices, , drop = FALSE]
    # Attempt to save the file and update the save status
    tryCatch({
      saveRDS(selected_courses_df, COURSE_DEFAULTS_PATH)
      showModal(modalDialog(
        title = "Success",
        paste("Courses Saved"),
        easyClose = TRUE,
        footer = NULL
      ))    }, 
      error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("An error has occurred in saving. You may have to restart the app.", e$message),
          easyClose = TRUE,
          footer = NULL
        ))    
        
        })
    
    


    # Close the modal dialog after a delay to allow user to read the message
    #invalidateLater(2000) # 2 seconds delay
    #removeModal()
  })
  # Load selected courses from a file
  observeEvent(input$loadCourseList, {
    loadSelections()
  })
  
  loadSelections <- function(){
    if (file.exists(COURSE_DEFAULTS_PATH)) {
      saved_courses <- readRDS(COURSE_DEFAULTS_PATH)
      selected_rows <- which(course_list_df()$id %in% saved_courses$id)
      if (!is.null(selected_rows)){
        proxy <- dataTableProxy("courseCheckboxUITable")
        selectRows(proxy, selected_rows)
        showNotification("Default courses loaded from file!")
        
        
      }
      else{
        showNotification("Something else happened.")
        
      }
    } else {
      showNotification("No default saved courses found. Try to save your courses to save time!", type = "error")
    }
  }
  
  
  
  
  #### Load Course Data ####
  
  #df for assignments
  assignment_list_df <- reactiveVal(NULL)
  
  
  #This is to show/hide the courses
  observeEvent(input$assignmentShowHide,{
    toggle(id = "assignment_div",anim = T)
  })
  
  #### Load assignments from Canvas ####
  #loadAssignmentsCanvas button is clicked
  observeEvent(input$loadAssignmentsCanvas, {
    #require selected courses to be not NULL
    req(selected_courses())
    #if the connection is successful, then load the assignments
    if (!is.null(connection_status()) && connection_status() == "Success" && 
        !is.null(selected_courses()) && length(selected_courses()) > 0) {
      #get the assignments
      df <- get_like_assignments(course_list_df()%>%filter(name %in% selected_courses()$name))
      assignment_list_df(df)
      if (is.null(assignment_list_df())) {
        showModal(modalDialog(
          title = "Important Message",
          "No common assignments found for the selected courses, make another selection.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    } else {
      showModal(modalDialog(
        title = "Error",
        paste("An error has occurred, please check your course selections. You may have to restart the app."),
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  #list of assignments from Canvas that are shared across all selected courses
  
  observeEvent(input$loadAssignmentsFromFile, {
    #require selected courses to be not NULL
    req(selected_courses())
    
    #check if there are defaults
    if(!file.exists(ASSIGNMENT_DEFAULTS_PATH)){
      showModal(modalDialog(
        title = "Error",
        paste("No defaults assignments found, please load them from canvas and save as a default assignment list"),
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    #if the connection is successful, then load the assignments
    if (!is.null(connection_status()) && connection_status() == "Success" && 
        !is.null(selected_courses()) && length(selected_courses()) > 0) {
      #get the assignments
      df <- readRDS(ASSIGNMENT_DEFAULTS_PATH)
      assignment_list_df(df)
    } else {
      showModal(modalDialog(
        title = "Error",
        paste("An error has occurred, please check your course selections. You may have to restart the app."),
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  #### Save Assignment List as Default ####
  observeEvent(input$saveAssignmentList, {
    # Attempt to save the file and update the save status
    req(assignment_list_df())
    tryCatch({
      saveRDS(assignment_list_df(), ASSIGNMENT_DEFAULTS_PATH)
      showModal(modalDialog(
        title = "Success",
        paste("Assignment Data Saved. You can reload this data from file the next time you use the app."),
        easyClose = TRUE,
        footer = NULL
      ))    }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("An error has occurred in saving. You may have to restart the app.", e$message),
          easyClose = TRUE,
          footer = NULL
        ))    })
    
    # Close the modal dialog after a delay to allow user to read the message
    #invalidateLater(2000) # 2 seconds delay
    #removeModal()
  })
  
  
  ####Assignment list UI####
  output$assignmentUI <- renderUI({
    fluidPage(
      #check if the assignment list is a single string (indicates there are no assignments)
      checkboxGroupInput("assignmentCheckbox", 
                         HTML("<b>Select the assignments you would like to include 
                         in the Canvas Data Pull gradebook pull:</b>"), 
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
  
  loaded_course_df <- reactiveVal()
  
  #This can be used in the plotting function maybe
  course_gradebook <- reactiveVal()
  
  observeEvent(input$load_canvas_data, {
      #req(input$assignmentCheckbox)
      #get the rosters for all courses if it hasn't been already
      #browser()
    
      if(is.null(roster_course_df())){
        #message to user to select assignments
        roster_course_df(get_student_roster(course_list_df()%>%filter(name %in% selected_courses()$name),"instr"))
      }
      
      roster.course <- roster_course_df()
      roster.course <- roster.course %>% mutate(
        `Max Points`=grades.unposted_current_points/grades.unposted_current_score*100)
      names(roster.course)[6:8] <- c("Score","Grade","Points")
      #roster.course <-  roster.course %>% rename(section = name)
      
      loaded_course_df(roster.course)

      #get the assignment list reactive value
      assign.df <- assignment_list_df()
      #get the selected assignments from the checkbox reactive
      selectedAssignments <- input$assignmentCheckbox

      #filter the assignment list to just the selected assignments
      assign.df <- assign.df %>% filter(name %in% selectedAssignments)

      #get the number of students in each course
      course.enrollment <- roster.course %>% group_by(course_id) %>% summarise(n = n())

      #join enrollment to assign.lookup
      assign.df <- assign.df %>%
        left_join(course.enrollment, by = "course_id")


      withProgress(message = "Gathering Assignment Data by User for All Courses From Canvas", value=0, {
        # gb.comb <- assign.df %>% group_by(course_id) %>%
        #   group_split %>% purrr::map_df(function(x,i){
        #     incProgress(1/length(unique(assign.df$course_id)))
        #     get_bulk_assignments(x, unique(x$course_id))},i=1:length(unique(assign.df$course_id)))
        
        #edit from GPT
        gb.canvas <- assign.df %>%
          group_by(course_id) %>%
          group_split() %>%
          purrr::map_dfr(~{
            incProgress(1/length(unique(assign.df$course_id)))
            get_bulk_assignments(.x, unique(.x$course_id))
          })
        
        
        
      })
      withProgress(message = "Merging Data", value=0, {
        
      gb.comb <- gb.canvas %>% filter(user_id %in% roster.course$user_id)

      #match the assignment id to an assignment name
      #add a name column that is based on the name match in assign.lookup by id
      #change the name of id in assign.lookup to assignment_id
      assign.df <-  assign.df %>% rename(assignment_id = id)
      setProgress(value = 0.25, message = "Rename Data...")
      
      gb.comb.parse <- gb.comb %>%
        left_join(assign.df, by = "assignment_id")%>%
        select(user_id,course_id,name,score,points_possible,assignment_group_id)%>%
        mutate(percent = score/points_possible)
      

      setProgress(value = 0.5, message = "Joining Data...")
      
            #rename name to assignment_name
      gb.comb.parse <-  gb.comb.parse %>% rename(assignment_name = name)
      #reactive value that may be useful in the plotting function
      # find the name in assign.group.comb and add it to the gb.comb.parse by assign_group_id
      setProgress(value = 0.6, message = "Fetching Assginment Groups")
      
      assign.comb <- assign.group.comb()
      
      assign.comb <- assign.comb %>% select(assignment_group_id,assignment_group_name)
      
      setProgress(value = 0.6, message = "Left Join ")
      
      gb.comb.parse <- gb.comb.parse %>% left_join(assign.comb, by = "assignment_group_id")
      
      setProgress(value = 0.75, message = "Joining More Data...")
      
      #this is not a long term solution, but ok for now I guess
      
      #this is the section
      #roster.course <-  roster.course %>% mutate(section = substr(section, nchar(section)-7, nchar(section)))
      
      #roster.course <-roster.course %>% mutate(section = substr(section, nchar(section)-7, nchar(section)))
      #roster.course <-roster.course %>% mutate(section = substr(section, 1, nchar(section)-1))
      roster.course <-roster.course %>% mutate(course_name = str_c(instructor,section,sep = "-"))
      setProgress(value = 0.9, message = "Mutating Data...")
      
      
      course.instructor.df <- as.data.frame(roster.course %>% group_by(course_id) %>% 
                                              select(instructor,section,section_hour,course_name) %>% distinct())
      #browser()
      gb.comb.parse <- gb.comb.parse %>% left_join(course.instructor.df, by = "course_id")
      
      
      
      #I may need to get the assignment group name
      #I have the assignmentgroup name
      #column names for this:
      #user_id int
      #course_id int
      #assignment_name end
      #score num
      #points_possible num
      #percent num
      #assignment_group_id int 
      #assignment_group_name chr
      
      #I could add the instructor name to this, or leave anonymized 
      course_gradebook(gb.comb.parse)
      updateSelectInput(session, "select_template_assignment",
                        choices = course_gradebook()$assignment_name)
      setProgress(value = 1.0, message = "Finished!")
      #add modal to show that the data has been loaded
      showModal(modalDialog(
        title = "Data Loaded",
        "Canvas Data Loaded Successfully",
        easyClose = TRUE,
        footer = NULL
      ))
      enable("download_template_canvas")
      enable("download_gradebook_canvas")
      })
    })
  
  
  #### Download Excel Template ####
  
  output$templateAssignmentUI <- renderUI({
    selectInput("select_template_assignment",
                label = "Choose an assignment for your template:",
                choices = NULL,
                selected = NULL)
    })

  #reactive for the select template assignment list after course_list_df() is not null
  observe({
    if (!is.null(course_list_df())) {

    }
  })
  
  #list the assignments that are available to make a template for
  observeEvent(input$select_template_assignment, {
    req(input$select_template_assignment)
  })
  
  
  #download the WPR grading template, to be used to process the WPR brief
  output$download_template_canvas <- downloadHandler(
    filename = function() {
      "Grading Template.xlsx"
    },
    content = function(file) {
      #Pivot wider to get assignment id as columns names
      req(roster_course_df())
      req(assignment_list_df())
      
      list.template <- excel_template_from_canvas(loaded_course_df(), 
                                 assignment_list_df(), 
                                 input$select_template_assignment)
      
      
      file.copy(list.template[[1]], file)
      
    })
  #### Download Canvas Gradebook ####
  #make a reactive for loaded_canvas_gradebook()
  
  
  
  #download the canvas data in a familiar gradebook excel format
   output$download_gradebook_canvas <- downloadHandler(
     filename = function() {
       "gradebook.xlsx"
     },
    content = function(file) {
      req(input$assignmentCheckbox)
      #make sure course_gradebook() is not null
      req(!is.null(course_gradebook()))
      #browser()
      roster.course <- loaded_course_df()

      gb.comb.parse <- course_gradebook() %>% rename(points = score)
      #Pivot wider to get assignment id as columns names
      admin_cols <- c("user_id","course_id","instructor",
                      "section", "section_hour")
      N_admin = length(admin_cols)
      #browser()
      #summarise points, points_possible, and score for each user by assignment_group_name
      summary_assign_groups <- gb.comb.parse %>% group_by(user_id,assignment_group_name) %>% 
        summarise(points = sum(points),points_possible = sum(points_possible)) %>% 
        mutate(percent = points/points_possible)
      
      gb.comb.parse <- gb.comb.parse %>% select(-course_name,-assignment_group_id,-assignment_group_name,
                                                all_of(admin_cols))%>%  
        pivot_wider(names_from = assignment_name,values_from = c(points,points_possible,percent),
                                                     names_glue = "{assignment_name}_{.value}",names_sort=TRUE) 
      
      gb.comb.parse <- gb.comb.parse %>% select(1:N_admin,order(names(.)[(N_admin+1):ncol(.)])+N_admin)
      
      #get the assignment group summaries and add to gb.comb.parse, match by user_id

      
      #pivot asummary assign groups to wide format
      summary_assign_wide <- summary_assign_groups %>% pivot_wider(names_from = assignment_group_name,
                                                                   values_from = c(points,points_possible,percent),
                                                                   names_glue = "{assignment_group_name}_{.value}",names_sort=TRUE)
      summary_assign_wide <- summary_assign_wide %>% select(1:1,order(names(.)[2:ncol(.)])+1)
      
      gb.comb.parse <- gb.comb.parse %>% left_join(summary_assign_wide, by = "user_id")
      

      
      gb.xl <- roster.course %>% select(user_id,user.sortable_name,email,
                                   Score,Grade,Points,`Max Points`)  %>% 
        left_join(gb.comb.parse, by = "user_id")%>% 
        mutate(Score = Score/100,`Max Points` = round(`Max Points`,digits=1),
               Points = round(Points,digits=1))
      
     # df_renamed <- rename(df, NewName1 = OldName1, NewName2 = OldName2)
      gb.xl <- rename(gb.xl, ID = user_id, Name = user.sortable_name, `Course ID` = course_id,
                        Instructor = instructor, Section = section,Hour=section_hour,
                                Email = email, Score = Score, Grade = Grade, Points = Points,
                                `Max Points` = `Max Points`)

      gb.xl <- gb.xl %>% select(ID,Name,Email,`Course ID`,Instructor,Section,Hour,
                                Grade,Score,Points,`Max Points`,everything())
      #download gb.xl as an xlsx file
      write.xlsx(gb.xl, file)
    })


  #### tab for data visualization ####
  filteredPlotData <- reactive({
    req(course_gradebook())
    course_gradebook() %>% group_by(section, assignment_group_name) %>%
      summarise(n=n(),mean=mean(percent*100,na.rm=TRUE),sd=sd(percent*100,na.rm=TRUE),1) %>%
      mutate(mean = round(mean,1), sd = round(sd,1))
  })
  
  #need to add the select input to the data visualization
  observe({
    updateSelectInput(session, "group_id",
                      choices = unique(course_gradebook()$assignment_group_name))
  })
  
  output$plotByAssignmentGroup <- renderPlot({
    ggplot(filteredPlotData(), aes(x = section, y = mean,fill=assignment_group_name)) +
      geom_bar(stat = "identity",width=0.5) +
      theme_bw() + theme(text = element_text(size=20))+
      #add error bars
      geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                    position=position_dodge(0.9)) 
  })
  
  
  #Also can do a plot of roster.course
  # I have the section and max points so that CD can check who hasn't posted to canvas
  
  #### tab for table viz####
  # AS OF 14JAN basic functionality works below
  # the below shows the course table filtered to just id and name
  filteredCourseData <- reactive({
    
    if (length(selected_courses) > 0) {
      course_list_df() %>% filter(name %in% selected_courses()$name) %>%
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
    filteredPlotData()
  }, options = list(pageLength = 20))
  
  
  ####Upload to canvas---------------- ####

  #### Upload Excel  "WPRGradesUpload"####
  WPRGradesUpload.path <- reactiveVal(NULL)
  upload.list <- reactiveValues(dataframes=list())
  num_versions_upload <- reactiveVal(0)
  
  observeEvent(input$WPRGradesUpload, { 
    req(input$WPRGradesUpload$datapath)
    WPRGradesUpload.path(input$WPRGradesUpload$datapath)
    enable("importWPRExcelBtn")
  })
  
  ####Check box Version Select ####
  output$checkboxUpload <- renderUI({
    req(WPRGradesUpload.path())
    
    inFile <- WPRGradesUpload.path()
    
    if (is.null(inFile))
      return(NULL)
    
    sheets <- excel_sheets(inFile)
    #if sheets has a value with "instructions" in it, remove it
    if(any(grepl("Instructions",sheets))){
      sheets <- sheets[-grep("Instructions",sheets)]
    }
    checkboxGroupInput("WPR_sheets", "Select Versions of WPR to analyze:", 
                       choices = sheets,
                       selected=sheets)
  })

  

  #button to combine data and check for duplicate entries before giving upload option
 observeEvent(input$load_excel_upload,{
   req(WPRGradesUpload.path())
   
   sheets <-  input$WPR_sheets #sheets list from the checkboxes
   num_versions_upload(length(sheets))
   
   tryCatch({
     list.df <- list()
     versions <- sheets
     for(n in 1:length(versions)){
       list.df[[n]] <- read_excel(WPRGradesUpload.path(),sheet=versions[n]) 
     }
     df.list <- import_WPR_excel(list.df,num_versions_upload())
     upload.list$dataframes <- df.list
     
     df.list <- import_WPR_excel(list.df,length(sheets))
     #first is df of all grades for gradebook, second element is df of questions
     #third element is df of duplicates, fourth element is those that aren't in either version
     upload.list$dataframes <- df.list
     showModal(modalDialog(
       title = "Import Successful",
       HTML("Check for duplicates, the cadets with no exams, and the grade data for accuracy."),
       footer = modalButton("Close")
     ))
     enable("upload_grades_canvas")
     
   },
   error = function(e) {
     showErrorModal(paste("Error:", e$message))
   })
 })
  
  
  
  #### Upload - Duplicates Table ####
 
   output$duplicateTableUpload <- renderDT({
     req(!is.null(upload.list$dataframes), length(upload.list$dataframes) >= 3)
     if (!is.null(upload.list$dataframes)) {
       datatable(upload.list$dataframes[[3]],options=list(
         pageLength = 25,  # Set initial number of entries to display
         autoWidth = TRUE,  # Automatically adjust column width
         searching = TRUE,  # Enable search box
         order = list(list(0, 'asc')),  # Initial sorting column (0-based index)
         lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')),  # Dropdown for page length options
         columnDefs = list(list(className = 'dt-left', targets = '_all'))  # Left-align all columns
         
       ))
     } else {
       print("No duplicates found")
     }
   },server=FALSE)
   #### Upload - No Entry Table ####
   output$noEntryTableUpload <- renderDT({
     req(!is.null(upload.list$dataframes), length(upload.list$dataframes) >= 3)
     if (!is.null(upload.list$dataframes)) {
       datatable(upload.list$dataframes[[4]],options=list(
         pageLength = 25,  # Set initial number of entries to display
         autoWidth = TRUE,  # Automatically adjust column width
         searching = TRUE,  # Enable search box
         order = list(list(0, 'asc')),  # Initial sorting column (0-based index)
         lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')),  # Dropdown for page length options
         columnDefs = list(list(className = 'dt-left', targets = '_all'))  # Left-align all columns
         
       ))
     } else {
       print("All cadets have an exam record.")
     }
   },server=FALSE)
   #### Upload - Data Table ####
   
   output$dataTableUpload <- renderDT({
     req(!is.null(upload.list$dataframes), length(upload.list$dataframes) >= 3)
     if (!is.null(upload.list$dataframes)) {
       datatable(upload.list$dataframes[[1]]%>%select(ID,Section,Name,version,
                                                      mge.percent,mge.points,mge.grade)%>%
                   mutate(mge.percent = round(mge.percent,1))%>%
                   rename("WPR %" = mge.percent,
                          "WPR Points" = mge.points,
                          "WPR Grade" = mge.grade),
                 options=list(
         pageLength = 25,  # Set initial number of entries to display
         autoWidth = TRUE,  # Automatically adjust column width
         searching = TRUE,  # Enable search box
         order = list(list(0, 'asc')),  # Initial sorting column (0-based index)
         lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')),  # Dropdown for page length options
         columnDefs = list(list(className = 'dt-left', targets = '_all'))  # Left-align all columns
         
       ))
     } else {
       print("No duplicates found")
     }
   },server=FALSE)
 
 
   #### Upload Logic ####
   observeEvent(input$upload_grades_canvas, {
     #logic to upload to canvas
     # define based on template reactive
     #add a modal to ask to verify they checked for duplicates, if they say yes then proceed, if no then exit the function
     showModal(modalDialog(
       title = "Confirm Upload",
       "Did you check for duplicates and check the data looked correct? \n 
        'Yes' to proceed with upload, 'No' to exit and return to app.",
       footer = tagList(
         actionButton("ConfirmUpload", "Yes"),
         actionButton("DenyUpload", "No")
       )
     ))
     
     
   })
   observeEvent(input$ConfirmUpload, {
     removeModal()
     #ID, Name,version,Section, Course.ID,Assignment.ID,mge.points
     grades.df <- upload.list$dataframes[[1]]
     #rename mge.points to points
     grades.df <- grades.df %>% rename(id=ID,
                                       points = mge.points,
                                       course_id = Course.ID,
                                       assignment_id = Assignment.ID)
     
     resp <- lapply(split(grades.df, grades.df$course_id), grade_assignments_bulk)
     
     #resp is a string display it using a modal so user can see if it worked
     resultVector <- unlist(resp)
     concatenatedResult <- paste(resultVector, collapse = "<br/>")
     
     showModal(modalDialog(
       title = "Upload Status",
       HTML(concatenatedResult), # Use HTML() to interpret line breaks
       footer = modalButton("Close")
     ))
   })

  observeEvent(input$DenyUpload, {
    removeModal()
   })


}
