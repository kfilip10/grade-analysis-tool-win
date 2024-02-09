# 
# 
# #### UI and server logic for Canvas API Integration####
# 
# The first functions below are called as tabset panels in app.R
#this is the first tabset, it runs on loading the tab.

#this is the function that is called from app.R
#This triggers the check for the API connection
createCanvasPrepPage <- function() {
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
               div(id="course_checkbox_div", style = "display: none;",
                   uiOutput("courseCheckboxUI")
                  ),
               h3("2. Load and Select Assignments"),
               actionButton(inputId = "loadAssignments",label = "Load Assignments List",
                                     style="color: white; background-color: green"),
              actionButton(inputId = "assignmentShowHide",label = "Show/Hide Assignment List"),
              tags$p("Loading the assignment listing will take approximately 2 seconds per course selected."),
               div(id="assignment_div",
                   uiOutput("assignmentUI")
               ),
                h3("3. Load the assignment data for the selected courses:"),
               disabled(actionButton(inputId = "load_canvas_data",label = "Load Grade and Roster Data from Canvas",
                                      style="color: white; background-color: green")),
                tags$hr(),
                h3("4. Download Options"),
                h4("4a. Download a gradebook of the checkbox assignments above:"),
                disabled(downloadButton("download_gradebook_canvas", "Download Canvas Gradebook")),
                h4("4b. Download a grading template:"),
                h5("Select which assignment to generate a grading template for:"),
                uiOutput("templateAssignmentUI"),
                disabled(downloadButton("download_template_canvas", "Download WPR Grading Template"))
               
               
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
    sidebarLayout(
      sidebarPanel(
        selectInput("group_id", "Assignment Group ID",
                    choices = NULL)
      ),
      mainPanel(
        plotOutput("plotByAssignmentGroup")
      )
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
  # Instructions
  fluidPage(
    titlePanel("Bulk Assignment Grade Upload"),
      mainPanel(
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
        fileInput("graded_template", HTML("<b>WPR Grade Data (.xlsx)</b>"), accept = c(".xlsx")),
        uiOutput("graded_template_checkbox_UI"),
        #action button to load the data from the template
        actionButton("load_template_data", "Load Data from Template"),
        # check for duplicate entries (indicate these will not be uploaded unless fixed and re-uploaded)
        h4("Duplicate Entries:"),
        p("The following duplicate entries (one cadet with grades in multiple versions most likely) were found that you should investigate."),
        tableOutput("duplicateTableUploads"),
        h4("Missing Entries:"),
        p("The following Cadets did not have an entry in the uploaded data or had incomplete grade data and will not be uploaded to Canvas unless corrected in excel."),
        tableOutput("noEntryTableUploads"),
        # call function to upload the file
        h2("Upload grades to canvas:"),
        p("The following Cadets did not have an entry in the uploaded data or had incomplete grade data and will not be uploaded to Canvas unless corrected in excel."),
        disabled(actionButton("upload_grades_canvas", "Upload Grade Data"))
        # display results
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
          #show the course selection button
          #show the courses selected from the courseCheckboxUI
          #actionButton(inputId = "courseShowHide",label = "Show/Hide Course Selection"),
          #div(id="course_checkbox_div", style = "display: none;",
          #    uiOutput("courseCheckboxUI")),
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
      
      #check if course_list_default is all listed in the course_list_df
      if(!all(course_list_default() %in% course_list_df()$name)){
        course_list_default("")
      }
      
      return(assign.group.comb)
    } else {
      NULL
    }
  })
  
  #### Course Selection ####
  selected_courses <- reactiveVal(NULL)
  
  course_list_default <- reactiveVal(  
    if(is.null(readRDS(COURSE_DEFAULTS_PATH))){
      return("")
    }else{
      course_default <- readRDS(COURSE_DEFAULTS_PATH)
      selected_courses(course_default)
      course_default
    })
  
  #render the course list as a checkbox
  output$courseCheckboxUI <- renderUI({
    tagList(
    checkboxGroupInput("courseCheckbox", "Select only the courses that will share the same assignment names:", 
                       choices = course_list_df()$name,
                       selected=course_list_default(),
                       inline = FALSE,
                       width='400px'),
    #add a select all button,
    actionButton("courseCheckboxselectAll", "Select All"),
    actionButton("courseCheckboxdeselectAll", "Deselect All"),
    actionButton("saveCourseList", "Save Selected Courses as Default")
    
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
  
  #observer to save the checkbox options into a reactive expression
  observeEvent(input$courseCheckbox, {
    selected_courses(input$courseCheckbox)
    
    #enable("assignmentShowHide")
    # Update the reactive value when the selection changes
  })
    
  output$selected_courses_list <- renderUI({
    selected <- selected_courses()
    if (is.null(selected) || length(selected) == 0) {
      tags$li("No options selected")
    } else {
      tags$ul(
        lapply(selected, function(option) {
          tags$li(option)
        })
      )
    }
  })
  
  #### Save Default Courses ####
  observeEvent(input$saveCourseList, {
    # Attempt to save the file and update the save status
    tryCatch({
      saveRDS(input$courseCheckbox, COURSE_DEFAULTS_PATH)
      showModal(modalDialog(
        title = "Success",
        paste("Courses Saved"),
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

  
  
  
  
  #### Load Course Data ####
  
  #This is to show/hide the courses
  observeEvent(input$assignmentShowHide,{
    toggle(id = "assignment_div",anim = T)
  })
  
  #loadAssignments button is clicked
  observeEvent(input$loadAssignments, {
    #require selected courses to be not NULL
    req(selected_courses())
    #if the connection is successful, then load the assignments
    if (!is.null(connection_status()) && connection_status() == "Success" && 
        !is.null(selected_courses()) && length(selected_courses()) > 0) {
      #get the assignments
      
      df <- get_like_assignments(course_list_df()%>%filter(name %in% selected_courses()))
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
  
  assignment_list_df <- reactiveVal(NULL)
  
  output$assignmentUI <- renderUI({
    fluidPage(
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
      roster.course <-  roster.course %>% rename(section = name)
      
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
      gb.comb.parse <- gb.comb.parse %>% left_join(assign.group.comb()%>%
                                                     select(assignment_group_id,assignment_group_name), by = "assignment_group_id")

      roster.course <-  roster.course %>% mutate(section = substr(section, nchar(section)-7, nchar(section)))
      
      roster.course <-roster.course %>% mutate(section = substr(section, nchar(section)-7, nchar(section)))
      roster.course <-roster.course %>% mutate(section = substr(section, 1, nchar(section)-1))
      roster.course <-roster.course %>% mutate(course_name = str_c(instructor,section,sep = "-"))
      
      
      course.instructor.df <- as.data.frame(roster.course %>% group_by(course_id) %>% 
                                              select(instructor,section,course_name) %>% distinct())
      
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
      
      enable("download_template_canvas")
      enable("download_gradebook_canvas")

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
      
      list.template <- excel_template_from_canvas(roster_course_df(), 
                                 assignment_list_df(), 
                                 input$select_template_assignment)
      
      
      file.copy(list.template[[1]], file)
      
    })
  #### Download Canvas Gradebook ####
  
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
      gb.comb.parse <- gb.comb.parse %>% select(-course_name,-assignment_group_id,-assignment_group_name,
                                                user_id,course_id,instructor,
                                                section)%>%  
        pivot_wider(names_from = assignment_name,values_from = c(score,points_possible,percent),
                                                     names_glue = "{assignment_name}_{.value}") 

      #now join the user_id and roster.course to get the student names
      
      
      gb.xl <- roster.course %>% select(user_id,user.sortable_name,user.login_id,
                                   Score,Grade,Points,`Max Points`)  %>% 
        left_join(gb.comb.parse, by = "user_id")%>% 
        mutate(Score = Score/100,`Max Points` = round(`Max Points`,digits=1),
               Points = round(Points,digits=1))
      
     # df_renamed <- rename(df, NewName1 = OldName1, NewName2 = OldName2)
      gb.xl <- rename(gb.xl, ID = user_id, Name = user.sortable_name, `Course ID` = course_id,
                        Instructor = instructor, Section = section,
                                Email = user.login_id, Score = Score, Grade = Grade, Points = Points,
                                `Max Points` = `Max Points`)

      gb.xl <- gb.xl %>% select(ID,Name,Email,`Course ID`,Instructor,Section,
                                Grade,Score,Points,`Max Points`,everything())
      #download gb.xl as an xlsx file
      write.xlsx(gb.xl, file)
    })


  #### tab for data visualization ####
  filteredPlotData <- reactive({
    course_gradebook() %>% group_by(course_name, assignment_group_name) %>%
      summarise(n=n(),mean=mean(percent,na.rm=TRUE),sd=sd(percent,na.rm=TRUE))
  })
  
  #need to add the select input to the data visualization
  observe({
    updateSelectInput(session, "group_id",
                      choices = unique(course_gradebook()$assignment_group_name))
  })
  
  output$plotByAssignmentGroup <- renderPlot({
    ggplot(filteredPlotData(), aes(x = course_name, y = mean,fill=assignment_group_name)) +
      geom_bar(stat = "identity") +
      theme_minimal()
  })
  
  
  #Also can do a plot of roster.course
  # I have the section and max points so that CD can check who hasn't posted to canvas
  
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
    filteredPlotData()
  }, options = list(pageLength = 5))
  
  
  #### tab for uploading grades to canvas ####
  #When an excel is uploaded
  observeEvent(input$graded_template, { 
    # Then enables the ability to make the grading template
    #df <- read_excel(input$graded_template$datapath)
    enable("upload_grades_canvas")
  })
  
  
  #make a checkbox of sheets in the excel file
  output$graded_template_checkbox_UI <- renderUI({
    req(input$graded_template$datapath)
    sheets <- excel_sheets(input$graded_template$datapath)

    checkboxGroupInput("graded_template_checkbox", 
                  "Select Sheets from Excel with Grade Data",  
                  choices = sheets,
                  selected=sheets)
  })
  
  canvas.upload.df <- reactiveValues(dataframes=list())
  
  #button to combine data and check for duplicate entries before giving upload option
 observeEvent(input$load_template_data,{
    req(input$graded_template$datapath)

   sheets <-  input$graded_template_checkbox #sheets list from the checkboxes
   
   # Store the number of sheets in a reactive variable for further use
   #load df.total and df.g here
   # df.grades is list of two dataframes (df.total and df.questions)
   tryCatch({
     
     list.df <- list()
     versions <- sheets
     versions <- tolower(versions)
     for(n in 1:length(versions)){
       sheetN <- match(1,str_detect(versions,as.character(n)))
       if(is.na(sheetN)){
         showModal(modalDialog(
           title = "Important message",
           "Make sure you only have one sheet for each version and there is a number '1' or 'one' corresponding to the version number in the sheet name."
         ))
         break
       }
       else {
         list.df[[n]] <- read_excel(input$graded_template$datapath,sheetN)}  
     }
     df.list <- import_WPR_excel(list.df,length(sheets))
     #first is df of all grades for gradebook, second element is df of questions
     #third element is df of duplicates, fourth element is those that aren't in either version
     canvas.upload.df$dataframes <- df.list
     showModal(modalDialog(
       title = "Import Successful",
       HTML("Check for duplicates and correct as needed or upload to Canvas"),
       footer = modalButton("Close")
     ))
     enable("upload_grades_canvas")
     
   },
   error = function(e) {
     showErrorModal(paste("Error:", e$message))
   })
 })
  
  
  
  #### Duplicate Data for Grade Upload ####
  output$duplicateTableUploads <- renderTable({
    if(length(canvas.upload.df$dataframes) > 0) {
      #browser()
    
    duplicates <- canvas.upload.df$dataframes[[3]]
    } else {
      return(NULL)
    }
  })
 #list of students with no entry
 output$noEntryTableUploads <- renderTable({
   if(length(canvas.upload.df$dataframes) > 0) {
     #browser()
     
     duplicates <- canvas.upload.df$dataframes[[4]]
   } else {
     return(NULL)
   }
 })
 
 
  observeEvent(input$upload_grades_canvas, {
    #logic to upload to canvas
    # define based on template reactive
    
    #ID, Name,version,Section, Course.ID,Assignment.ID,mge.points
    grades.df <- canvas.upload.df$dataframes[[1]]
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
}
