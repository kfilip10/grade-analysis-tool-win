


# UI ----

gs_updategrades_ui <- function(){
fluidPage(
  tabsetPanel(
    
    
    tabPanel("Select Assignment Data", 
             
      h3("1. Load Assignments from Canvas"),
      # assignments list as a datatable
      actionButton("gs_loadassignments_button", "Load Assignments",style = "width: 400px;"),
      br(),
      h3("2. Select Assignment to Update"),
      div(id="gs_assignmentselect_div",
          uiOutput("gs_assignmentselect_UI")
      ),
      p("After selecting the assignment you want to update go to the next tab to update the grades."),
      h3("Unpublished Assignments"),
      p("If you do not see the assignment you want to update, check the list below to see if it is unpublished. If it is unpublished you will need to publish it before we can update grades."),
      uiOutput("unpublishedassignments_ui")       # Placeholder for reactive UI
    ),
    
    
      #data table of grades to update
      tabPanel("Review Grades Roster and Update", 
               uiOutput("dynamic_header_assignment"),  # Placeholder for the reactive header
               p("Check the data table below reflects the update you will push then click the 'Update Grades' button."),
               actionButton("gs_updategrades_button", "Update Grades",style = "width: 400px;"),
               br(),
               h3("Review Entries to be uploaded to Canvas"),
               p("Check the data table below reflects the update you will push then click the 'Update Grades' button'."),
        DTOutput("gs_updategrades_table"),
      br(),
      
      ),
    
    
    tabPanel("Review Duplicate Assignments",
      h4("Review duplicate Assignment info"),
      p("If you see duplicate assignments in the table below, you will need to rectify this if your chosen assignment is listed."),
      p("To fix you will need to remove the duplicated assignment in the listed Canvas course so there is only one."),
      DTOutput("duplicate_assignments_table")
    )
  )
)
}

# Server ----


gs_updategrades_server <- function(input, output, session,gs_data,gs_wizard_status){
  
  # Reactives ----
  df_assignments <- reactiveVal()
  
  upload_roster <- reactiveVal()
  
  duplicate_assignments <- reactiveVal()
  
  #
  # Load Assignments ----
  #gs_loadassignments_button
  observeEvent(input$gs_loadassignments_button, {
    #browser()
    # DEBUG:
    #gs_data <- readRDS("test/gs_data_upload_test.rds")
    
    req(gs_data$upload_roster)
    df <- gs_data$upload_roster
    df2 <- df %>% select(course_id,
                         section_id,
                         user_id,
                         user.sortable_name,
                         instructor,
                         section_hour,
                         gs_data$score_column,
                         gs_data$max_column)
    
    upload_roster(df2) 
    
    #get the course list from the data 
    # TODO: fix naming
    #course_list <- unique(df2$course_id)
    df2 <- df2 %>% rename(id = course_id)
    #get the assignments from the course list
    df_assignment_names <- get_like_assignments(df2)
    
    
    # find if there are duplicate names with the same course id
    
    #filter by each unique course_id and check if there are duplicates in the name
    
    df_duplicates <- df_assignment_names %>% group_by(course_id) %>% 
      filter(duplicated(name) | duplicated(name, fromLast = TRUE)) %>%
      arrange(course_id, name)
    duplicate_assignments(df_duplicates)
    
    if(nrow(df_duplicates) > 0){
      showModal(modalDialog(
        title = "Duplicate Assignment Names Detected",
        p("The following assignments have the same name in the same course. 
          You will need to rectify this if the assignment you want to update is listed."),
        renderTable({
          duplicate_assignments()
        }),
        easyClose = TRUE
      ))
    }
    
    #TODO: Fix this naming convention
    #store into a reactive
    df_assignments(df_assignment_names)
    
    
  })
  # Render duplicates table (optional, outside modal)
  output$duplicate_assignments_table <- DT::renderDataTable({
    duplicate_assignments()
  })
  
  ## Assignment select ----
  output$gs_assignmentselect_UI <- renderUI({
    
    req(df_assignments())
    #browser()
    
    #sort assignment_names alphabetically
    assignment_names <- unique(df_assignments()$name)
    assignment_names <- sort(assignment_names)
    
   div(
      selectInput(
      inputId = "gs_assignmentselect",
      label = "Assignment to update grades:",
      choices = assignment_names,
      selected = NULL)
    )
    })

  #Updates ----
  ## confirm modal ----
  observeEvent(input$gs_updategrades_button,{
    #browser()

      showModal(modalDialog(
        title = "Confirm Upload",
        "Did you check for duplicates and check the data looked correct? \n 
        'Yes' to proceed with upload, 'No' to exit and return to app.",
        footer = tagList(
          actionButton("gs_ConfirmUpload", "Yes"),
          actionButton("gs_DenyUpload", "No")
        )
      ))
      
      
    })
  
  ##update grade logic ----
    observeEvent(input$gs_ConfirmUpload, {
      removeModal()
      #DEBUG:
      #browser()
      #gs_data <- readRDS("test/gs_data_upload_test.rds")
      df <- upload_roster()
      assignment_name <- input$gs_assignmentselect
      df_assignments <- df_assignments()
      
      #I need to get the 'id' from df_assignments based on the assignment_name
      df_assignments <- df_assignments %>% filter(name %in% assignment_name) %>%select(id,course_id)
      
      #left join df with df_assignments by course_id
      df2 <- left_join(df,df_assignments, by = "course_id")
      
      grades_df <- df2 %>% select(user_id,gs_data$score_column,course_id,id) %>%
        rename(assignment_id=id,
               points = gs_data$score_column)

      
      
      resp <- lapply(split(grades_df, grades_df$course_id), grade_assignments_bulk)
      
      #resp is a string display it using a modal so user can see if it worked
      resultVector <- unlist(resp)
      concatenatedResult <- paste(resultVector, collapse = "<br/>")
      
      showModal(modalDialog(
        title = "Upload Status",
        HTML(concatenatedResult), # Use HTML() to interpret line breaks
        footer = modalButton("Close")
      ))
    })
    
    observeEvent(input$gs_DenyUpload, {
      removeModal()
    })
    
    # UI Reactives ----
    
    ## UI for unpublished assignments ----
    # Render UI if there are unpublished assignments
    output$unpublishedassignments_ui <- renderUI({
      req(df_assignments())
      
      # Check for unpublished assignments
      unpublished <- df_assignments() %>% filter(!published)
      
      if (nrow(unpublished) > 0) {
        # Reactive UI to notify about unpublished assignments
        div(
          style = "color: red; margin-top: 20px;",
          h4("Warning: Some assignments are unpublished!"),
          p("The following assignments are unpublished:"),
          renderTable(unpublished),
        )
      }
    })
  
    ## Dynamic Header for Assignment ----
    output$dynamic_header_assignment <- renderUI({
      req(input$gs_assignmentselect)  # Ensure the input is not NULL
      h3("Update grades for:", input$gs_assignmentselect)
    })
    
    ## Grades roster display ----
    
    output$gs_updategrades_table <- DT::renderDataTable({
      #browser()
      req(upload_roster())
      upload_roster()
      
    })
}
