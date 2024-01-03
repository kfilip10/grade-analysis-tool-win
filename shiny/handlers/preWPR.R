createPreWPRPage <- function() {
  fluidRow(
    column(width = 6,
           # Content for the left side
           h3("Grading Template Generator"),
           p("This side will generate the grading template based on the values in the 
           right column and the number of versions of your exam."),
           #numericInput("versionsTemplate", "How many versions will your test have?", 
           #            value = 1, min = 1, max = 10,width="400px"),
           tags$p("Upload all canvas gradebooks from before the WPR/TEE. For decentralized canvas courses (PHYS201/202) you will need to select all sections at the same time."),
           fileInput("CanvasGradebook", HTML("<b>Upload all pre-WPR/TEE canvas gradebooks:</b>"), accept = c(".csv"), multiple = TRUE),
           disabled(actionButton("make_template_btn", "Make/update Grading Template")),
           HTML("<p></p><br><p></p>"),
           disabled(downloadButton("download_template_btn", "Download Grading Template")),
           p("After downloading the template copy the 'v1' sheet for the number of versions you have."),
           disabled(downloadButton("download_canvas_btn", "Download Combined Canvas Roster"))
    ),
    
    column(width = 6,
           # Content for the right side
           h3("Canvas Deafults"),
           p("These are the default gradebook identifiers that you shouldn't need to change.
           If the gradebook column names change you may need to update these values."),
           tags$p(HTML("<b> NOTE:</b> Default values only need to be changed if the canvas gradebook format has changed.")
           ),
           textInput(inputId="studentStr", 
                     label=HTML("Gradebook column name for student name:"), width="400px",value = "Student"),
           textInput(inputId="studentIDStr", 
                     label=HTML("Gradebook column name for student ID number:"), width="400px",value = "ID"),
           textInput(inputId="sectionstr", 
                     label=HTML("Section column name for  students current section"),width="400px", value = "Section"),
           textInput(inputId="currPointsStr", 
                     label=HTML("Gradebook column name for student current points:"), width="400px",value = "Current Points"),
           textInput(inputId="currScoreStr", 
                     label=HTML("Gradebook column name for  students current percent score"),width="400px", value = "Current Score"),
           numericInput("canvasrowNumStudents", "Select the excel row where the first student record appears.", 
                        value = 4, min = 1, max = 10,width="400px"),
    )
  )
}


preWPR_Handler <- function(input, output, session) {
  
#### Pre-WPR Template ####
#### Combining Canvas Gradebook - Prep page#### 
studentStr <- reactive({
  input$studentStr
})
studentIDStr <- reactive({
  input$studentIDStr
})
currPointsStr <- reactive({
  input$currPointsStr
})
currScoreStr <- reactive({
  input$currScoreStr
})
sectionstr <- reactive({
  input$sectionstr
})
canvasrowNumStudents <- reactive({
  input$canvasrowNumStudents
})
versionsTemplate <- reactive({
  input$versionsTemplate
})


observeEvent(input$CanvasGradebook, { 
  
  enable("make_template_btn")
  
})
templatePath <- reactiveVal(NULL)
combined_canvas_Path <- reactiveVal(NULL)

#tempDir <- reactiveVal(tempdir())

observeEvent(input$make_template_btn, {
  req(input$CanvasGradebook)
  files <- input$CanvasGradebook
  if (is.null(files))
    return(NULL)
  
  data <- lapply(files$datapath, import_canvas,student.str=studentStr(),
                 studentID.str=studentIDStr(),
                 section.str=sectionstr(),
                 currPoints.str=currPointsStr(), 
                 currScore.str=currScoreStr(),
                 rowStudents= canvasrowNumStudents())
  gb <- do.call(rbind, data)
  
  template.list <- prepare_template(gb, studentStr(), 
                                    studentIDStr(),
                                    sectionstr(), 
                                    currPointsStr(), 
                                    currScoreStr(),
                                    tempDir)
  #template.list has 1. the path, 2. string summary, 3. duplicate summary, 4. the gradebook
  template_path <- template.list[[1]]
  #Add a modal dialog to show the summary of the template
  showModal(modalDialog(
    title = "Gradebook Input Summary",
    HTML(paste0(template.list[[2]],"<br>","<br>",template.list[[3]],"<br>","<br>","You may close this dialog and download the template now from the template page.")),
    footer = modalButton("Close")
  ))
  enable("download_template_btn")
  enable("download_canvas_btn")
  templatePath(template_path)
  combined_canvas_Path(template.list[[4]])
})
#write combined_data to excel
#combined_data is a dataframe of the combined canvas gradebook
# 

# Downloads the template excel file based on the canvas gradebook
# This needs to take the combined excel and then download
output$download_template_btn <- downloadHandler(
  filename = function() {
    file_name <- "Grading Template.xlsx"  # Filename for the backup file
    file_name 
  },
  content = function(file) {
    file_path <- templatePath()     
    file.copy(file_path, file)
  })

output$download_canvas_btn <- downloadHandler(
  filename = function() {
    file_name <- "Canvas Gradebook.csv"  # Filename for the backup file
    file_name 
  },
  content = function(file) {
    file_path <- combined_canvas_Path()     
    file.copy(file_path, file)
  })
}