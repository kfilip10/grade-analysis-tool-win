

#### UI ####
createManualPage <- function() {
  div(
    style=bodystyle,
    h3("Manual backup of the PANE Grade Report Generator!"),
    tags$p("This tab is for a non-canvas related grade upload. 
               It uses a standalone excel template to still generate the brief in the event of no access to Canvas, a bug in the canvas tool,
               or just preference by the user."),
    h4("Instructions:"),
    tags$ol(
      tags$li("Download the template below"),
      downloadButton("download_template_manual", "Download Template Format in Excel"),
      tags$li(textInput(inputId="courseTitleManual", label=HTML("<b>Type the course name</b>"), value = "PHYS ")),
      tags$li(textInput(inputId="eventTitleManual", label=HTML("<b>Type the event name</b>"), value = "WPR ")),
      fileInput("excelFileManual", HTML("<b>Upload the Excel with the WPR Grades</b>"), accept = c(".xlsx")),
      fileInput("cutSheetManual",HTML("<b>Upload the PDF Cut Sheet </b> <br>
                                (Leave blank if no cut sheets are desired in final brief) "), accept = c(".pdf")),
      #tags$li(selectInput("sortValueManual", label = "Select which method of sorting questions:",
      #                    choices = c("Group By Concept", "Display in Question Order"),
      #                    selected = "Display in Question Order")),
      tags$li(numericInput("bin_width_manual", "Select a grade bin width for histograms:", 
                           value = 5, min = 0, max = 10)),
      tags$li(disabled(actionButton("convertBtnManual", "Create Grade Brief",style = "background-color: #3498db; color: #ffffff;")),
      ),
      HTML("<p></p><br><p></p>"),
      tags$li(disabled(downloadButton("pptDownloadManual", "Download Grade Brief (PPT)",style = "background-color: #3498db; color: #ffffff;"))),
      tags$li(disabled(downloadButton("downloadCanvasEntry", "Download data for canvas Entry (.xlsx)",style = "background-color: #3498db; color: #ffffff;")))
    ),
  )
  
}

#### Server ####

brief_Manual_Handler <- function(input, output, session){

#### Manual Backup ####
# If manual backup is true then run the normal ppt.R function 
#(I'll call it manual.R)
grades.list.manual <- reactiveValues(dataframes=list())
num_versions_manual <- reactiveVal(0)

output$download_template_manual <- downloadHandler(
  filename = function() {
    file_name <- "Report Test.xlsx"  # Filename for the backup file
    file_name 
  },
  content = function(file) {
    file_path <- file.path(getwd(),'www', "Report Test.xlsx")      
    file.copy(file_path, file)
  })

course_title_manual <- reactive({
  input$courseTitleManual
})

event_title_manual <- reactive({
  input$eventTitleManual
})

sort_style_manual <- reactive({
  #"Group By Concept" or "Display in Question Order"
  input$sortValueManual
})

#### Cut sheet reactive ####
cutSheetManual <- reactive({
  #req(input$cutSheet)
  if (!isTruthy(input$cutSheetManual))
    return(NULL)
  inFile2 <- input$cutSheetManual
  pdf <- magick::image_read_pdf(inFile2$datapath)
  readBin(input$cutSheetManual$datapath, "raw", n = file.info(input$cutSheetManual$datapath)$size)
  return(pdf)
})
#### Excel Reactive ####

observeEvent(input$excelFileManual, {
  req(input$excelFileManual$datapath)
  sheets <- excel_sheets(input$excelFileManual$datapath)
  
  
  # Store the number of sheets in a reactive variable for further use
  num_versions_manual(length(sheets))
  
  
  #load df.total and df.g here
  # df.grades is list of two dataframes (df.total and df.questions)
  
  tryCatch({
    
    list.df <- list()
    versions <- excel_sheets(input$excelFileManual$datapath)
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
        list.df[[n]] <- read_excel(input$excelFileManual$datapath,sheetN)}  
    }
    df.list <- import_excel_manual(list.df,length(versions))
    grades.list.manual$dataframes <- df.list
    
    enable("convertBtnManual")
    
  },
  error = function(e) {
    showErrorModal(paste("Error:", e$message))
  })
  
  
})

observeEvent(input$convertBtnManual, {
  req(input$excelFileManual$datapath)
  tryCatch({
    
    withProgress(message = 'Generating brief', value = 0,min=0,max=1, {
      pdf <- cutSheetManual()
      
      #Call function below
      # Call the make_ppt function with the input values
      
      bin.width <-input$bin_width_manual #Update this to a user input 
      
      df.tot <- grades.list.manual$dataframes[[1]]
      df.q <- grades.list.manual$dataframes[[2]]
      
      progress.tot <- nrow(df.q %>% group_by(version,question) %>% summarise(n()))+
        nrow(df.q %>% group_by(version) %>% summarise(n()))+4
      
      incProgress(1/progress.tot, detail = paste("Calling Function"))
      
      
      ppt <- manual_ppt(df.tot, df.q,course_title_manual(), event_title_manual(), num_versions_manual(), 
                        pdf, bin.width, "Display in Question Order",progress.tot) #,
      
      enable("pptDownloadManual")
      setProgress(1, detail = paste("Complete!"))
    })
    # After the task is finished, display a modal
    showModal(modalDialog(
      title = "Brief Created",
      "The brief has been created and is available for download on the 'Create Brief' Tab.",
      footer = modalButton("Close")
    ))
  },
  error = function(e) {
    showErrorModal(paste("Error:", e$message))
  }
  )
  # Provide download link
  output$pptDownloadManual <- downloadHandler(
    filename = function() {
      "Grade Report.pptx"
    },
    content = function(file) {print(ppt,target= file)
    }
  )
})

}