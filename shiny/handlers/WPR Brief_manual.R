


briefManual_Handler <- function(input, output, session){

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