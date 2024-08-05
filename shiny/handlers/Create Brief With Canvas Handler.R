
#### UI ####

createBriefUploadPage <- function() {
  div(
    style=bodystyle,
    h3("1. Upload Grade data:"),
    tags$p("Upload the WPR template file that you created in the 'Pre-WPR Prep' tab."),
    fileInput("WPRGrades", HTML("<b>WPR Grade Data (.xlsx)</b>"), accept = c(".xlsx")),
    fileInput("cutSheet",HTML("<b>Cut Sheet (.pdf) (optional) </b> <br>
                                      (Leave blank if no cut sheets are desired in final brief) "), accept = c(".pdf")),
    h4("1a. Select the Excel sheets which correspond to your WPR data."),
    tags$p("NOTE: The version numbering in the brief will conform to the 
       order of the sheets listed below (i.e. first checked version is version 1, second v2, etc.)"),
    uiOutput("checkboxes"),
    h3("2. Load excel data:"),
    disabled(actionButton("importWPRExcelBtn", "Load Excel Data",style = "background-color: #3498db; color: #ffffff;")),
    h3("2a. Duplicate Entries"),
    p("The following duplicate entries (one cadet with grades in multiple versions:) were found that you should investigate:"),
    DTOutput("duplicateTable"),
    h3("2b. Cadets with no Exam Entry"),
    DTOutput("noEntryTable"),
    
    )
  
  
}

createBriefPage <- function(){
  div(
    h3("3. Select Parameters"),
    textInput(inputId="courseTitle", label=HTML("<b>Course name:</b>"), value = "PHYS 201"),
    textInput(inputId="eventTitle", label=HTML("<b>Event name:</b>"), value = "WPR "),
    #removed the bin_width parameter because it wasn't totally needed, left here in case we want it back.
    #numericInput("bin_width", "Select a grade bin width for histograms:",
    #             value = 5, min = 4, max = 10), 
    h3("4. Create the Grade Brief"),
    p("Note: You must upload an excel file first"),
    disabled(actionButton("convertBtn", "Create Grade Brief",style = "background-color: #3498db; color: #ffffff;")),
    h3("5. Download the Grade Brief"),
    disabled(downloadButton("pptDownload", "Download Grade Brief (PPT)",style = "background-color: #3498db; color: #ffffff;"))
  )
}

#### Server ####


brief_Handler <- function(input, output, session){
  
  #### Create Brief ####
  grades.list <- reactiveValues(dataframes=list())
  
  num_versions <- reactiveVal(0)
  #selected_sheets <- reactiveVal(NULL) #Don;t think it is used? 9DEC
  
  
  course_title <- reactive({
    input$courseTitle
  })
  
  event_title <- reactive({
    input$eventTitle
  })
  
  #### Upload Excel ####
  
  wprGrades.path <- reactiveVal(NULL)
  
  observeEvent(input$WPRGrades, { 
    req(input$WPRGrades$datapath)
    wprGrades.path(input$WPRGrades$datapath)
    enable("importWPRExcelBtn")
    
  })
  
  #### Version Check box ####
  output$checkboxes <- renderUI({
    req(wprGrades.path())
    
    inFile <- wprGrades.path()
    
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
  
  
  
  
  #### Import WPR Data  Button####
  #Import WPR Data button
  #Takes the sheet list of selected versions and uses that to select the excel sheets
  #which are used to parse the excel data in
  observeEvent(input$importWPRExcelBtn, {
    req(wprGrades.path())
    
    sheets <-  input$WPR_sheets #sheets list from the checkboxes
    #remove any sheets with "instructions" in the name
    # Store the number of sheets in a reactive variable for further use
    num_versions(length(sheets))
    
    #load df.total and df.g here
    # df.grades is list of two dataframes (df.total and df.questions)
    tryCatch({
      
      list.df <- list()
      versions <- sheets
      #versions <- tolower(versions)
      for(n in 1:length(versions)){
          list.df[[n]] <- read_excel(wprGrades.path(),sheet=versions[n]) 
      }
      df.list <- import_WPR_excel(list.df,num_versions())
      grades.list$dataframes <- df.list

      showModal(modalDialog(
        title = "Import Successful",
        HTML(" You may now create the brief from the next tab or view plot data."),
        footer = modalButton("Close")
      ))
      enable("convertBtn")
      
    },
    error = function(e) {
      showErrorModal(paste("Error:", e$message))
    })
    
    #deprecated code to group questions in the brief by concept (didn't work well)
    output$quesConcCompare <- renderText({
      numquestions <- df.list[[2]] %>% group_by(version) %>% summarise(questions = n_distinct(question))
      
      numConcepts <- df.list[[2]] %>% summarise(questions = n_distinct(concept))
      
      question.Conc.Comp <- !any(FALSE %in% apply(numquestions,1,function(x)
        if(x[2]==numConcepts[1]){
          return(TRUE)
        }
        else{
          print(FALSE)
        }))
      
      if(question.Conc.Comp){
        return(str_c("There are ", numConcepts[1], " unique concepts and each version has that many questions. 
              It is recommended to group by concept"))
      }else{
        return(str_c("There are ", numConcepts[1], " unique oncepts which exceeds the number of questions. 
              It is recommended to group by question."))
      }
      
    })
    
  })
  
  #### Plot: version selection ####
  output$dynamic_inputs <- renderUI({
    req(input$WPRGrades$datapath)
    #df.q2 <- grades.list$dataframes[[2]]
    selectizeInput("version", "Version", choices = unique(grades.list$dataframes[[1]]$version), selected = NULL)
  })
  
  #### Plot: Barplot ####
  output$barplot <- renderPlot({
    req(input$WPRGrades, input$version)
    df <- grades.list$dataframes[[1]]
    df$mge.grade <- factor(df$mge.grade, grades)
    
    df%>%filter(version==input$version)%>%count(mge.grade)%>%mutate(label=paste(n))%>%
      ggplot(aes(x=factor(mge.grade),y=n)) + 
      geom_col(width=0.7) +
      geom_text(
        aes(y=-4,label = label),
        vjust=0,
        color="black"
        ,size=7,
        nudge_y=0)+
      theme_hc()+
      theme(text = element_text(size = 18))+
      labs(title = str_c(input$version," Grades Distribution"),x="",y="Count")
    
  })
  
  #### Render Tables ####
  output$versionTable <- renderTable({
    req(input$WPRGrades$datapath)
    df.vers.sum <-grades.list$dataframes[[1]] %>% 
      group_by(version) %>% summarise("Count"=n(),
                                      "Mean (%)"=round(mean(mge.percent),1),
                                      "Median (%)"=round(median(mge.percent),1),
                                      "Std. Dev (%)"=round(sd(mge.percent),1),
                                      "# D/F's"=sum(mge.grade=="D"|mge.grade=="F"),
                                      "# A's"=sum(mge.grade=="A"|mge.grade=="A+"|mge.grade=="A-"))
    print(df.vers.sum)
    #head(grades.list$dataframes[[1]])
  })

  
  output$duplicateTable <- renderDT({
    req(!is.null(grades.list$dataframes), length(grades.list$dataframes) >= 3)
    if (!is.null(grades.list$dataframes)) {
      datatable(grades.list$dataframes[[3]],options=list(
        pageLength = 10,  # Set initial number of entries to display
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
  
  
  output$noEntryTable <- renderDT({
    req(!is.null(grades.list$dataframes), length(grades.list$dataframes) >= 3)
    if (!is.null(grades.list$dataframes)) {
      datatable(grades.list$dataframes[[4]],options=list(
        pageLength = 10,  # Set initial number of entries to display
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
    
    
    
    
  #### Cut sheet reactive ####
  cutSheet <- reactive({
    #req(input$cutSheet)
    if (!isTruthy(input$cutSheet))
      return(NULL)
    inFile2 <- input$cutSheet
    pdf <- magick::image_read_pdf(inFile2$datapath)
    readBin(input$cutSheet$datapath, "raw", n = file.info(input$cutSheet$datapath)$size)
    return(pdf)
  })
  
  
  sort_style <- reactive({
    #"Group By Concept" or "Display in Question Order"
    input$sortValue
  })
  
  
  #### Convert PPT ####
  observeEvent(input$convertBtn, {
    req(input$WPRGrades$datapath)
    tryCatch({
      
      withProgress(message = 'Generating brief', value = 0,min=0,max=1, {
        pdf <- cutSheet()

        #Call function below

        #bin.width <-input$bin_width #Update this to a user input if we uncomment from UI function
        bin.width <- 5
        
        
        #ppt <- make_ppt(grades.list$dataframe[[1]], grades.list$dataframe[[2]],course_title, event_title, num_versions, pdf, bin.width)
        title <- course_title
        df.q <- grades.list$dataframes[[2]]
        progress.tot <- nrow(df.q %>% group_by(version,question) %>% summarise(n()))+
          nrow(df.q %>% group_by(version) %>% summarise(n()))+4
        incProgress(1/progress.tot, detail = paste("Calling Function"))
        sortStyle <- "Display in Question Order"
        ppt <- make_ppt(grades.list$dataframes,course_title(), event_title(), pdf, bin.width,sortStyle,progress.tot) #,
        
        enable("pptDownload")
        setProgress(1, detail = paste("Complete!"))
      })
      # After the task is finished, display a modal
      showModal(modalDialog(
        title = "Brief Created",
        "The brief has been created and is available for download!",
        footer = modalButton("Close")
      ))
    },
    error = function(e) {
      showErrorModal(paste("Error:", e$message))
    }
    )
    
    #### Download Brief ####
    output$pptDownload <- downloadHandler(
      filename = function() {
        "Grade Report.pptx"
      },
      content = function(file) {print(ppt,target= file)
      }
    )
    
    #### Download Canvas Entry ####
    output$downloadCanvasEntry <- downloadHandler(
      filename = function() {
        "Canvas Entry.csv"
      },
      content = function(file) {
        #df.csv <- grades.list$dataframes[[1]] %>% select(ID,Name,Section,mge.points)
        write.csv(df.total,file)
        print(ppt,target= file)
      }
    )
  })
  
  
  
  
  
}
