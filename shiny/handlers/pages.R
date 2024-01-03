

createHomePage <- function() {
  fluidRow(
    column(width = 7,
    h3("Welcome to PANE Grade Report Generator! Now with automatic Updates!"),
    tags$hr(),
    tags$p("There are two ways to make a grade report, both of which result in a nearly identical grades brief"),
    tags$ol(
      tags$li("The first is to use the 'WPR Analysis and Brief' tab to generate a powerpoint. This relies on usage of the 
              Pre-WPR Prep tab and a canvas gradebook to automate some of the process."),
      tags$li("The second is to use the 'Brief Without Canvas' tab to generate the powerpoint brief 
      using a more rudimentary grading template that does not rely on a canvas gradebook."),
    ),
    h4("Steps to create a brief using your canvas gradebook:"),
    tags$ol(
      tags$li("Before grading your WPR go to the 'Pre-WPR Prep' tab and upload your canvas gradebook(s) to create a grading template."),
      tags$li("Grade your WPR using that template. Do not make changes to the format of the template."),
      tags$li("Go to the 'WPR Analysis and Brief' tab:"),
          tags$ol(
            tags$li("In the Upload data tab: Upload your WPR Grade Data Template. Optionally you can upload a cut sheet as well."),
            tags$li("In the Create Brief tab: Type in your course name, event name, and select a bin width for the histograms in the brief."),
            type="a"
          ),
      tags$li("Download your briefing and review for completeness."),
    ),
    tags$hr(),
    h4("Tab details:"
    ),
        h5("1. Pre-WPR Prep"
    ),
        tags$p("This page is for generating the template to fill in your WPR grades based on your Canvas Gradebook. 
           You can upload your excel file and cut sheet here. 
           You can also select which questions you want to include in the report."
    ),
        h5("2. WPR Analysis and Brief"),
      tags$p("This page is for generating the powerpoint brief. 
           You can upload your excel file and cut sheet here.
             NOTE: It is possible to run the report without pre/post points data, prepare your excel to have a '1' in all pre/post points cells"
      ),
    h5("3. Post Brief OML"
    ),
    tags$p("This page creates the post brief OML using a canvas gradebook (with WPR data) and a course 'Graybook' absence report"
    ),
      h5("4. Make Brief Without Canvas Gradebook"
      ),
      tags$p("This tab is for generating the powerpoint brief using a simplifed excel only format. 
           You can upload your excel file and cut sheet here.
             NOTE: It is possible to run the report without pre/post points data, prepare your excel to have a '1' in all pre/post points cells"
      )
    
  ) 
  )
}


createOMLPage <- function() {
  div(
    style=bodystyle,
    h3("OML Preparation"),
    tags$p("This tab takes your course canvas gradebook(s) after uploading the WPR and creates the OML (Work in progress).")
    
  )
  
  
}

createBackupPage <- function() {
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

createFAQPage <- function() {
  div(
    h3("Frequently Asked Questions"),
    tags$ol(
      tags$li("How do I get the gradebook data from Canvas?"),
      tags$par("Navigate to the grades page and click 'export gradebook'. Do not alter the .csv file generated."),
      tags$li("I forgot to get the gradebook before the WPR, what can I do?"),
      tags$par("If you have a current gradebook run the report using the current gradebook. Then you will have to manually
               deduct the WPR total from the 'Pre max' column and the Student's total from the 'Pre points' column. 
               Then follow the usual instructions to create your brief."),
    ),
    HTML("<b>Still Need Help?</b>"),
    tags$a("Try seeking further information here", 
           href = "https://www.cnn.com/interactive/2019/06/us/dad-joke-generator-trnd/", 
           target = "_blank",style="text-align:center"),
    
  )
  
}