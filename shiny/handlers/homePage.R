
#### UI ####

createHomePage <- function() {
  fluidRow(
    column(width = 7,
           h3("Welcome to PANE Grade Report Generator! Now with automatic updates!"),
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
