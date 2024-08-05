
#### UI ####

createHomePage <- function() {
  fluidRow(
    column(width = 7,
           h3("Welcome to PANE Grade Report Generator! Now with automatic updates!"),
           tags$hr(),
           tags$p("This app now has Canvas API integration, allowing you to generate your WPR briefs without having to download/upload your gradebook!"),
      h4("Steps to create a brief using this app:"),
      tags$ol(
        tags$li("On First launch you will need to configure yout Canvas API token. Click on the 'Settings' tab and follow the instructions."),
        tags$li("Go the the Canvas Access Tab, select your courses, and select the assignment you want to grade."),
        tags$li("Here you can download the WPR grading template for one assignment or you could select multiple assignments and download a consolidated gradebook."),
        tags$li("Grade your WPR using that template. Do not make changes to the format of the template."),
        tags$li("Go to the 'WPR Analysis and Brief' tab:"),
        tags$ol(
          tags$li("In the Upload data tab: Upload your WPR Grade Data Template. Optionally you can upload a cut sheet as well."),
          tags$li("In the Create Brief tab: Type in your course name, event name, and select a bin width for the histograms in the brief."),
          type="a"
        ),
        tags$li("Download your briefing and review for completeness.")      ),
      # tags$hr(),
      # h4("Tab details:"
      # ),
      # h5("1. Pre-WPR Prep"
      # ),
      # tags$p("This page is for generating the template to fill in your WPR grades based on your Canvas Gradebook. 
      #      You can upload your excel file and cut sheet here. 
      #      You can also select which questions you want to include in the report."
      # ),
      # h5("2. WPR Analysis and Brief"),
      # tags$p("This page is for generating the powerpoint brief. 
      #      You can upload your excel file and cut sheet here.
      #        NOTE: It is possible to run the report without pre/post points data, prepare your excel to have a '1' in all pre/post points cells"
      # ),
      # h5("3. Post Brief OML"
      # ),
      # tags$p("This page creates the post brief OML using a canvas gradebook (with WPR data) and a course 'Graybook' absence report"
      # ),
      # h5("4. Make Brief Without Canvas Gradebook"
      # ),
      # tags$p("This tab is for generating the powerpoint brief using a simplifed excel only format. 
      #      You can upload your excel file and cut sheet here.
      #        NOTE: It is possible to run the report without pre/post points data, prepare your excel to have a '1' in all pre/post points cells"
      # )
      
    ) 
  )
}
