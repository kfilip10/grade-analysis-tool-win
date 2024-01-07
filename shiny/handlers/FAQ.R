
#### UI ####

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