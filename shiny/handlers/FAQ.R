
#### UI ####

createFAQPage <- function() {
  div(
    h3("Frequently Asked Questions"),
    tags$ol(
      tags$li("Why does my app keep crashing?"),
      tags$p(
        "The developer is a pretty terrible coder and also has limited data sets to test with. If you are experiencing routine crashes, contact him at: ",
        tags$a(href = "mailto:kevin.filip@westpoint.edu", "kevin.filip@westpoint.edu"),
        "or",
        tags$a(href= "mailto:kevin.t.filip.mil@army.mil", "kevin.t.filip.mil@army.mil"),
        "or call/text 630-391-3587 at any hour of the day, I'll help!"
      ) 
    ),
    HTML("<b>Still Need Help?</b>"),
    tags$a("Try seeking further information here", 
           href = "https://www.cnn.com/interactive/2019/06/us/dad-joke-generator-trnd/", 
           target = "_blank",style="text-align:center"),
    
  )
  
}
