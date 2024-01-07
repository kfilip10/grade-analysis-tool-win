# app.R
# Purpose: This is the main file for the Grade Report Generator. It is the file that is run to start the application.
# Started with a simple template from https://shiny.posit.co/r/gallery/start-simple/faithful/
# Modularized over time to make it easier to maintain and add new features piece by piece
# Structure: 
# - ui calls functions in the handlers folder to create the UI
# - server calls corresponding functions in the handlers folder which passes the session to communicate back and forth
# - Each handler script includes its UI and server functions
# - shinyApp(ui = ui, server = server) launches the app
# Some Resources for shiny:
# - https://mastering-shiny.org/index.html
# - https://shiny.rstudio.com/tutorial/


#### Function to load packages ####
req <- scan(file.path(getwd(), "req.txt"), character(), quiet = T)#This was how it was in standalone
#invisible so no return/console output
#lapply - apply function to list (it applies the library function to the list of packages in req, which is the req.txt file)
# character.only = T - only use the character strings in the list, not the other stuff
invisible(lapply(req, library, character.only = T))

#### Custom CSS example, .sidebar isn't used but container for use later#### 
customCSS <- function() {
  tags$head(
    tags$style(
      HTML(
        "
        /* Custom CSS to change font size of sidebar */
        .sidebar {
          font-size: 18px; /* Change font size as needed */
        }
        "
      )
    )
  )
}

#### Load Global Variables and some setup ####
source('global.R') #loads 'global.R' file into environment, there may be a better way to do this but this works for now
sourceAllFilesInFolder(file.path(getwd(),"handlers")) #loads the files in the handlers folder
sourceAllFilesInFolder(file.path(getwd(),"functions")) #loads the files in the functions folder
sourceAllFilesInFolder(file.path(getwd(),"functions","canvas api")) #loads the files in the functions folder

#Changes max file upload size to 30 MB
options(shiny.maxRequestSize = 30*1024^2)
options(spinner.type = 6, spinner.color = "#cfbb34")
#Runs the function to check if the grade csv exists and also makes the settings folder

####Load UI####
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  useShinyjs(), #shiny JS used for hiding/showing elements and enables html formatting https://deanattali.com/shinyjs/overview#demo
  #navbarPage - creates a navbar at the top of the page
  navbarPage(
    id = "navbarID",
    #title and logo. Logo won't load when launching from R Studio, works in deployed app
    title=div( 
      img(src = "PANE.png", height = "35px", width = "30px", style = "margin-right: 10px;"),
      "Grade Report Generator"
    ),
    #Makes the home page. createHomePage() is in the handlers folder
    tabPanel("Home",icon = icon("home"),  createHomePage() #icons from: https://www.w3schools.com/bootstrap/bootstrap_ref_comp_glyphs.asp
    ), 
    #Prep and Canvas Data
    tabPanel("Prep and Canvas Data",
             value = "prep_panel",
             uiOutput("dynamicUI"),
             icon = icon("cloud",lib="glyphicon")
    ),
    #Makes the pre-WPR data prep page. createPreWPRPage() is in the handlers folder
    tabPanel("Pre-WPR Prep",icon = icon("list-alt",lib="glyphicon"),createPreWPRPage()
    ),
    #Makes the WPR Analysis and Brief page. This page within the navbar has multiple tabs (tabsetPanel handles those)
    tabPanel("WPR Analysis and Brief", icon = icon("stats",lib="glyphicon"), # Using a Shiny icon for comparison
      tabsetPanel(
        tabPanel("Upload Data", createBriefUploadPage()
        ),
        tabPanel("Create Brief",createBriefPage()
        ),
        #this is for making the distribution plots, as this becomes more robust it will become a standalone function/module
        tabPanel("Version Grade DistributionPlots",
                 div(               
                   h2(HTML("<b>Plot Parameters</b>"), style="text-align:left"),
                   uiOutput("dynamic_inputs")
                 ),
                 verbatimTextOutput("summary"),
                 plotOutput("barplot")
        ),
        #This is a tabular view of the version summary data
        tabPanel("Version Summary",
                 tableOutput("versionTable"),
                 verbatimTextOutput("Excel Viewer")
        )
        
      ),
    ),
    #Post Brief OML is not integrated yet
    #tabPanel("Post Brief OML",icon=icon("list"),createOMLPage()),
    
    #Brief without Canvas is buggy as of 6JAN. Need to make it more robust
    tabPanel("Brief without Canvas",icon=icon("warning-sign",lib="glyphicon"),createManualPage()),
    #Settings page which will write to a folder defined in global.R
    tabPanel("Settings",icon=icon("cog"),createSettingsPage()
    ),
    #FAQ page which is sparse as of 6JAN
    tabPanel("FAQ",icon=icon("question"),createFAQPage(),
             )
  ),
  # Custom CSS to change font size of the bin_width but that is removed for now.
    # tags$head(
  #   tags$style(HTML("
  #     /* Custom CSS to resize numeric input */
  #     #bin_width[type='number'] {
  #       width: 50px; /* Change the width to your desired value */
  #     }
  #   "))
  # ),
)

# Server
server <- function(input, output, session) {
  
  
  output$dynamicUI <- renderUI({
  if (input$navbarID == "prep_panel") {
    createCanvasPrepPage()
    
  }
  })
  # Check if the Canvas API token exists
  canvas_api_token <- reactiveVal()
  
  observeEvent(input$navbarID, {
    if (input$navbarID == "prep_panel") {
      # Code to update canvas_api_token
      if (file.exists(canvas_api_token_path)) {
        canvas_api_token(readRDS(canvas_api_token_path))
      } else {
        canvas_api_token(NULL)
      }
    }
  })
  
  canvasPrep_Handler(input, output, session, canvas_api_token)
  
  # Pre WPR Prep logic and display
  preWPR_Handler(input, output, session)
    
  # Settings logic and display
  settings_Handler(input, output, session)

  # Brief logic and display
  brief_Handler(input, output, session)
  
  #Manual brief logic and display
  brief_Manual_Handler(input, output, session)
  
  #### Task Kill ####
  session$onSessionEnded(function() {
    stopApp()
    # you can put your code here to close the connection
  })
  

}



shinyApp(ui = ui, server = server)


