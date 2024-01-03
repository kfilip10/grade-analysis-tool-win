# Load necessary libraries
#req <- scan(file.path(dirname(getwd()), "req.txt"), character(), quiet = T)
req <- scan(file.path(getwd(), "req.txt"), character(), quiet = T)#This was how it was in standalone
invisible(lapply(req, library, character.only = T))

#### Function to load packages ####

#### globals ####
#source(file.path(getwd(),"app","ppt.R"))
source('global.R')
handler_path <- file.path(wd,"handlers")
sourceAllFilesInFolder(handler_path)

function_path <- file.path(wd,"functions")
sourceAllFilesInFolder(function_path)


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



#### UI####
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  useShinyjs(),
  navbarPage(
    title=div(
      img(src = "PANE.png", height = "35px", width = "30px", style = "margin-right: 10px;"),
      "Grade Report Generator"
    ),
    #span( "Grade Report Generator", style = "background-color: #DEEBF7", style="color:red" ),    
    
    tabPanel("Home",icon = icon("home"),  createHomePage()
    ),
    
    tabPanel("Pre-WPR Prep",icon = icon("list-alt",lib="glyphicon"),createPreWPRPage()
             ),
    
    tabPanel("WPR Analysis and Brief", icon = icon("stats",lib="glyphicon"), # Using a Shiny icon for comparison
      tabsetPanel(
        tabPanel("Upload Data", createBriefUploadPage()
        ),
        tabPanel("Create Brief",createBriefPage()
        ),
        tabPanel("Version Grade DistributionPlots",
                 div(               
                   h2(HTML("<b>Plot Parameters</b>"), style="text-align:left"),
                   uiOutput("dynamic_inputs")
                 ),
                 verbatimTextOutput("summary"),
                 plotOutput("barplot")
        ),
        tabPanel("Version Summary",
                 tableOutput("versionTable"),
                 verbatimTextOutput("Excel Viewer")
        )
        
      ),
    ),
    tabPanel("Post Brief OML",icon=icon("list"),createOMLPage()),
    tabPanel("Brief without Canvas",icon=icon("warning-sign",lib="glyphicon"),createBackupPage()),
    tabPanel("Settings",icon=icon("cog"),createSettingsPage()
    ),
    tabPanel("FAQ",icon=icon("question"),createFAQPage(),
             )
  ),
  #titlePanel("Grade Brief Generator"),  
  tags$head(
    tags$style(HTML("
      /* Custom CSS to resize numeric input */
      #bin_width[type='number'] {
        width: 50px; /* Change the width to your desired value */
      }
    "))
  ),
)

# Server
server <- function(input, output, session) {

  
  
  # Pre WPR Prep logic and display
  preWPR_Handler(input, output, session)
    
  # Settings logic and display
  settings_Handler(input, output, session)

  # Brief logic and display
  brief_Handler(input, output, session)
  
  #Manual brief logic and display
  briefManual_Handler(input, output, session)
  
  #### Task Kill ####
  session$onSessionEnded(function() {
    stopApp()
    # you can put your code here to close the connection
  })
  

}


shinyApp(ui = ui, server = server)

