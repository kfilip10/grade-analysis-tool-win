# app.R
# lintr: disable
# Purpose: This is the main file for the Grade Report Generator.
# It is the file that is run to start the application.
# Modularized over time to make it easier to maintain
# Structure:
# - ui (below) calls functions in the handlers folder to create the UI
# - server calls corresponding functions in the handlers folder
# -  passes the session to communicate back and forth
# - Each handler script includes its UI and server functions
# - shinyApp(ui = ui, server = server) launches the app
# Some Resources for shiny:
# - https://mastering-shiny.org/index.html
# - https://shiny.rstudio.com/tutorial/


## Load Packages ----
req <- scan(file.path(getwd(), "req.txt"), character(), quiet = TRUE)
# This was how it was in standalone
# invisible so no return/console output

# character.only = T - only use the character strings in the list
invisible(lapply(req, library, character.only = T)) # nolint

check_internet_access <- function() {
  # Ping a reliable server (e.g., Google's DNS server)
  ping_result <- pingr::ping("8.8.8.8", count = 1, timeout = 1)
  
  # Check if the ping was successful
  return(!is.na(ping_result))
}

## Load Globals and setup ----
# loads 'global.R' file into environment
source("global.R")
# loads the files in the handlers folder
sourceAllFilesInFolder(file.path(getwd(), "handlers"))
# loads the files in the handlers folder
sourceAllFilesInFolder(file.path(getwd(), "handlers", "Gradescope"))
# loads the files in the functions folder
sourceAllFilesInFolder(file.path(getwd(), "functions"))
# loads the files in the functions folder
sourceAllFilesInFolder(file.path(getwd(), "functions", "canvas api"))

sourceAllFilesInFolder(file.path(getwd(), "functions", "canvas UI and local functions"))

# Changes max file upload size to 30 MB
# Runs the function to check if the grade csv exists 
#also makes the settings folder
options(shiny.maxRequestSize = 30 * 1024^2)
options(spinner.type = 6, spinner.color = "#cfbb34")


## Load UI ----
ui <- fluidPage(
  theme = bs_theme( bootswatch = "flatly"),
  shinyjs::useShinyjs(), 
  # shiny JS used for hiding/showing elements and enables html formatting https
  
  # CSS for the app
  tags$head(
    tags$style(HTML("
    table.dataTable tr.selected td {
      box-shadow: inset 0 0 9999px #2a7839 !important;  /*Change color and size to your preference */
    }      
    /* Active tab - gold */
  .nav-tabs .nav-link.active {
      background-color: gold !important; /* Gold background for active tab */
      color: black !important;          /* Black text for contrast */
      border-radius: 5px;               /* Optional rounded corners */
  }
    .tab-completed {
    background-color: #2a7839 !important; /* Green background */
    color: white !important;             /* White text */
    }
     .custom-button {
        background-color: #007BFF; /* Bootstrap primary color */
        color: white;
        font-size: 16px;
        padding: 10px 20px;
        border: none;
        border-radius: 5px;
        cursor: pointer;
      }
      .custom-button:hover {
        background-color: #0056b3;
      }
      .arrow-icon {
        margin-left: 10px;
        font-weight: bold;
      }
  ")),
  ),
  
  # navbarPage - creates a navbar at the top of the page
  navbarPage(
    id = "navbarID",
    # title and logo. Logo won't load when launching from R Studio, works in deployed app
    title = div(
      img(src = "PANE.png", height = "35px", width = "30px", style = "margin-right: 10px;"),
      "Grade Report Generator"
    ),
    # Makes the home page. createHomePage() is in the handlers folder
    tabPanel("Home",
             icon = icon("home"), createHomePage(),
             textOutput("connectivityStatus"),

             # icons from: https://www.w3schools.com/bootstrap/bootstrap_ref_comp_glyphs.asp
    ),
    ### Gradescope Page----
    tabPanel("Brief from Gradescope",
             value = "gradescope_panel",
             icon = icon("fire", lib = "glyphicon"),
             tabsetPanel(
               id = "gradescope_tabs", # Add an id here
               tabPanel("Preparation",value="gs_prep",gs_prep_ui()),
               tabPanel("Input Scores",value="gs_scores", gs_scores_ui()),
               tabPanel("Access Canvas",value="gs_canvas", withSpinner(gs_canvas_ui())),
               tabPanel("Input Cut Data",value="gs_cuts", gs_cuts_ui()),
               tabPanel("Make the Brief",value="gs_createbrief", gs_createbrief_ui()),
               tabPanel("Upload Scores to Canvas",value="gs_uploadgrades", gs_updategrades_ui())
             )
    ),
    
    
    ### Canvas Data Page----
    tabPanel("Canvas Access",
             value = "canvas_panel",
             icon = icon("cloud", lib = "glyphicon"),
             tabsetPanel(
               tabPanel("Canvas Gradebook and Template", withSpinner(createCanvasPrepPage())), 
               tabPanel("Assignment Group Stats", canvas_assignment_viewer()),
               tabPanel("Course grade Stats", canvas_gradebook_viewer()),
               tabPanel(
                 "Canvas Grade uploads",
                 withSpinner(uiOutput("canvas_upload_grades_ui"))
               ),
             )
    ),
    
    ### WPR Brief Page ----
    # Makes the pre-WPR data prep page.
    # Makes the WPR Analysis and Brief page.
    # This page within the navbar has multiple tabs
    
    tabPanel("WPR Analysis and Brief",
             icon = icon("stats", lib = "glyphicon"),
             tabsetPanel(
               tabPanel("Upload Data", createBriefUploadPage()),
               tabPanel("Create Brief", createBriefPage()),
               # this is for making the distribution plots
               tabPanel(
                 "Version Grade DistributionPlots",
                 div(
                   h2(HTML("<b>Plot Parameters</b>"), style = "text-align:left"),
                   uiOutput("dynamic_inputs")
                 ),
                 verbatimTextOutput("summary"),
                 plotOutput("barplot")
               ),
               # This is a tabular view of the version summary data
               tabPanel(
                 "Version Summary",
                 tableOutput("versionTable"),
                 verbatimTextOutput("Excel Viewer")
               )
             ),
    ),
    
    ### Post Brief OML Page (TBD)----
    # Post Brief OML is not integrated yet
    # tabPanel("Post Brief OML",icon=icon("list"),createOMLPage()),
    
    ### Settings ----
    # Settings page which will write to a folder defined in global.R
    
    tabPanel("Settings", icon = icon("cog"), createSettingsPage()),
    
    ### FAW ----
    
    # FAQ page which is sparse as of 6JAN24
    tabPanel("FAQ", icon = icon("question"), createFAQPage(), )
  ),
  
)
 
# Server ----
server <- function(input, output, session) {
  output$connectivityStatus <- renderText({
    if (check_internet_access()) {
      # Internet access is available
      "Note: Internet access is available"
    } else {
      # Internet access is not available
      "Note: Internet access is not available"
    }
  })
  # Check if the Canvas API token exists
  canvas_api_token <- reactiveVal( readRDS(canvas_api_token_path) )
  

  # Gradescope UI Elements ----
  shinyjs::disable(selector = "a[data-value='gs_scores']")
  #DEBUG - Disables UI elements for testing
  #shinyjs::disable(selector = "a[data-value='gs_canvas']")
  shinyjs::disable(selector = "a[data-value='gs_cuts']")
  #shinyjs::disable(selector = "a[data-value='gs_createbrief']")
  #shinyjs::disable(selector = "a[data-value='gs_uploadgrades']")
  
  # Shared reactives for tab completion
  gs_wizard_status <- reactiveValues(
    gs_prep_completed = FALSE,
    gs_scores_completed = FALSE,
    gs_canvas_completed = FALSE,
    gs_cuts_completed = FALSE,
    gs_brief_completed = FALSE
  )
  
  gs_data <- reactiveValues(
    canvas_api_token = NULL, # string read to access
    gs_question_groups = NULL, #score groups from gradescope
    gs_roster = NULL,
    canvas_roster = NULL, # data frame of grades from canvas
    missing_roster = NULL, # data frame of missing grades
    cuts_df = NULL, # data frame of cuts
    ppt = NULL
  )
  
  gs_prep_server(input, output, session, gs_data,gs_wizard_status)
  gs_scores_server(input, output, session, gs_data,gs_wizard_status)
  gs_canvas_server(input, output, session, gs_data,gs_wizard_status,canvas_api_token)
  gs_cuts_server(input, output, session, gs_data,gs_wizard_status)
  gs_createbrief_server(input, output, session, gs_data,gs_wizard_status)
  gs_updategrades_server(input, output, session, gs_data,gs_wizard_status)
  
  
  ### Handling Sequential Tabs ----
  # Gradescope Preparation: Mark complete and enable next tab
  observeEvent(input$confirm_gs_prep, {
    removeModal() # Remove the modal
    shinyjs::addClass(selector = "a[data-value='gs_prep']", class = "tab-completed")
    shinyjs::enable(selector = "a[data-value='gs_scores']")
    updateTabsetPanel(session, "gradescope_tabs", selected = "gs_scores")
  })
  
  observeEvent(input$confirm_gs_scores, {
    removeModal()
    shinyjs::addClass(selector = "a[data-value='gs_scores']", class = "tab-completed")
    shinyjs::enable(selector = "a[data-value='gs_canvas']")
    updateTabsetPanel(session, "gradescope_tabs", selected = "gs_canvas")
  })
  
  observeEvent(input$confirm_gs_canvas, {
    removeModal()
    shinyjs::addClass(selector = "a[data-value='gs_canvas']", class = "tab-completed")
    shinyjs::enable(selector = "a[data-value='gs_cuts']")
    updateTabsetPanel(session, "gradescope_tabs", selected = "gs_cuts")
  })
  
  observeEvent(input$confirm_gs_cuts, {
    shinyjs::addClass(selector = "a[data-value='gs_cuts']", class = "tab-completed")
    shinyjs::enable(selector = "a[data-value='gs_createbrief']")
    updateTabsetPanel(session, "gradescope_tabs", selected = "gs_createbrief")
  })
  
  observeEvent(input$gs_createbrief_next, {
    shinyjs::addClass(selector = "a[data-value='gs_createbrief']", class = "tab-completed")
    shinyjs::enable(selector = "a[data-value='gs_uploadgrades']")
    updateTabsetPanel(session, "gradescope_tabs", selected = "gs_uploadgrades")
  })
  observeEvent(input$gs_brief_shortcut, {
    shinyjs::enable(selector = "a[data-value='gs_createbrief']")
    updateTabsetPanel(session, "gradescope_tabs", selected = "gs_createbrief")
  })
  ###GS Canvas API token ----
  # store the api token in the gradescope shared data object
  observeEvent(input$navbarID, {
    if (input$navbarID == "gradescope_panel") {
      # Code to update canvas_api_token
      if (file.exists(canvas_api_token_path)) {
        gs_data$canvas_api_token <- readRDS(canvas_api_token_path)
      } else {
        gs_data$canvas_api_token <- NULL
      }
    }
  })
  
  # Canvas UI Elements ----
  output$Canvas_data_UI <- renderUI({
    if (input$navbarID == "canvas_panel") {
      createCanvasPrepPage()
    }
  })
  
  output$canvas_upload_grades_ui <- renderUI({
    if (input$navbarID == "canvas_panel") {
      canvas_grade_upload()
    }
  })
  


  
  canvasPrep_Handler(input, output, session, canvas_api_token)
  
  
  # Canvas UI Elements ----
  
  
  # Settings logic and display
  settings_Handler(input, output, session,canvas_api_token)
  
  # Brief logic and display
  brief_Handler(input, output, session)
  
  
  #### Task Kill ####
  session$onSessionEnded(function() {
    stopApp()
    # you can put your code here to close the connection
  })
}



shinyApp(ui = ui, server = server)
