library(shiny)
library(DT)

# Sample dataframe
my_df <- data.frame(
  ID = 1:10,
  Name = LETTERS[1:10],
  Value = runif(10)
)

# Define UI
ui <- fluidPage(
  titlePanel("Search and Add to List"),
  sidebarLayout(
    sidebarPanel(
      textInput("search", "Search", value = "")
    ),
    mainPanel(
      DTOutput("datatable"),
      h3("Selected Items:"),
      DTOutput("selectedList")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive expression for filtered data
  filteredData <- reactive({
    if(input$search != "") {
      my_df[grepl(input$search, my_df$Name, ignore.case = TRUE),]
    } else {
      my_df
    }
  })
  
  # Render the data table for search results
  output$datatable <- renderDT({
    datatable(filteredData(), selection = 'single')
  }, server = FALSE)
  
  # Reactive value to store selected items
  selectedItems <- reactiveVal(data.frame(ID = integer(), Name = character(), Value = numeric(), stringsAsFactors = FALSE))
  
  # Update selected items when a row is clicked
  observeEvent(input$datatable_rows_selected, {
    selectedItem <- filteredData()[input$datatable_rows_selected,]
    # Check if item is already in the list
    if(!any(selectedItems()[, "ID"] == selectedItem$ID)) {
      selectedItems(rbind(selectedItems(), selectedItem))
    }
  })
  
  # Render the selected items list with an option to remove
  output$selectedList <- renderDT({
    datatable(selectedItems(), selection = 'none', options = list(dom = 't', paging = FALSE, ordering = FALSE))
  }, server = FALSE)
  
  # Handle item removal by clicking on it
  observeEvent(input$selectedList_cell_clicked, {
    cell <- input$selectedList_cell_clicked
    if (!is.null(cell) && !is.null(cell$row)) {
      # Remove the item from the list
      items <- selectedItems()
      if (nrow(items) >= cell$row) { # Check if row is valid
        items <- items[-cell$row,]
        selectedItems(items)
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
