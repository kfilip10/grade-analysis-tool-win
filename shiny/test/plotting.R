#could have multiple assignments
#group by 
#- section and assignment
#- assignment group
# #-none
# #select 
# #- instructor
# #- assignment from list of assignments
# 
# 
# filteredPlotData <-   course_gradebook %>% group_by(section, assignment_group_name) %>%
#     summarise(n=n(),mean=mean(percent,na.rm=TRUE),sd=sd(percent,na.rm=TRUE))
#   
#   
# 
# 
# 
#  # output$plotByAssignmentGroup <- renderPlot({
#  #   browser()
#     ggplot(filteredPlotData, aes(x = section, y = mean,fill=assignment_group_name)) +
#       geom_bar(stat = "identity") +
#       theme_bw() 
#  # })
    
    
    
    library(shiny)
    library(ggplot2)
    library(dplyr)
    
    # Sample dataframe
    #set.seed(123)
    #course_gradebook
    
    ui <- fluidPage(
      titlePanel("Dynamic Data Filtering and Bar Chart"),
      mainPanel(
          selectInput("filterCat", "Select Category:", choices = unique(course_gradebook$assignment_name)),
          selectInput("groupBy", "Group Data By:", choices = c("section", "instructor")),
          actionButton("btn", "Update Chart"),


          plotOutput("barChart")
        )
    )
    
    
    server <- function(input, output) {
      filteredData <- eventReactive(input$btn, {
        course_gradebook %>%
          filter(assignment_name == input$filterCat)
      })
      
      output$barChart <- renderPlot({
        req(filteredData()) # Ensure data is available before plotting
        
        aggData <- filteredData() %>%
          group_by(!!sym(input$groupBy)) %>%
          summarise(AvgValue = mean(percent*100, na.rm = TRUE),sd=sd(percent*100,na.rm=TRUE),count=n())
        
        ggplot(aggData, aes_string(x = input$groupBy, y = "AvgValue")) +
          geom_bar(stat = "identity",width=0.5) +
          theme_bw() + theme(text = element_text(size=20))+
          #add error bars
          geom_errorbar(aes(ymin=AvgValue-sd, ymax=AvgValue+sd), width=.2,
                        position=position_dodge(0.9)) +
          labs(y = "Average Value", x = input$groupBy, title = paste("Average Value by", input$groupBy))
      })
    }
    
    shinyApp(ui, server)
    