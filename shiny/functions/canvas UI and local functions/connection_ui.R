
# Create a UI for the connection status
# connectionstatus - boolean from reactive context that indicates if the connection was successful
create_connection_ui <- function(connectionstatus) {
  if (!is.null(connectionstatus) && connectionstatus != "Success") {
    tagList(
      tags$p(style = "color: white;font-weight: bold;background-color: #781d1d;
                      text-align: center; border-radius: 10px; padding: 5px;",
             "Connection Failed:", connectionstatus),
      tags$p("Please check your API token is correct in settings, that your token is not expired (Canvas settings), and that Canvas is operational.")
    )
  } else if (!is.null(connectionstatus) && connectionstatus == "Success") {
    tagList(
      tags$p(style = "color: white;font-weight: bold;background-color: #6fbd7a;
                              text-align: center; border-radius: 10px; padding: 5px;",
             "API Connection Successful!"),
      h4("Instructions"),
      tags$ol(
        tags$li("Select the courses you want to collect data from."),
        tags$li("Check your sections have the correct name and AY."),
      ),
      tags$hr()
    )
  } else {
    tagList()
  }
  }
