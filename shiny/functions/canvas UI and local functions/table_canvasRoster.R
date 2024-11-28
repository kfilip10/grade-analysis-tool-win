
table_canvasRoster <- function(canvas_roster, page_length = 999) {
  # Check if the data is NULL
  if (is.null(canvas_roster)) {
    return(NULL)
  }
  # Create the DataTable
  #browser()
  DT::datatable(
    canvas_roster %>%
      select(course_id,instructor, section_hour,user.sortable_name, section, sis_user_id, 
             user_id, grades.unposted_current_score,grades.unposted_current_grade,
             grades.unposted_current_points,email)%>%
      mutate(grades.unposted_current_score=round(grades.unposted_current_score,1),
             grades.unposted_current_points=round(grades.unposted_current_points,1)
             ),
    colnames = c(
      "Course" = "course_id",
      "Instructor" = "instructor",
      "Hours" = "section_hour",
      "Section" = "section",
      "Name" = "user.sortable_name",
      "CID" = "sis_user_id",
      "Canvas ID" = "user_id",
      "Score (%)" = "grades.unposted_current_score",
      "Grade" = "grades.unposted_current_grade",
      "Points" = "grades.unposted_current_points",
      "Email" = "email"
    ),
    selection = 'multiple',  # Enable multiple selection
    filter = 'top',          # Enable column-based filtering
    extensions = 'Buttons',  # Enable the Buttons extension
    
    options = list(
      dom = 'Bfrtip',        # Define the table control elements
      buttons = list(
        list(
          extend = 'excel',
          text = 'Download Excel (All)',
          exportOptions = list(
            modifier = list(page = 'all') # Export all rows, not just visible
          )
        )
      ),
      pageLength = page_length,  # Set the number of rows per page
      autoWidth = TRUE,
      columnDefs = list(
        list(width = '40px', targets = c(2, 3)) # Adjust specific column width if needed
      )
    )
  )
}
