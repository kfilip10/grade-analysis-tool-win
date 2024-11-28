
table_courses <- function(course_data, page_length = 10) {
  # Check if the data is NULL
  if (is.null(course_data)) {
    return(NULL)
  }
  
  # Create the DataTable
  DT::datatable(
    course_data %>%
      select(course, section_hour, section, term, id, total_students, instr_name),
    colnames = c(
      "Term" = "term",
      "Course" = "course",
      "Instructor" = "instr_name",
      "Hours" = "section_hour",
      "Section" = "section",
      "Students" = "total_students",
      "ID" = "id"
    ),
    selection = 'multiple',  # Enable multiple selection
    filter = 'top',          # Enable column-based filtering
    options = list(
      pageLength = page_length,  # Set the number of rows per page
      autoWidth = TRUE,
      columnDefs = list(
        list(width = '40px', targets = c(2, 3)) # Adjust specific column width if needed
      )
    )
  )
}
