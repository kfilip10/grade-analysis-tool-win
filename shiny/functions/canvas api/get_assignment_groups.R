get_assignment_groups <- function(df,course_id,progress=FALSE){
  #assume page is 1 per course (unlikely to have 100 assignment groups within a course)
  #get the number of page
  url <- make_canvas_url('courses',course_id, "assignment_groups")
  args <- list(per_page = 100,
               page = 1
  )
  #paste and separate by commas
  #paste0("[",paste(assign.lookup$id, collapse = ","),"]")
  
  response <- GET(url,
                  add_headers(Authorization = paste("Bearer", check_token())),
                  query  = args,
                  encode = 'form')
  
  if(response$status_code != 200)
    stop(response$status_code)
  else{
    assign.group <- response %>%
      httr::content("text") %>%
      jsonlite::fromJSON(flatten = TRUE)
    return(assign.group)
  }
}