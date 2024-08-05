#this is the function that will be applied to each course_id group
#resp <- lapply(split(roster.course, roster.course$course_id), bulk_grade_post)
#expects a dataframe from the gradebook with columns: course_id, user_id, id (from assignment table), points
# to get the bulk grades posted I need the following in the df:
#' @param course_id [Integer] id from course roster
#' @param user_id [Integer] id from course roster
#' @param assignment_id [Integer] id from assignment list 
#' @param points [Integer] number of points to assign, labelled in course roster as 'points'
#' @req dataframe with combined course roster
#' @req dataframe with combined assignment list
grade_assignments_bulk <- function(df){
  
  if(length(unique(df$assignment_id)) > 1) stop("Only one assignment_id allowed")
  assignment_id <- unique(df$assignment_id)
  course_id <- unique(df$course_id)
  grade_data <- setNames(as.list(df$points), paste0('grade_data[', df$id, '][posted_grade]'))
  url <- make_canvas_url('courses',course_id, "assignments", assignment_id, "submissions/update_grades")
  response <- POST(url,
                   add_headers(Authorization = paste("Bearer", check_token())),
                   body = grade_data,
                   encode = 'form')
  #then I need to return the result as success or failure and return the course id and assignment id
  #if success then return the course_id and assignment_id
  #if failure then return the course_id and assignment_id and the response
  if (response$status_code == 200) {return(paste("Successful upload for course: ",course_id))}
  else{return(paste("FAILED TO UPLOAD FOR COURSE: ",course_id, " with response ", response$status_code))}
}