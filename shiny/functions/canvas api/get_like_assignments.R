#' Purpose: gets a list of assignments that are the same for a list of courses
#' @param course.df list of all courses we want to aggregate
#' returns: dataframe of assignments that are the same across all courses
get_like_assignments <- function(course.df){
    
  #get the assignment list for each course
    withProgress(message = "Gathering Assignment Data for All Courses", value=0, {
    assignments <- lapply(unique(course.df$id), function(x,i){
      incProgress(1/nrow(course.df))
      #browser()
      get_course_items(course_id = x, item = "assignments") %>%
        select(id,name,points_possible,assignment_group_id,course_id,published)},i = seq_along(course.df))
    })


  #find the assignment names that are the same across all elements of the assignments list
  assignments.common <- Reduce(intersect, lapply(assignments, function(x) x$name))
  
  #if it is 0 then stop and tell the user (prevents errors downstream)
  if(length(assignments.common) == 0) {return(NULL)}
  #filter the assignments list to only include the common assignments
  assignments.common.df <- lapply(assignments, function(x) x[x$name %in% assignments.common,])
  #combines the assignment lists into one dataframe
  assignments.comb <- do.call(rbind, assignments.common.df)
  return(assignments.comb)
}