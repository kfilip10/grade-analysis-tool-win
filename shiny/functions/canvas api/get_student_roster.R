#' @param course.df list of all courses we want to aggregate
#' @req API token set
#' @return df of all students in all courses with their point data and instructor info
get_student_roster <- function(course.df,instructor.search.key = "instr"){
  
  withProgress(message = "Gathering Roster Data for All Courses", value=0, {
    
  enrollment.list <- lapply(course.df$id,  function(x,i){ 
    incProgress(1/nrow(course.df))
    get_course_items(course_id = x, item = "enrollments",include="current_points")
    },i=seq_along(course.df) )
  
  })
  enrollment.df <- do.call(rbind, enrollment.list)
  
  st.comb <- enrollment.df %>% filter(type=="StudentEnrollment")
  
  #this is the list of instructors matched to the course they teach based on the 'instructor' role
  instr <- enrollment.df %>% filter(type=="TeacherEnrollment", grepl(instructor.search.key,role,ignore.case=TRUE)) %>% 
    select(course_id,user.short_name)
  
  #This is now a table of useres, the course they are in, and their points
  st.comb <- st.comb %>%select(course_id,
                               user_id,
                               user.sortable_name,
                               user.login_id,
                               grades.unposted_current_score,
                               grades.unposted_current_grade,
                               grades.unposted_current_points)
  
  #lookup the instructor for each student based on maching course_id
  st.comb <- merge(st.comb, instr, by.x="course_id", by.y="course_id")
  
  #change column name to instructor
  names(st.comb)[names(st.comb) == "user.short_name"] <- "instructor"
  
  #add the course name (so we know section / hour info)
  st.comb <- merge(st.comb, course.df%>%select(id,name), by.x="course_id", by.y="id")
  
  return(st.comb)
}