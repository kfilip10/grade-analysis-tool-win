#' @param course.df list of all courses we want to aggregate
#' @req API token set
#' @return df of all students in all courses with their point data and instructor info
get_student_roster <- function(course.df,instructor.search.key = "instr"){
  
  withProgress(message = "Gathering Roster Data for All Courses", value=0, {
  #browser()
  

  enrollment.list <- lapply(unique(course.df$id),  function(x,i){ 
    incProgress(1/nrow(course.df)/2)
    get_course_items(course_id = x, item = "enrollments",include="current_points")
    },i=seq_along(course.df) )
  
  user.list <- lapply(unique(course.df$id),  function(x,i){ 
    incProgress(1/nrow(course.df)/2)
    get_course_items(course_id = x, item = "users")
  },i=seq_along(course.df) )
  })
  
  enrollment.df <- do.call(rbind, enrollment.list)
  
  user.df <- do.call(rbind, user.list) %>%select(id,email) %>%rename(user_id=id) %>% distinct()
  #add email from user.df to enrollment.df
  enrollment.df <- left_join(enrollment.df, user.df, by="user_id")
  
  #this is the list of instructors matched to the course they teach based on the 'instructor' role
  instr <- enrollment.df %>% filter(type=="TeacherEnrollment", grepl(instructor.search.key,role,ignore.case=TRUE)) %>% 
    select(course_id,user.short_name) %>% distinct()
  #if there are two instructors in the same course, combine them and separate by an ampersand
  instr <- instr %>% group_by(course_id) %>% summarise(user.short_name = paste(user.short_name, collapse=" & "))
  
  st.comb <- enrollment.df %>% filter(type=="StudentEnrollment")
  #browser()
  #This is now a table of useres, the course they are in, and their points
  st.comb <- st.comb %>%select(course_id,
                               user_id,
                               course_section_id,
                               user.sortable_name,
                               email,
                               grades.unposted_current_score,
                               grades.unposted_current_grade,
                               grades.unposted_current_points)
  #rename course_section_id to section_id
  names(st.comb)[names(st.comb) == "course_section_id"] <- "section_id"

  #lookup the instructor for each student based on maching course_id
  st.comb <- merge(st.comb, instr, by.x="course_id", by.y="course_id")
  
  #change column name to instructor
  names(st.comb)[names(st.comb) == "user.short_name"] <- "instructor"
  
  #add the course name (so we know section / hour info)
  st.comb <- merge(st.comb, course.df%>%select(course,section_hour,section,section_id), by.x="section_id", by.y="section_id")
  
  return(st.comb)
}