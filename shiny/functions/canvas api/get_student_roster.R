#' @param course_df list of all courses we want to aggregate
#' @req API token set
#' @return df of all students in all courses with their point data and instructor info
get_student_roster <- function(course_df, error_list, instructor.search.key = "instr") {
  withProgress(message = "Gathering Roster Data for All Courses", value = 0, {
    # error_list definition
    
    enrollment.list <- lapply(unique(course_df$id), function(x, i) {
      incProgress(1 / nrow(course_df) / 2)
      tryCatch(
        {
          get_course_items(course_id = x, item = "enrollments", include = "current_points")
        },
        error = function(e) {
          # Append error message to the reactive list
          current_errors <- error_list
          error_list(c(current_errors, list(paste("Error processing course ID:", x, "->", e$message))))
          return(NULL) # Return NULL for failed courses
        }
      )
    }, i = seq_along(course_df))
  })
  if (length(enrollment.list) == 0) {
    return(NULL)
  }
  
  enrollment_df <- do.call(rbind, enrollment.list)
  
  # user.df <- do.call(rbind, user.list) %>%select(id,email) %>%rename(user_id=id) %>% distinct()
  # add email from user.df to enrollment_df
  # enrollment_df <- left_join(enrollment_df, user.df, by="user_id")
  
  # this is the list of instructors matched to the course they teach based on the 'instructor' role
  instr <- enrollment_df %>%
    filter(type == "TeacherEnrollment", grepl(instructor.search.key, role, ignore.case = TRUE)) %>%
    select(course_id, user.short_name) %>%
    distinct()
  # if there are two instructors in the same course, combine them and separate by an ampersand
  instr <- instr %>%
    group_by(course_id) %>%
    summarise(user.short_name = paste(user.short_name, collapse = " & "))
  
  st_comb <- enrollment_df %>% filter(type == "StudentEnrollment")
  # browser()
  # This is now a table of useres, the course they are in, and their points
  # rename user.login_id to email
  # 
  colnames(st_comb) <- gsub("user.login_id", "email", colnames(st_comb))
  
  st_comb <- st_comb %>% select(
    course_id,
    user_id,
    sis_user_id, # C number
    course_section_id,
    user.sortable_name,
    email,
    grades.unposted_current_score,
    grades.unposted_current_grade,
    grades.unposted_current_points
  )
  # rename course_section_id to section_id
  names(st_comb)[names(st_comb) == "course_section_id"] <- "section_id"
  
  # lookup the instructor for each student based on maching course_id
  st_comb <- merge(st_comb, instr, by.x = "course_id", by.y = "course_id")
  
  # change column name to instructor
  names(st_comb)[names(st_comb) == "user.short_name"] <- "instructor"
  
  # add the course name (so we know section / hour info)
  st_comb <- merge(st_comb, course_df %>% select(course, section_hour, section, section_id), by.x = "section_id", by.y = "section_id")
  
  return(st_comb)
}
