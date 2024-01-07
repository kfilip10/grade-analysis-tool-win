edit_course_assignment <- function(course_id, assignment_id, due_at = NULL, lock_at = NULL, unlock_at = NULL) {
  
  args = sc(list(due_at = due_at,
                 lock_at = lock_at,
                 unlock_at = unlock_at))
  
  names(args) <- sprintf("assignment[%s]", names(args))
  url <- make_canvas_url("courses", course_id, "assignments",assignment_id)
  invisible(canvas_query(url, args, "PUT"))
  message(sprintf("Assignment %s updated", assignment_id))
}

