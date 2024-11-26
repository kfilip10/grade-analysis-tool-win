#' @examples
#' #' get_course_list()
#' #' get_course_list(user_id = 366)
#' #' get_course_list(include = c("teachers", "total_students"))
get_course_list <- function(user_id = NULL, include = NULL) {
  if (!is.null(user_id)) {
    url <- make_canvas_url("users", user_id, "courses")
  } else {
    url <- make_canvas_url("courses")
  }
  args <- list(
    per_page = 100,
    user_id = user_id
  )
  include <- iter_args_list(include, "include[]")
  args <- c(args, include)
  dat <- process_response(url, args)
  return(unique(dat))
}

