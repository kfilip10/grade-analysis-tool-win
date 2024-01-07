#' @title Function to return various course items.
#'
#' @description Returns a data.frame of various course items. See "item" argument below. Omitting the "item argument
#' returns a course object.
#'
#' @param course_id A valid Canvas course id
#' @param item Optional -- one of "settings", "discussion_topics", "todo", "enrollments", "features", "files", "modules", "front_page", "pages", "quizzes", "folders".
#' @param include Optional additions to the query string
#' @return data frame
#' @export
#'
#' @examples
#' #' get_course_items(course_id = 20, item = "settings")
#' #' get_course_items(course_id = 20, item = "enrollments")
#' #' get_course_items(20, item = "users", include = "email")
get_course_items <- function(course_id, item, include = NULL) {
  valid_items <- c("settings", "discussion_topics", "todo", "enrollments", "users", "students",
                   "features", "assignments", "files", "modules", "front_page", "pages", "quizzes",
                   "folders", "assignment_groups")
  if (!missing(item) && !item %in% valid_items) {
    stop(paste("item argument must be one of:", paste(valid_items, collapse = ", ")))
  }
  if (!missing(item)) {
    url <- make_canvas_url("courses", course_id, item)
  } else {
    #Omitting the item argument will return general information about the course
    url <- make_canvas_url("courses", course_id)
  }
  args <- list(per_page = 100)
  include <- iter_args_list(include, "include[]")
  args <- c(args, include)
  process_response(url, args) %>%
    dplyr::mutate(course_id = course_id)
}