# include can be “students”: Associations to include with the group.
# “enrollments”: If ‘students’ is also included, return the section enrollment for each student
# https://canvas.instructure.com/doc/api/sections.html
get_section_info <- function(course_id, section_id=NULL,include=NULL) {
  url <- make_canvas_url("courses", course_id, "sections", section_id)
  args <- list(per_page = 100)
  include <- iter_args_list(include, "include[]")
  #args <- c(args, include)
  process_response(url, include) %>%
    dplyr::mutate(course_id = course_id)
}