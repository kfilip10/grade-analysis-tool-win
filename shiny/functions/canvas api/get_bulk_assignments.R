# Takes a course list dataframe from canvas api with at least an id 
# 
#' @param course_list dataframe with at least an id column. This comes from user checkbox. Requires
#' @param course_id course id to get assignments for
get_bulk_assignments <- function(course.list,course_id,progress=FALSE){
  n_pages <- ceiling(max(course.list$n)*nrow(course.list)/100)
  #Left off here what's next? make arguments
  url <- make_canvas_url('courses',course_id, "students/submissions")
  args <- list(`student_ids[]` = "all",
               per_page = 100
  )
  assignment_query <- iter_args_list(course.list$id, "assignment_ids[]")
  args <- c(args, assignment_query)
  
  response <- GET(url,
                  add_headers(Authorization = paste("Bearer", check_token())),
                  query  = args,
                  encode = 'form')
  d <- paginate(response) %>%
    purrr::map(httr::content, "text") %>%
    purrr::map(jsonlite::fromJSON, flatten = TRUE)
  
  # flatten to data.frame if able, otherwise return as is
  #d <- tryCatch(purrr::map_df(d, purrr::flatten_df),
  #              error = function(e) d)
  d <- dplyr::bind_rows(d)
  
  return(d)
  #handle pagination
  # gradebook.comb <- purrr::map_df(seq_len(n_pages), function(page) {
  #   browser()
  #   
  #   if(progress)
  #     cat(page, "of", n_pages, "\n")
  #   
  #   args <- list(`student_ids[]` = "all",
  #                per_page = 100,
  #                page = page
  #   )
  #   
  #   #this is the key
  #   assignment_query <- iter_args_list(course.list$id, "assignment_ids[]")
  #   args <- c(args, assignment_query)
  #   
  #   #paste and separate by commas
  #   #paste0("[",paste(assign.lookup$id, collapse = ","),"]")
  #   
  #   response <- GET(url,
  #                   add_headers(Authorization = paste("Bearer", check_token())),
  #                   query  = args,
  #                   encode = 'form')
  #   
  #   if(response$status_code != 200)
  #     stop(response$status_code)
  #   else{
  #     gradebook <- response %>%
  #       httr::content("text") %>%
  #       jsonlite::fromJSON(flatten = TRUE)
  #     return(gradebook)
  #     
  #   }
    
  #})
  
  #return(gradebook.comb)
  
}
