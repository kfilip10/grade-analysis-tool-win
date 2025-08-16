#' Sourced from r-canvas github: https://github.com/daranzolin/rcanvas
#' Some edits made for functionality after finding bugs
#' Canvas API helpers
#'
#' These functions set your Canvas API token, as well as the Canvas base URL.
#' These functions are necessary for `rcanvas` to run.



# env for the Canvas domain
cdenv <- new.env()

#' @name apihelpers
#' @md

#' @param token your API token
#' @export
#' @rdname apihelpers
#' @examples
#' set_canvas_token("abc123")
set_canvas_token <- function(token) {
  keyring::key_set_with_value("rcanvas_CANVAS_API_TOKEN", NULL, token)
}


# @param domain Canvas domain
#' @export
#' @rdname apihelpers
#' @examples
#' set_canvas_domain("https://canvas.upenn.edu")
set_canvas_domain <- function(domain) {
  assign("rcanvas_CANVAS_DOMAIN", domain, envir = cdenv)
}


#' @rdname apihelpers
check_token <- function() {
  token <- keyring::key_get("rcanvas_CANVAS_API_TOKEN")
  if (identical(token, "")) {
    stop("Check your token is in settings and it has not expired.",
      call. = FALSE
    )
  }
  token
}

api_test <- function() {
  tryCatch(
    {
      url <- make_canvas_url("users", "self")
      resp <- canvas_query(url)

      if (httr::status_code(resp) == 200) {
        return("Success")
      } else {
        return("Failed to retrieve from Canvas. Please check your authentication and API endpoint.")
      }
    },
    error = function(e) {
      # Return the error message as a string
      return(paste(e$message))
    }
  )
}


canvas_url <- function() paste0(get("rcanvas_CANVAS_DOMAIN", envir = cdenv), "/api/v1")

make_canvas_url <- function(...) {
  url <- paste(canvas_url(), ..., sep = "/")
  if (getOption(".rcanvas.show.url", default = FALSE)) {
    message(url)
  }
  return(url)
}

#' @importFrom httr GET POST PUT HEAD
canvas_query <- function(urlx, args = NULL, type = "GET") {
  args <- sc(args)
  resp_fun_args <- list(
    url = urlx,
    httr::add_headers(Authorization = paste("Bearer", check_token()))
  )

  if (type %in% c("POST", "PUT")) {
    resp_fun_args$body <- args
  } else {
    resp_fun_args$query <- args
  }

  resp <- do.call(type, resp_fun_args)
  # Make the API request
  response <- httr::GET(urlx, httr::add_headers(Authorization = paste("Bearer", check_token())))

  # Check the response status code
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve from Canvas. Please check your authentication and API endpoint. URL: ", urlx)
  }
  httr::stop_for_status(resp)
  resp
}

iter_args_list <- function(x, label) {
  ln <- list()
  for (i in seq_along(x)) {
    ln[[i]] <- x[i]
    names(ln)[[i]] <- label
  }
  ln
}

sc <- function(x) {
  purrr::discard(x, is.null)
}

convert_dates <- function(base_date = Sys.Date(), days) {
  new_date <- base_date + lubridate::ddays(days)
  format(new_date, "%Y-%m-%d")
}

#' Execute a query on the remove API
#'
#' This function allows you to call methods which are not specifically exposed by this API yet
#'
#' @param endpoint the API endpoint to call, with or without the canvas domain. You can give a vector of parts which will be joined with slashes.
#' @param args a list of arguments for the call
#' @param method GET or POST
#' @param process_response if TRUE (default for GET requests), paginate results and return a data frame
#' @return A data.frame if process_response is TRUE, otherwise an httr response
#'
#' @export
#' @examples
#' # A get request to the announcements endpoint (replicating get_announcements):
#' do_query("announcements", list(`context_codes[]` = "course_1234"))
#'
#' # A post request to the group membership endpoint (replicating add_group_user):
#' do_query(c("groups", 123, "memberships"), list(user_id = 1), method = "POST")
do_query <- function(endpoint, args = NULL, method = "GET", process_response = (method == "GET")) {
  endpoint <- paste(endpoint, collapse = "/")
  if (!grepl("^https?://", endpoint)) endpoint <- paste0(canvas_url(), endpoint)
  if (process_response) {
    if (method != "GET") stop("Process_response can only be used on GET requests")
    process_response(endpoint, args)
  } else {
    invisible(canvas_query(endpoint, args, method))
  }
}

#### Paginatation ####
#' Sourced from r-canvas github: https://github.com/daranzolin/rcanvas
#' Some edits made for functionality
#'
#' Process a Canvas API response
#'
#' Wrapper function for common tasks in going from Canvas URL to data.frame. Most
#' of the heavy lifting is done in \code{paginate}, which finds which pages to
#' download. This function adds necessary arguments to those pages (e.g. the
#' authentication token), downloads the content, converts from JSON into
#' data.frame format, and if there are multiple pages/data.frames, converts it
#' into one final data.frame if able.
#'
#' @param url url to query
#' @param args query arguments to be passed to \code{httr}, e.g. auth token
#'
#' @return processed dataframe or list if unable to simplify
#' @importFrom magrittr `%>%`
process_response <- function(url, args) {
  resp <- canvas_query(url, args, "GET")

  d <- paginate(resp) %>%
    purrr::map(httr::content, "text") %>%
    purrr::map(jsonlite::fromJSON, flatten = TRUE)

  # flatten to data.frame if able, otherwise return as is
  # d <- tryCatch(purrr::map_df(d, purrr::flatten_df),
  #              error = function(e) d)
  dplyr::bind_rows(d)
}

#' Get responses from Canvas API pages
#'
#' @description The Canvas headers include a link object (usually), in form:
#' \code{Link:
#' <https://canvas.url/api/v1/[...]?page=1&per_page=10>; rel="current",
#' <https://canvas.url/api/v1/[...]?page=2&per_page=10>; rel="next",
#' <https://canvas.url/api/v1/[...]?page=1&per_page=10>; rel="first",
#' <https://canvas.url/api/v1/[...]?page=15&per_page=10>; rel="last"}
#'
#' In this case, we need to download every page from 1 to 15 to capture all data.
#' This function parses the response object intelligently, using only HEAD
#' requests, to figure out these page requirements.
#'
#' @param x a httr response object
#' @param showProgress if TRUE (default), show a textual progress bar
#'
#' @return unparsed responses
#'
#' @examples
#' \dontrun{
#' resp <- canvas_query(url, args, "HEAD")
#' get_pages(resp)
#' }
paginate <- function(x, showProgress = T) {
  first_response <- list(x)
  stopifnot(httr::status_code(x) == 200) # OK status
  pages <- httr::headers(x)$link
  print(paste("Link header:", pages)) # <-- Add this line

  # browser()
  if (is.null(pages)) {
    return(first_response)
  }

  should_continue <- TRUE

  if (has_rel(pages, "last")) {
    last_page <- get_page(x, "last")
    print(paste("last_page URL:", last_page))
    # Check for 'page=first' (single page)
    if (grepl("page=first", last_page)) {
      print("Detected 'page=first' in last_page; treating as single page.")
      return(first_response)
    }
    # Otherwise, extract numeric page value
    page_val <- stringr::str_match(last_page, "page=(\\d+)")[, 2]
    print(paste("Extracted page value:", page_val))
    if (is.na(page_val)) {
      stop("Could not parse number of pages from last page URL.")
    }
    n_pages <- as.integer(page_val)
    print(paste("Number of pages detected:", n_pages))
    if (n_pages == 1) {
      return(first_response)
    } else {
      pages <- increment_pages(last_page, 2:n_pages)
      print("Page URLs to fetch:")
      print(pages)
      if (showProgress) {
        bar <- txtProgressBar(max = n_pages, style = 3)
      }
      queryfunc <- function(...) {
        if (showProgress) bar$up(bar$getVal() + 1)
        canvas_query(...)
      }
      responses <- pages %>%
        purrr::map(queryfunc, args = list(access_token = check_token()))
      responses <- c(first_response, responses)
      lapply(responses, function(resp) {
        content <- httr::content(resp, "text")
        data <- jsonlite::fromJSON(content, flatten = TRUE)
        print(paste("Records in page:", length(data)))
      })
      return(responses)
    }
  } else {
    if (has_rel(httr::headers(x)$link, "next")) {
      pages_list <- list() # Create a new list for pages

      # Debug: Check what get_page returns
      current_page <- get_page(x, "current")
      print(paste("Current page URL:", current_page))

      if (is.null(current_page) || current_page == "") {
        stop("Could not get current page URL")
      }

      pages_list[[1]] <- current_page

      inc <- 2

      # edge case for if there is no 'last' header, see:
      # https://canvas.instructure.com/doc/api/file.pagination.html
      # https://github.com/daranzolin/rcanvas/issues/4
      while (should_continue) {
        page_temp <- get_page(x, "next")
        print(paste("Next page URL:", page_temp))
        pages_list[[inc]] <- page_temp
        x <- canvas_query(page_temp,
          args = list(access_token = check_token()),
          type = "HEAD"
        )
        if (!has_rel(httr::headers(x)$link, "next")) {
          should_continue <- FALSE
        } else {
          inc <- inc + 1
        }
      }

      print(paste("Total pages collected:", length(pages_list)))
      responses <- pages_list %>%
        purrr::map(canvas_query, args = list(access_token = check_token()))
      return(responses)
    }
  }
}
# ...existing code...


increment_pages <- function(base_url, n_pages) {
  # odd regex but necessary, see http://regexr.com/3evr4
  stringr::str_replace(
    base_url, "([\\?&])(page=[0-9a-zA-Z]{1,})",
    sprintf("\\1page=%s", n_pages)
  )
}

has_rel <- function(x, rel) {
  stopifnot(!is.null(rel))
  any(grepl(paste0("rel=\"", rel, "\""), x))
}

get_page <- function(resp, page) {
  pages <- resp$headers$link
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  pages <- stringr::str_split(pages, ",")[[1]]

  # Look for the specific rel="page" pattern
  url <- stringr::str_subset(pages, paste0('rel="', page, '"'))

  # Take only the first match if multiple found
  if (length(url) > 1) {
    url <- url[1]
  }

  if (length(url) == 0) {
    return(NULL)
  }

  url <- stringr::str_extract(url, url_pattern)
  url <- stringr::str_replace_all(url, "[<>;]", "")
  return(url)
}


#' Enroll multiple users in a Canvas course as students
#' @param course_id The Canvas course ID
#' @param user_ids A vector of Canvas user IDs or SIS IDs
#' @param enrollment_type The type of enrollment (default: "StudentEnrollment")
#' @return A list of API responses
add_many_users_to_course <- function(course_id, user_ids, enrollment_type = "StudentEnrollment") {
  purrr::map(user_ids, function(user_id) {
    url <- make_canvas_url("courses", course_id, "enrollments")
    args <- list(
      "enrollment[user_id]" = user_id,
      "enrollment[type]" = enrollment_type,
      "enrollment[enrollment_state]" = "active"
    )
    tryCatch(
      {
        resp <- canvas_query(url, args, type = "POST")
        message(sprintf("Enrolled user %s", user_id))
        resp
      },
      error = function(e) {
        warning(sprintf("Failed to enroll user %s: %s", user_id, e$message))
        NULL
      }
    )
  })
}
