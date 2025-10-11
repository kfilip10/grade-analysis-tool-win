#' 
#' 
gs_load_courses <- function(connection_status){
      #check if there is a file at SECTION_DEFAULTS_PATH
    if(file.exists(SECTION_DEFAULTS_PATH)){
      #check the date the file was written
      file_date <- file.info(SECTION_DEFAULTS_PATH)$mtime
      file_date <- as.Date(file_date)
      #if it is older than 60 days then ask if they want to reload the data
      if (Sys.Date() - file_date > 10) {
        #print("File is older than 14 days")
        gs_load_section_start()
      } else {
        #if it is within 60 days then load the data
        return(readRDS(SECTION_DEFAULTS_PATH))
      }
    } else {
      #if there isn't then get the section list data
      gs_load_section_start()
    }


}

gs_load_section_start<- function(){
  #course_list_df has the courses
  #includes: sections - dataframe of sections in each course
  #total students: total number of students in each course
  #open a box that says 'Loading course data, please wait' that also has a with progress
  # Open a modal dialog before loading data
  showModal(modalDialog(
    title = "Loading",
    "Loading course data, please wait...",
    footer = NULL,
    easyClose = FALSE
  ))
  withProgress(message = 'Loading course data, please wait...', value = 0, {
    course_list_df <- get_course_list(include=c("sections","total_students","term"))


    sections <- lapply(course_list_df$id,function(x){
      incProgress(1/length(unique(course_list_df$id)),
                  detail = paste("Loading course data for course", x))
      get_section_info(x,include = "total_students")

    })


    #end_time <- Sys.time()
    #end_time - start_time
    sections.comb <- bind_rows(sections)

    #filter no student courses and non-USMA courses
    sections.comb <- sections.comb %>% filter(!total_students == 0, !is.na(sis_section_id))
    #browser()
    ##Creating dataframe of sections##
    #'name' - section name
    #'total_students' - total number of students enrolled in the section
    #'section_id' section id from get_section_info 'id'
    # rename id in sections to section_id
    names(sections.comb)[names(sections.comb) == "id"] <- "section_id"
    #'#'id' course id - from id
    names(sections.comb)[names(sections.comb) == "course_id"] <- "id"

    #'course' course name extracted from name
    sections.comb <- sections.comb %>% mutate(course = str_extract(sis_section_id, "[A-Z]{2}[0-9]{3}"))

    #'# 'term' - from course_list_df term.name
    #Extract the term from course_list_df term.name by matching id
    sections.comb <- sections.comb %>% left_join(course_list_df %>%select(id,term.name), by = "id")
    names(sections.comb)[names(sections.comb) == "term.name"] <- "term"

    hours <- sections.comb$name %>% str_extract("\\b[A-Z0-9]{4}\\b\\s+\\d+")
    section <- str_extract(hours, "\\d+\\s*$")
    hours <- str_extract(hours, "\\b[A-Z0-9]{4}\\b")
    #If section is 'NA' then label as 'NA'
    # 'section_hour' - extracted hour from section name 'A1B1'
    # 'section' - section number from section name '3' or '4'
    sections.comb <- sections.comb %>% mutate(section_hour = hours, section = section)
    sections.comb <- sections.comb %>% mutate(instr_name = str_extract(name, "(?<=-)[^-]+$"))
    #returns sections.comb
    #'name' - section name
    #'id' course id - from id
    #'course' course name extracted from name
    #'total_students' - total number of students enrolled in the section
    #'section_id' section id from get_section_info 'id'
    #'# 'term' - from course_list_df term.name
    # 'section_hour' - extracted hour from section name 'A1B1'
    # 'section' - section number from section name '3' or '4'
    # 'instr_name' - name of instructor from section name
  })
  removeModal()
  #save sections.comb to section_defaults_path defined in global.r
  saveRDS(sections.comb, SECTION_DEFAULTS_PATH)
  return(sections.comb)



}



#'   
#'   