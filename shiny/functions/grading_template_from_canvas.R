# Function to take API data and turn into excel template for grading
# takes a canvas API gradebook from 'canvasPrep.R'
# INPUT: 
# course_gradebook() reactive from canvasAPI.R
# assignment_list_df() reactive from canvasAPI.R
# event name from drop down selection from canvasAPI.R
# Returns dataframe with student ID, student name, max points, current points
#inputs: event name, roster_course_df(), assignment_list_df()
#Assume I select "Intro Survey" as my graded event
excel_template_from_canvas <- function(roster.course,assign.df,event.name){
    #roster.course is roster_course_df()
    #assign.df is assignment_list_df()
    #first I need the list of user IDs, instructors, user.sortable_name,Points,Max points,and the last 8 characters of name
    template.df <- roster.course %>% select(user_id, instructor, user.sortable_name, Points, `Max Points`, course_id, section,section_hour)
    #mutate the name again to remove the last character
    #template.df <- template.df %>% mutate(section = substr(section, 1, nchar(section)-1))
    #template.df <- template.df %>% mutate(percent = Points/`Max Points`)

    #now I need to get the assignment ID for the event name from the assign.df
    # and match it using the course_id in template.df
    assign.df <-  assign.df %>% rename(assignment_id = id)
    template.df <- template.df %>% 
    left_join(assign.df%>%filter(name==event.name)%>%select(course_id,assignment_id), by = "course_id")
    #Open the input template excel file in the www folder
    template_name <- "input template.xlsx"
    time_str <- format(Sys.time(), "%m%d_%H%M%S")
    new_temp_name <- paste0("Grading template",time_str,".xlsx")
    
    wb_path <- file.path(getwd(),'www',template_name)
    #tempDir <- tempdir() Defined globally
    # Create a copy of the file in the temporary directory
    if (file.exists(wb_path)) {
    #get string of current time
    file.copy(wb_path, file.path(tempDir, new_temp_name))
    # If the file already exists in the destination, use unique names to avoid overwriting:
    # file.copy(file_path, file.path(destination_dir, paste0("copied_", basename(file_path))))
    } else {
    print("Input File does not exist or cannot be accessed.")
    }
    template.path <- file.path(tempDir,new_temp_name)
    data <- readWorkbook(wb_path, sheet = 1)
    row_ID = 6

    wb <- loadWorkbook(template.path)
    #browser()
    #write student names
    #find the row that has id in the loaded workbook
    
    writeData(wb, sheet = 1, x = template.df$user_id, startRow = row_ID, startCol = 3, colNames = FALSE)
    writeData(wb, sheet = 1, x = template.df$instructor, startRow = row_ID, startCol = 4, colNames = FALSE)
    writeData(wb, sheet = 1, x = template.df$section, startRow = row_ID, startCol = 5, colNames = FALSE)
    writeData(wb, sheet = 1, x = template.df$section_hour, startRow = row_ID, startCol = 6, colNames = FALSE)
    writeData(wb, sheet = 1, x = template.df$user.sortable_name, startRow = row_ID, startCol = 7, colNames = FALSE)
    writeData(wb, sheet = 1, x = template.df$Points, startRow = row_ID, startCol = 18, colNames = FALSE)
    writeData(wb, sheet = 1, x = template.df$`Max Points`, startRow = row_ID, startCol = 19, colNames = FALSE)
    writeData(wb, sheet = 1, x = template.df$course_id, startRow = row_ID, startCol = 21, colNames = FALSE)
    writeData(wb, sheet = 1, x = template.df$assignment_id, startRow = row_ID, startCol = 22, colNames = FALSE)
    
    
    saveWorkbook(wb, template.path,overwrite=TRUE)
    
    #save gb.clean to a csv file in the temp directory
    write.csv(template.df, file.path(tempDir,"canvas combined.csv"), row.names = FALSE)
    
    str.list <- list()
    str.list[[1]] <- template.path
    str.list[[2]] <- NULL #not needed
    str.list[[3]] <- NULL #not needed
    str.list[[4]] <- file.path(tempDir,"canvas combined.csv")
    
    return(str.list)
}



