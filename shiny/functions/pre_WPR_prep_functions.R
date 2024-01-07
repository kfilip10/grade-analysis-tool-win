
# takes a canvas csv file and returns it as a dataframe
# INPUT: 
# CSV, 
# string for current points column, string for current score column
# Integer of row which has assignment names
# Integer of row which begins the student names (so that it is hopefully more future proof)
# needs to remove row 2 and 3
# then needs to remove 
# Returns dataframe with student ID, student name, max points, current points

import_canvas <- function(gb.csv, student.str,studentID.str,section.str,currPoints.str, currScore.str, rowStudents){
  
  gb <- read_csv(gb.csv)
  
  student.str.idx <- which(colnames(gb)==student.str)
  studentID.str.idx <- which(colnames(gb)==studentID.str)
  section.idx <- which(colnames(gb)==section.str)
  currPoints.idx <- which(colnames(gb)==currPoints.str)
  currScore.idx <- which(colnames(gb)==currScore.str)

  # check if any of index are 0 and stop the function and return which are zero
  check_zero <- function(idx, str){
    if(idx==0){
      stop(paste0("The provided column name for ",str," was not found in the csv file:",gb.csv))
    }
  }
  check_zero(student.str.idx,student.str)
  check_zero(studentID.str.idx,studentID.str)
  check_zero(section.idx,section.str)
  check_zero(currPoints.idx,currPoints.str)
  check_zero(currScore.idx,currScore.str)
  
  #remove rows until the first student record
  gb <- gb[-seq(1, rowStudents-2),]
  
  #remove the first 5 characters of the section name
  gb[[section.str]] <- substr(gb[[section.str]], 6, nchar(gb[[section.str]]))
  
  #remove any spaces until first lette in section name
  gb[[section.str]] <- gsub("^\\s+", "", gb[[section.str]])
  
  #remove all columns except the first two and the current points and score
  gb <- gb[,c(student.str.idx,studentID.str.idx,section.idx,currPoints.idx,currScore.idx)]
  

  
  #check if a student is named 'Test Student' and remove them
  if(any(gb$Student=="Student, Test")){
    gb <- gb[-which(gb$Student=="Student, Test"),]
  }
  #change the currPoints.idx to a numeric
  currPoints.idx <- which(colnames(gb)==currPoints.str)
  currScore.idx <- which(colnames(gb)==currScore.str)
  columns_vector <- c(currPoints.str, currScore.str)
  
  for (col in columns_vector) {
    gb[[col]] <- as.numeric(as.character(gb[[col]]))
  }
  

  
  #return the dataframe
  return(gb)
}


#template <- prepare_template(gb, studentStr(), studentIDStr(),sectionstr(), currPointsStr(), currScoreStr())
# Takes a combined canvas gradebook and strings from the user for column IDs
# returns
# 1. path to the template
# 2. string summarizing the IDs combined
# 3. string summarizing any duplicate IDs
# 4. path to combined csv file
prepare_template <- function(gb,student.str,studentID.str,section.str,currPoints.str, currScore.str,tempDir){
  
  student.str.idx <- which(colnames(gb)==student.str)
  studentID.str.idx <- which(colnames(gb)==studentID.str)
  section.idx <- which(colnames(gb)==section.str)
  currPoints.idx <- which(colnames(gb)==currPoints.str)
  currScore.idx <- which(colnames(gb)==currScore.str)
  
  #divide the current points by the score to get the max points
  gb[,ncol(gb)+1] <- round(gb[,currPoints.idx]/gb[,currScore.idx]*100,1)
  #change the column name to "max points"
  colnames(gb)[ncol(gb)] <- "Max Points"
  
  #remove duplicates from gradebook
  gb.clean <- gb[!duplicated(gb[,studentID.str.idx]),]
  gb.dupl <- gb[duplicated(gb[,studentID.str.idx]),]
  
  section.dupl <- unique(gb.dupl[section.str])
  dupl.str.section <- paste0(unlist(section.dupl), collapse = " | ")
  
  dupl.str.count <- nrow(gb.dupl)
  
  #get the data on numbers of duplicates for modal dialog on return.
  duplicate.str <- paste0(dupl.str.count, " duplicated student IDs were found and removed for the grading template. These IDs came from the following sections which were likely uploaded in error: ", dupl.str.section)
  sum.str <- paste0("A total of ", nrow(gb.clean), " students were found in the gradebooks 
                    and added to the template from ",  nrow(unique(gb.clean[,section.idx]))," sections.")
  #print(sum.str)
    #Open the input template excel file in the www folder
  template_name <- "input template.xlsx"
  time_str <- format(Sys.time(), "%m%d_%H%M%S")
  new_temp_name <- paste0("Grading template",time_str,".xlsx")
  
  wb_path <- file.path(getwd(),'www',template_name)
  
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
  wb <- loadWorkbook(template.path)
  
  # Get the names of sheets in the workbook
  
  #write student names
  writeData(wb, sheet = 1, x = gb.clean[[studentID.str]], startRow = 10, startCol = 3, colNames = FALSE)
  writeData(wb, sheet = 1, x = gb.clean[[section.str]], startRow = 10, startCol = 4, colNames = FALSE)
  writeData(wb, sheet = 1, x = gb.clean[[student.str]], startRow = 10, startCol = 5, colNames = FALSE)
  writeData(wb, sheet = 1, x = gb.clean[[currPoints.str]], startRow = 10, startCol = 16, colNames = FALSE)
  writeData(wb, sheet = 1, x = gb.clean[ncol(gb.clean)], startRow = 10, startCol = 17, colNames = FALSE)
  
  #protectWorksheet(wb, sheet = "v1", protect = TRUE, lockSelectingUnlockedCells = FALSE,
  #                 lockFormattingCells = FALSE, lockFormattingColumns = FALSE,
  #                 lockFormattingRows = FALSE,
  #                 lockInsertingColumns = FALSE, lockInsertingRows = FALSE, 
  #               lockDeletingColumns = FALSE, lockDeletingRows = FALSE,
  #                lockSorting = FALSE, lockAutoFilter = FALSE, lockPivotTables = FALSE
  #                )
  
  saveWorkbook(wb, template.path,overwrite=TRUE)
  #save gb.clean to a csv file in the temp directory
  write.csv(gb.clean, file.path(tempDir,"canvas combined.csv"), row.names = FALSE)
  
  str.list <- list()
  str.list[[1]] <- template.path
  str.list[[2]] <- sum.str
  str.list[[3]] <- duplicate.str
  str.list[[4]] <- file.path(tempDir,"canvas combined.csv")
  
  return(str.list)
  
  
}