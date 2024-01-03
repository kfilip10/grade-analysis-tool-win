
#### Checking for grade thresholds on startup####
doc_folder <- "Grade Brief Generator"
grade_csv_name <- "GradeThresholds.csv"
check_grade_csv <- function() {
  if (!dir.exists(file.path(Sys.getenv("USERPROFILE"),"Documents",doc_folder))) {
    dir.create(file.path(Sys.getenv("USERPROFILE"),"Documents",doc_folder))
    #copy the gradethreshold csv from www folder to the new folder
    file.copy(from = paste0(getwd(),"/www/",grade_csv_name),
              to = file.path(Sys.getenv("USERPROFILE"),"Documents",doc_folder,grade_csv_name))
    grades.path <- file.path(Sys.getenv("USERPROFILE"),"Documents",doc_folder,grade_csv_name)
  }
  
  # check if the gradethreshold csv exists in the user documents folder, if it doesn't copy it from the www folder
  # if it does exist then get the path to the file for the program
  if (!file.exists(file.path(Sys.getenv("USERPROFILE"),"Documents",doc_folder,grade_csv_name))) {
    file.copy(from = paste0(getwd(),"/www/",grade_csv_name),to = file.path(Sys.getenv("USERPROFILE"),"Documents",doc_folder,grade_csv_name))
    grades.path <- file.path(Sys.getenv("USERPROFILE"),"Documents",doc_folder,grade_csv_name)
  } else {
    
    grades.path <- file.path(Sys.getenv("USERPROFILE"),"Documents",doc_folder,grade_csv_name)
  }
  return(grades.path)
}

grades.path <- check_grade_csv()

wd <- getwd() 
tempDir <- tempdir()

version.palette<-brewer.pal(6,"Dark2")
#grades.path <- paste0(wd,"/www/GradeThresholds.csv")
grades.csv <- read.csv(grades.path)
grades <- rev(as.vector(unlist(grades.csv[,1])))
grades.desc <- as.vector(unlist(grades.csv[,1]))
breaks <- rev(as.vector(unlist(grades.csv[,2])))
breaks.desc <- as.vector(unlist(grades.csv[,2]))
breaks[12] <- Inf #top end of range (A+ in course, exceeding 100%)
titlestyle <- " font-size: 22px; /* Change font size */
                background-color: #FFE08D; 
                color: #000000; 
                padding: 10px;
                border: 2px solid #ffffff;/* Add a border */
                border-radius: 5px;
                font-weight: bold; /* Make text bold */"
bodystyle <- " font-size: 16px; /* Change font size */
                color: #000000; 
                padding: 10px;
                border: 2px solid #ffffff;/* Add a border */"






sourceAllFilesInFolder <- function(folder_path) {
  file_list <- list.files(folder_path, pattern = "\\.R$", full.names = TRUE)
  for (file in file_list) {
    source(file)
  }
}

showErrorModal <- function(errorMsg) {
  showModal(
    modalDialog(
      title = "Error: Check your data follows the provided format and please Contact the POC.",
      errorMsg,
      footer = modalButton("Close")
    )
  )
}

letter_grade <- function(score, breaks, grades) {
  n <- length(breaks)
  for (i in 1:(n-1)) {
    if (score < breaks[i+1]) {
      grade <- grades[i] # found the grade the score belongs to
      break  # exit the for-loop
    }
  }
  grade
}

(function(req_file, install = T, update = F, silent = F) {
  
  # Read text file containing required packages
  req <- scan(req_file, character(), quiet = T)
  
  # Update packages
  if (update) {
    update.packages(repos = "https://cloud.r-project.org", ask = F)
  }
  
  # Install missing packages
  if (length(req) > 0 & install) {
    missing_packages <- req[!(req %in% installed.packages()[,"Package"])]
    if (length(missing_packages) > 0) {
      install.packages(
        missing_packages,
        repos = "https://cloud.r-project.org",
        dependencies = T,
        clean = T
      )
    }
  }
  
  # Additional stuff here
  # if (!webshot::is_phantomjs_installed()) {
  #   webshot::install_phantomjs()
  # }
  
  # Load packages
  if (silent) {
    suppressPackageStartupMessages(invisible(lapply(req, library, character.only = T)))
  } else {
    lapply(req, library, character.only = T)
  }
  
  
})(file.path(getwd(),"req.txt"), silent = F)
