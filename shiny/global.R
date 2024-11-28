# The global.R script contains variables used across the app

wd <- getwd() 
tempDir <- tempdir()

#### Grade thresholds - Checking for grade thresholds on startup####
settings_folder <- "Grade Analysis Tool Settings"
settings_path <- file.path(Sys.getenv("USERPROFILE"),"Documents",settings_folder)
if (!dir.exists(settings_path)) {
  dir.create(settings_path)
}
grade_csv_name <- "GradeThresholds.csv"
grade_csv_path <- file.path(Sys.getenv("USERPROFILE"),"Documents",settings_folder,grade_csv_name)

#List of courses to save for easy reloading
COURSE_DEFAULTS <- "COURSE_DEFAULTS.rds"
COURSE_DEFAULTS_PATH <- file.path(settings_path,COURSE_DEFAULTS)

SECTION_DEFAULTS <- "SECTION_DEFAULTS.rds"
SECTION_DEFAULTS_PATH <- file.path(settings_path,SECTION_DEFAULTS)

#List of assignment data to save for easy reloading
ASSIGNMENT_DEFAULTS <- "ASSIGNMENT_DEFAULTS.rds"
ASSIGNMENT_DEFAULTS_PATH <- file.path(settings_path,ASSIGNMENT_DEFAULTS)


#This function checks if the GradeThresholds.csv is in the user documents folder, if it isn't then it adds it


check_grade_csv <- function() {
  # check if the gradethreshold csv exists in the user documents folder, if it doesn't copy it from the www folder
  # if it does exist then get the path to the file for the program
  if (!file.exists(grade_csv_path)) {
    file.copy(from = file.path(getwd(),"www",grade_csv_name),
              to = file.path(grade_csv_path))
    #return(TRUE)
  } else {
    #return(TRUE)
  }
}

#Load csv into settings folder and then define the grades and breaks vectors for use in the app
check_grade_csv()
grades.csv <- read.csv(grade_csv_path)
grades <- rev(as.vector(unlist(grades.csv[,1])))
grades.desc <- as.vector(unlist(grades.csv[,1]))
breaks <- rev(as.vector(unlist(grades.csv[,2])))
breaks.desc <- as.vector(unlist(grades.csv[,2]))
breaks[12] <- Inf #top end of range (A+ in course, exceeding 100%)




#### Themes and Styles ####
#palette for numerous versions of a test. 
version_palette<-brewer.pal(8,"Dark2")

#Some css / style variables
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


#### GLobal Parameters ####
bin.width <- 4
INSTRUCTOR_SEARCH_KEY <- "instr"

#### Global Functions ####
#function to source all subfiles
#folder_path <- file.path(getwd(),"functions","canvas api")
sourceAllFilesInFolder <- function(folder_path) {
  file_list <- list.files(folder_path, pattern = "\\.R$", full.names = TRUE)
  for (file in file_list) {
    source(file)
  }
}

#function to show an error modal
showErrorModal <- function(errorMsg) {
  showModal(
    modalDialog(
      title = "Error Encountered",
      errorMsg,
      footer = modalButton("Close")
    )
  )
}
canvas_api_token_path <- file.path(settings_path,"token.rds")
#check if file exists
if (!file.exists(canvas_api_token_path)) {
  saveRDS("", canvas_api_token_path)
} 
api_domain <- "https://westpoint.instructure.com"


#### Letter grade function ####
#function to assign a grade based on a percent score
#uses the breaks and grades vectors from the GradeThresholds.csv file
#function edited from a StackOverflow post that I can't seem to find the link to anymore
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

#### Install Packages ####
#Installs package dependencies based on list in req.txt
#parenthesis are used to make an anonymous function and immediately call it
#from: https://github.com/derryleng/Shiny_Desktop_App
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
  
  
  # Load packages
  if (silent) {
    suppressPackageStartupMessages(invisible(lapply(req, library, character.only = T)))
  } else {
    lapply(req, library, character.only = T)
  }
  
  
})(file.path(getwd(),"req.txt"), silent = F)#This line calls the function with those arguments
