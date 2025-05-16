# 1 PANE Grade Brief Tool 
`
[[Course director  and lab director page]]
[[Data Viz Report for WPRs]]

Electron: [[yarn]]
- Debugging in browser()
saveRDS(df,file ="env/uploaded template graded.rds")

# 2 Overview

This R Shiny application serves as a powerful tool for analyzing and visualizing exam grade data. It takes in raw exam scores grouped by question points, performs analysis, and generates a comprehensive PowerPoint presentation summarizing the results.

## 2.1 Features

- **Data Input**: Allows the user to upload exam grade data in Excel format.
- **Analysis**: Performs statistical analysis and visualization of the exam grade distributions.
- **Presentation Generation**: Generates a PowerPoint presentation containing summary charts, graphs, and insights.

## 2.2 Installation
1. Download the .exe setup file from the following link
2. Install on your local machine
3. Run the program, on first startup it will download the required packages for R Studio from CRAN (an active internet connection is required).

## 2.3 Usage

1. **Upload Data**: 
2. **Data Analysis**: Explore various visualizations and statistical summaries available based on the uploaded data.
3. **Generate Presentation**: Once satisfied with the analysis, click on the "Generate Presentation" button to create a PowerPoint file.
5. **Download**: Download the generated PowerPoint file for distribution within the Physics department.

## 2.4 WPR Workflow

### 2.4.1 Excel template
- Cadet Admin Data
	- ID - canvas ID. This is not used in the brief but is useful for data validation during grade data entry in large enrollment courses (core)
	- Course - course identifier. This is only used in the brief if it is a core course (201/251)

# 3 Program Details

## 3.1 Roadmap
- [ ] Mac deployability
	- [ ] Updated file pathing to be OS agnostic, still needs testing.


## 3.3 Issues

### 3.3.1 Misc
- [ ] save assignment data doesn't work first time, need to add a check in global to check if the file is there and write to file if it is not
- [ ] downloading the wpr template didn't work, not sure why.
	- [ ] Check excel template is reading correctly since I think I changed it slightly
- [ ] issue where R launches while updater is going
- [ ] assignment group stats
	- [ ] remove side panel
	- [ ] add filtering by assignment
	- [ ] add grouping by instructor 
	- [ ] edit table displayed in table (round and make percents)
- [ ] Add course grade stats table maybe?`
- [ ] some issues on R not closing after an error
- [ ] display dataframe to be uploaded before uploading to canvas
- [ ] sometimes on first launch it will hang up, add a timer in the autoupdater so it proceeds with launch if that hangs it up
- [ ] Think through workflow of building OML, either we upload WPR grades and then redownload a gradebook or we manually calc in excel, I think uploading grades and then pull gradebook is the better option. 

- [ ] Fix errors on first load
	- [ ] seems like error is from uncaught reference erro: require is not defined in loading.html:23:45
Error on startup:
![[Pasted image 20240318154540.png]]
Loads ok:
![[Pasted image 20240318154609.png]]

## 3.4 Latest Dev Notes
- 
- Edit API Token not working on Laptop
	- My guess is it has to do with writing to a folder or file that doesn't exist when it launches
	- It creates the Grade Analysis Tool Settings fine
	- But there is no API token .rds file in there. I probably need to create a NULL one if it doesn't exist (in global.R) and then when I load edit API 
- Finally got the app to a good working state and can upload WPR grades from the template.
- Need to add graph and table for gradebook data
- If there are no assignments with the same name I could instead print out the assignment groupings? Or I could put in a search bar to populate assignments that way
- Next issue is API calls in the electron app
	- When I yarn start it was not connecting
	- I commented out the webrequest section but it didn't help
	- I am next trying to build first and see if it just doesn't work in a test environment
		- no luck
	- Now let me see if it has anything to do with the spinner call


THoughts but not needed:
- Maybe I should change when it pulls in canvas data? Like a button that says 'confirm course and assignment listing', 'Load Canvas Data'
	- This will get the gb.comb.parse set as course_gradebook
- Then the dropdown can be:
	- download gradebook
		- make gb.xl a reactive
	- download WPR Template
		- make a different reactive for tha
- Or the user has the option to use graph or table viewer
### 3.4.1 Graphing
- Basic funcitonality works, but isn't useful yet
	- assignment group ids need to be selected by name
	- Courses need to display by instructor,hour
	- Box and whisker is probably a better indicator
- Need to load into dev environment and get exactly what I want and then port in that code
	- course_gradebook() is the reactive that has all the assignment group info
	- It is currently defined once the user clicks 'download grade data'
- 

### 3.4.2 Course gradebook
Problem: I need to efficiently pull all cadets and get relevant course statistics for them:
	- Overall score
		- used for 
	- SIS ID
	- Score for events of interest
		- But each WPR has a different assignment ID
		- so would need to pull all assignments, filter by name and then make a list of assignment IDs for WPRX
		- Then when I pull cadet data I would need to match to %in% that list of IDs
I will likely need to use this : https://canvas.instructure.com/doc/api/submissions.html#method.submissions_api.for_students
to get the submissions for each student
use the include[]=user to have the student name info
Then I just need to efficiently do that
the built in gradebook function works and is worth investigating but took almost a second per student which is too long.
https://community.canvaslms.com/t5/Canvas-Developers-Group/Export-Individual-Course-Gradebook-Via-API/m-p/180764
#### 3.4.2.1 Parallel calls May help with speed 
	1. `library(parallel)`
    
1. **Define Your API Call Function**: This function will make individual API calls. Ensure this function is self-contained (i.e., it should load any necessary libraries and not rely on objects from the global environment).
    
    RCopy code
    
    `api_call <- function(url, token) {   library(httr)    response <- GET(url, add_headers(Authorization = paste("Bearer", token)))   if (status_code(response) == 200) {     return(content(response, "text", encoding = "UTF-8"))   } else {     return(NULL)   } }`
    
2. **Set Up URLs and Tokens for Each Call**: Prepare the list of URLs and tokens for each API call.
    
    RCopy code
    
    `urls <- c("http://api.example.com/data1", "http://api.example.com/data2")  # Replace with actual URLs tokens <- rep("<Your-Access-Token>", length(urls))`
    
3. **Use `mclapply` or `parLapply` for Parallel Execution**: Use `mclapply` (on Unix-like systems) or set up a cluster and use `parLapply` (on any system, including Windows) to perform the API calls in parallel.
    
    RCopy code
    
    `# Using mclapply results <- mclapply(seq_along(urls), function(i) api_call(urls[i], tokens[i]), mc.cores = detectCores())  # Or using parLapply cl <- makeCluster(detectCores()) clusterExport(cl, c("api_call", "urls", "tokens"))  # Export necessary objects to the cluster results <- parLapply(cl, seq_along(urls), function(i) api_call(urls[i], tokens[i])) stopCluster(cl)`

### 3.4.3 WPR Template


### 3.4.4 Assignment Listings?
https://canvas.instructure.com/doc/api/analytics.html#method.analytics_api.course_assignments

 [Get course-level student summary data](https://canvas.instructure.com/doc/api/analytics.html#method.analytics_api.course_student_summaries)

 GET /api/v1/courses/:course_id/analytics/student_summaries

**Scope:** `url:GET|/api/v1/courses/:course_id/analytics/student_summaries`

Returns a summary of per-user access information for all students in a course. This includes total page views, total participations, and a breakdown of on-time/late status for all homework submissions in the course.

Each student’s summary also includes the maximum number of page views and participations by any student in the course, which may be useful for some visualizations (since determining maximums client side can be tricky with pagination).
## 3.5 Features in progress
- [x] Comment code better ✅ 2024-01-06
- [ ] Update to read using Cadet ID?
### 3.5.1 Canvas integration
- Added api tab
	- Added setting for API Key to be saved
	- Added logic to test if API key is working
	- Loading wheel works
	- Reactive value passing the api.key is updated
- added API code adopted and edited from rcanvas 
- [ ] add settings for api domain
- [ ] API tab allows user to
		- Added a button to select the courses that are of interest
		- [ ]  Adding a combo box to select which feature to perform.
		- [ ]
	- combine gradebooks
		- pulls their course list
		- let the user select which courses they'd like to combine
		- Download a csv of a merged gradebook
- For WPR Template I will need to have a hidden column for that student's assignment ID and their userID so I can update grade automatically
- 

### 3.5.2 Electron integration
- Shiny app now deploys as an electron app
- includes 

### 3.5.3 General 
- Removed grade bin width parameter
- Increased max file size upload to 30MB for cut sheets
- [ ] Canvas token way to check if it is expired
- This one is expired: 15453~djjwyPsspgsEZhJ4KYKCAMc8H3aXcd7Irz3mKXMcicQPRXREcSB7xWiXgXhPOCrO
- Added 'restore to default grade values' in settings
	- Reloads the grades.threshold file from www folder
- [ ] improve error handling for incomplete grading template files
	- what if a field is missing
- [ ] return csv's for uploading grades by finding instructor names and iterating through and returning a 
- [ ] update brief functions to rely on function calls passing ppt and adding slides
	- this will help me improve the manual briefing function
- [ ] fix manual backup to call same make_ppt function and use same excel template
- [ ] Allow WPR plot to group by section
- [ ]  add Tab for Canvas Gradebook combination (also OML eventually)
		- Take multiple CSV
		- read the headers in each one
		- make checkboxes for similar headers
		- prompt the user to select the ones they want
		- then combine by selecting those headers from each csv
		- give option to download the file
		- Then upload additional files 
- Canvas Gradebook data viz to sum all the shared columns and 
- [ ] function to download grades for canvas entry
- [ ] fixed grades.path not being used correctly after implementing settinsg update.


# 4 Explanation of Program Components
### 4.1.1 Electron 
Electron is an app framework that uses Javascript to deploy apps via a built in browser. It is designed to be easily configurable for cross platform support.
### 4.1.2 app.R



### 4.1.3 dev.R


### 4.1.4 global.R


### 4.1.5 pages.R


### 4.1.6 reactive_logic.R




### 4.1.7 manual_ppt.R
- This is a 'manual' backup to be able to generate a grade brief even if Canvas messes up.
- It provides a template excel file for the user to populate and runs what was the V0.5 version of the brief 




## 4.2 V1.1
### 4.2.1 Updates
- Changed deployment to enable future cross platform support (MAC/Windows)
- Implemented automatic updates from a GitHub repository
- 
### 4.2.2 Bugs

# 5 Previous Versions
## 5.1 V1.

### 5.1.1 Known issues

#### 5.1.1.1 Manual entry needs overhaul
- match current excel template (just no need for canvas ID stuff)
- call current ppt function just without certain slides
- manual entry needs pre/post max points in the columns
- manual entry will fail without a good error if the first sheet isn't 'v1' or something with a 1 in it
	- add in the sheet selector into it
- download data for canvas entry not working on manual page
- manual entry plots are not formatted correctly
- 

### 5.1.2 Bug Fixes
- incorrect pathing to grade csv file 
	- updated to point to same documents folder that the editable one is in
- 


## 5.2 V0.7
The biggest improvement is that now you can upload Canvas gradebooks (or multiple, in anticipation of next semester) and it will create the WPR Grading template for you based on who is in the course.

Then you can upload that grading template to create the brief. It saves a lot of the manual labor building the template, copying data over, matching names etc. It also tells you if duplicate entries were found (like one person graded for both versions). 
  
For the brief itself

- Updated the min/max for versions
- Added a 'what was the grade distribution of the population before they took an exam' slide
- Added a slide that summarizes which cadets that are in your course did not take either version.

  

There are some other UI improvements and an ability to adjust the grade cutoffs but not really needed right now.

### 5.2.1 Features / To-do
- [x] max/min on version page ✅ 2023-12-09



#### 5.2.1.1 Read Canvas and prep for exam
- Code imports and combines multiple canvas gradebooks
- Next need to write those to the new template
	- leave in the pre/post points to the right in the template
- User has to manually clone versions based on how many they have.
- 

#### 5.2.1.2 Check IDs / students taking the test status report slide
- It also can find which students have no entry

#### 5.2.1.3 Report Generation
- add T/F if it is a standard canvas or manual
	- if canvas then do the function to make the report slide
	- If not then don't do the report generation slide

	- include t test
	- probably a separate function?
	- 

#### 5.2.1.4 Backup briefing
- Simpler format that allows a user to build a template with a standard excel template
- update template download that's just blank
- This is a backup / rudimentary approach that does not rely on canvas gradebook functionality
- 
#### 5.2.1.5 QOL Improvements
- Allow users to update the grade range
	- on open check for grade data
		- place code in hte
	- if no grade data write .csv with default values to Users/%user%/PANE Grade Brief Tool
	- else read what is already there
	- Add button to update grade brief values
		[[R saving reactive values]]
- Improve overall aesthetics
	- icons: https://rstudio.github.io/shiny/reference/icon.html
	- themes: https://bootswatch.com/
#### 5.2.1.6 Installation
- Update inno .exe icon to the PANE Crest
- check it updates when installed (to same .exe)


## 5.3 V0.6
- 6DEC23
- Limited release for AY24-2 WPR3 in PHYS 201 and 202
- Improvements:
	- Added basic UI functionality
	- Added optional cut sheet uploads
	- Added tabbed summary pages for future plot and data visualization
	- Deployed .exe to additional users for testing
- Bugs/improvements:
	- Uploading questions with numeric values resulted in incorrect excel importing
	- Excel sheet uploaded required additional canvas data manipulation
## 5.4 V0.5
- 25OCT23
- Initial release for AY24-1 WPR2 in PHYS201
- Allowed upload of excel file and download of ppt brief
- Minimal functionality and proof of concept
- Results: Improve user experience and update brief format

## 5.5 V0.1 to 0.5
- Local test of standalone R code in R Studio
- Implementation of R Shiny applet

# 6 Developing Electron apps
To start developing electron apps you will need to configure your computer with the following tools
- node.js - extensive library to utilize when building the apps
- npm or yarn - package managers required for most electron apps
- choco - used for updating R installations in your local folder
- innoextract - part of choco
- Cygwin - used to install R and leverage innoextract tools
- Visual Studio Code - easy IDE for editing files and running terminal commands all in one place
- 

1. Install node.js, npm, choco, innoextract, Cygwin
2. Make a directory
3. Clone yarn2 repository
	1. Update package.json to app name
	2. Optionally make a github repo to push updates 
4. Copy over the R files
	1. Shiny files
	2. R directory
	3. run updater for packages
5. check src files 
	1. don't copy over index.js exactly, add in updating functionality
6. Run `yarn start`
	1. Errors are expected, install dependencies as indicated.
7. package
8. Make Github repo

## 6.1 Contributors

## 6.2 References

1. Framework for deploying as .exe
	1. https://github.com/derryleng/Shiny_Desktop_App by Dr. Lee Pang
2. deploying with electron, multiplatform
	1. https://github.com/lawalter/r-shiny-electron-app?tab=readme-ov-file
3. Multiple code snippets generated by Chat GPT and adapted for use and implementation
4. grade binning function
	1. 

## 6.3 License

This project is licensed under the [MIT License](https://chat.openai.com/c/LICENSE).

