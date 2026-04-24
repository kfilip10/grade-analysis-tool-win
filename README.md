# Grade Analysis Tool (Windows)

A desktop application that automates grade distribution analysis for courses and exams. It wraps an **R Shiny** web application inside an **Electron** shell so end users get a native `.exe` installer with no need to install R separately. The app interfaces with **Canvas LMS** and **Gradescope** APIs to pull gradebook data, compute statistics, and generate PowerPoint briefing slides.

---

## Table of Contents

- [Overview](#overview)
- [Architecture](#architecture)
  - [Startup Sequence](#startup-sequence)
  - [Electron Layer (`src/`)](#electron-layer-src)
  - [R Shiny Layer (`shiny/`)](#r-shiny-layer-shiny)
  - [Bundled R Runtime (`r-win/`)](#bundled-r-runtime-r-win)
- [Shiny App Structure](#shiny-app-structure)
- [Build and Release](#build-and-release)
- [R Library Packaging and Update Checklist](#r-library-packaging-and-update-checklist)
- [Troubleshooting Guide](#troubleshooting-guide)
- [Installation and Usage](#installation-and-usage)

---

## Overview

| Component | Technology | Purpose |
|-----------|-----------|---------|
| Desktop shell | Electron 28 | Window management, auto-updates, process lifecycle |
| Backend server | R 4.3.2 + Shiny | Data processing, Canvas/Gradescope API calls, report generation |
| Packaging | electron-builder + NSIS | Produces a one-click Windows installer |
| Auto-update | electron-updater | GitHub Releases-based OTA updates |

The user launches the `.exe`. Electron spawns a local R process that starts a Shiny server on a random port. Once the server responds, Electron loads that URL in a `BrowserWindow`. All R packages are pre-bundled in `r-win/library/` so there is no install step for the user.

---

## Architecture

```
┌──────────────────────────────────────────────────────┐
│                   Electron (main.js)                 │
│  - Spawns R child process                            │
│  - Shows loading splash screen                       │
│  - Checks for auto-updates (GitHub Releases)         │
│  - Manages PID file for cleanup on crash/relaunch    │
│  - IPC: kill-server, restart-app, toggle-r-console   │
├──────────────────┬───────────────────────────────────┤
│  Loading Screen  │  BrowserWindow → http://127.0.0.1:PORT │
│  (loading.html)  │  (renders Shiny UI)               │
└──────────────────┴──────────┬────────────────────────┘
                              │ HTTP (localhost)
                   ┌──────────▼──────────┐
                   │   R Shiny Server    │
                   │   (start-shiny.R)   │
                   │                     │
                   │  shiny/app.R        │
                   │  shiny/global.R     │
                   │  shiny/handlers/    │
                   │  shiny/functions/   │
                   └─────────────────────┘
```

### Startup Sequence

1. **Electron `app.on('ready')`** &mdash; kills any orphaned R processes from a previous bad shutdown (reads PIDs from `rPIDs.txt` in `userData`).
2. **Auto-update check** &mdash; `electron-updater` checks GitHub Releases. If an update is found it downloads, quits, and installs before proceeding.
3. **`tryStartWebserver()`** &mdash; picks a random available port (3000–8000), spawns `r-win/bin/R --vanilla -f start-shiny.R` with environment variables pointing to the bundled library and the `shiny/` directory.
4. **Polling loop** &mdash; sends `HEAD` requests to `http://127.0.0.1:<port>` up to 15 times over ~30 seconds. On success the loading splash is replaced with the main `BrowserWindow` loading the Shiny URL.
5. **Timeout** &mdash; if the server hasn't responded in 90 seconds a dialog offers to restart or close.

### Electron Layer (`src/`)

| File | Role |
|------|------|
| `main.js` | Application entry point. Spawns R, manages windows, IPC handlers, auto-updater, menu, PID tracking. |
| `index.js` | ESM re-export of `main.js`. |
| `preload.js` | Context bridge exposing `electronAPI` to renderer (kill-server, restart, toggle R console, loading events). |
| `helpers.js` | `randomPort()`, `waitFor()`, `getRPath()` — utility functions for R process management. |
| `loading.html / loading.css` | Splash screen shown while R boots. Displays connection attempt progress. |
| `failed.html` | Error screen shown if R fails to start. |
| `update.html / loading_download.html` | UI for auto-update download progress. |

Key IPC channels:
- `kill-server` — renderer asks main to quit the app.
- `restart-app` — kills R, relaunches Electron.
- `toggle-r-console` — restarts R with/without a visible console window (debug menu).

### R Shiny Layer (`shiny/`)

| File / Folder | Role |
|---------------|------|
| `app.R` | Top-level Shiny UI + server definition. Loads all handlers and functions via `sourceAllFilesInFolder()`. |
| `global.R` | Shared state: grade thresholds, settings paths, package loading from `req.txt`, global helper functions. |
| `req.txt` | Plain-text list of CRAN package names the app depends on. |
| `handlers/` | Shiny module files — each contains both UI and server logic for a tab (Home, Canvas, Gradescope, Settings, FAQ, etc.). |
| `handlers/Gradescope/` | Gradescope-specific handler modules (prep, scores, canvas integration, cuts, brief creation, grade upload). |
| `functions/` | Pure business logic split by domain. |
| `functions/canvas api/` | Canvas LMS REST API wrappers (courses, rosters, assignments, gradebook, bulk grading). |
| `functions/canvas UI and local functions/` | UI helpers for Canvas connectivity and course/section table display. |
| `www/` | Static assets served by Shiny (images, templates, grade threshold CSV, PowerPoint templates). |
| `dev.R` | Developer scratch file for manual debugging — not used in production. |

### Bundled R Runtime (`r-win/`)

A **portable, self-contained R 4.3.2 installation** committed directly into the repo. It includes:

- `bin/` — R executable and supporting binaries.
- `library/` — **Pre-installed CRAN packages** (all dependencies listed in `req.txt` plus transitive deps). This is the critical piece that makes the app work without the user installing R or any packages.
- `etc/`, `include/`, `modules/`, `share/`, `src/`, `Tcl/` — standard R runtime support files.

The Electron main process sets the following environment variables so R uses only the bundled library:
```
R_LIBS       = r-win/library
R_LIBS_USER  = r-win/library
R_LIBS_SITE  = r-win/library
R_LIB_PATHS  = r-win/library
```

`start-shiny.R` pins `.lib.loc` to `R_LIB_PATHS` at startup, ensuring R never looks at a user-level library.

---

## Shiny App Structure

The Shiny app is organized as a modular single-file app (`app.R`) with dynamically sourced handlers:

```
shiny/
├── app.R                    # UI layout (navbarPage with tabs) + server wiring
├── global.R                 # Shared config, grade thresholds, package loader
├── req.txt                  # Package dependency list
├── handlers/
│   ├── Home Page API.R      # Home/landing page
│   ├── Canvas API Handler.R # Canvas gradebook tab logic
│   ├── Create Brief With Canvas Handler.R
│   ├── settings.R           # Settings page (API tokens, grade thresholds)
│   ├── FAQ.R
│   └── Gradescope/          # All Gradescope workflow tabs
│       ├── gs_prep.R
│       ├── gs_scores.R
│       ├── gs_canvas.R
│       ├── gs_cuts.R
│       ├── gs_createbrief.R
│       └── gs_updategrades.R
├── functions/
│   ├── brief_from_gradescope.R
│   ├── brief_manual_functions.R
│   ├── grading_template_from_canvas.R
│   ├── pre_WPR_prep_functions.R
│   ├── Brief from Canvas API Functions.R
│   ├── canvas api/          # REST wrappers for Canvas LMS
│   │   ├── api_utils.R
│   │   ├── get_course_list.R
│   │   ├── get_course_gradebook.R
│   │   ├── get_student_roster.R
│   │   ├── grade_assignments_bulk.R
│   │   └── ... (other API endpoints)
│   └── canvas UI and local functions/
│       ├── connection_ui.R
│       ├── load_courses.R
│       └── table_courses.R
└── www/                     # Static assets
    ├── PANE.png             # App logo
    ├── template.pptx        # PowerPoint template for briefs
    ├── template_gs.pptx     # Gradescope-specific template
    ├── GradeThresholds.csv  # Default grade scale
    └── input template.xlsx
```

**Main app tabs:**
1. **Home** — landing page with connectivity status.
2. **Canvas Gradebook** — pull gradebook from Canvas API, view assignment group stats, course grade stats, upload grades back.
3. **Gradescope Brief** — multi-step workflow: preparation → input scores → Canvas access → cut data → generate brief → upload scores.
4. **Manual Brief** — upload data manually, create brief, view distributions and version summaries.
5. **Settings** — Canvas API token management, grade threshold configuration.
6. **FAQ** — help content.

User settings are stored in `~/Documents/Grade Analysis Tool Settings/` (grade thresholds, Canvas API token, course/section/assignment defaults).

---

## Build and Release

**Prerequisites:** Node.js, Yarn, and a GitHub token for publishing.

For more details on deploying the app go to:
 Sharepoint -> Core Physics -> Faculty Development -> Course Director Academy -> Grade Analysis Tool

```bash
# Install Node dependencies
yarn install

# Run in development
yarn start

# Build installer (local only)
yarn build

# Build and publish to GitHub Releases
yarn dist
```

The build uses `electron-builder` with NSIS targeting Windows. Key `package.json` build config:
- `asar: false` — the app is **not** packed into an asar archive (required because R needs direct filesystem access to `r-win/` and `shiny/`).
- `extraResources: ["src/**/*"]` — ensures `src/` files are included.
- `nsis.include: "installer/installer.nsh"` — custom NSIS installer script.
- Auto-update publishes to GitHub Releases (`provider: "github"`).

**Release steps:**
1. `yarn start` — verify the app runs locally.
2. Bump `version` in `package.json`.
3. `yarn dist` — builds and publishes to GitHub Releases.
4. On GitHub, take the release out of draft / mark as latest.

---

## R Library Packaging and Update Checklist

The bundled R library in `r-win/library/` is the mechanism that lets users run the app without installing R or packages. Here is how to update it.

### How it works

- `get-r-win.sh` downloads and extracts a portable R binary (currently R 4.3.2) into `r-win/`.
- `add-cran-binary-pkgs.R` reads a package list, resolves all transitive dependencies, downloads Windows binary packages from CRAN, and installs them into `r-win/library/`. It also strips `help/`, `doc/`, `tests/`, and other non-essential directories to reduce size.
- `shiny/req.txt` is the authoritative list of packages the Shiny app loads at runtime.

### Update Checklist

Use this when you need to add, remove, or update R packages in the bundled library:

- [ ] **1. Edit `shiny/req.txt`** — add or remove the package name(s) your Shiny code needs.
- [ ] **2. Update `add-cran-binary-pkgs.R`** — if the new package isn't already in the `cran_pkgs` vector, add it there as well. This script handles dependency resolution, but explicitly listing top-level packages ensures nothing is missed.
- [ ] **3. Run `add-cran-binary-pkgs.R`** — execute from the repo root (`Rscript add-cran-binary-pkgs.R`). This downloads binary packages and installs them into `r-win/library/`. You need a working R installation on your dev machine to run this.
- [ ] **4. Verify** — run `yarn start` and confirm the app launches without package-not-found errors. Check the R console output (Debug → Toggle R Console) for any missing dependency warnings.
- [ ] **5. Commit `r-win/library/`** — the entire library directory is committed to the repo. Make sure new package folders are staged.
- [ ] **6. Test the built installer** — run `yarn build`, install the output `.exe`, and confirm the app works from the installed location (paths differ from dev).

### Upgrading the R version

- [ ] Edit `get-r-win.sh` to point to the new R version URL.
- [ ] Run the script to replace `r-win/` with the new R installation.
- [ ] Re-run `add-cran-binary-pkgs.R` to reinstall all packages against the new R version.
- [ ] Test thoroughly — binary package compatibility can change between R minor versions.

---

## Troubleshooting Guide

### Where to look first

| Symptom | Where to check |
|---------|---------------|
| App won't start / stuck on loading screen | Electron logs: `%APPDATA%/Grade Analysis Tool/logs/main.log` |
| R process crashes on startup | Toggle R Console (Debug menu) to see R's stdout/stderr |
| Missing package error in R | Check `r-win/library/` for the package folder; re-run `add-cran-binary-pkgs.R` |
| Canvas API errors | Settings tab → verify API token; check `~/Documents/Grade Analysis Tool Settings/token.rds` |
| Orphaned R processes after crash | The app writes PIDs to `%APPDATA%/Grade Analysis Tool/rPIDs.txt` and kills them on next launch. Manually check Task Manager for `R.exe` processes. |
| Auto-update not working | Check `electron-updater` logs in `main.log`. Ensure the GitHub Release is published (not draft). |
| Port conflict | The app picks a random port 3000–8000 and verifies it is available. If you have many services running, this can cause retry loops. |

### Key log locations

- **Electron logs:** `%APPDATA%/Grade Analysis Tool/logs/main.log` (via `electron-log`)
- **R console output:** Enable via Debug → Toggle R Console in the app menu bar.
- **PID file:** `%APPDATA%/Grade Analysis Tool/rPIDs.txt`
- **User settings:** `~/Documents/Grade Analysis Tool Settings/`

### Debug mode

- `Debug → Toggle R Console` — restarts R with a visible console window. Closing that console window will also close the app.
- `Debug → Open Developer Tools` (`Ctrl+Shift+I`) — opens Chromium DevTools for the Electron renderer.
- In `main.js`, set `debugRConsole = true` to always show the R console, or `isDevUpdates = true` to test the auto-updater against `dev-app-update.yml`.
- Add a `browser()` line of code in the R script and when it runs that it will go into debug mode in R.
---

# Old Dev Notes
These are old dev notes from kevin, I left them in there just in case a feature is investigated and mentioned here. They probably aren't super useful.

## Installation and Usage

1. Download the latest `.exe` installer from [GitHub Releases](https://github.com/kfilip10/grade-analysis-tool-win/releases).
2. Run the installer — it is a one-click NSIS install, no configuration needed.
3. Launch **Grade Analysis Tool** from the desktop shortcut or Start Menu.
4. On the **Settings** tab, enter your Canvas API token.
5. Use the **Canvas Gradebook** or **Gradescope Brief** tabs to pull data and generate analysis briefs.
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

