// Adapted from https://github.com/lawalter/r-shiny-electron-app?tab=readme-ov-file   -->
// Modified to work with ECMAScript modules (ESM) and Electron 12  KTF 5JAN24
// Based code from "Copyright (c) 2018 Dirk Schumacher, Noam Ross, Rich FitzJohn" at link above
// Generally the R server generation is from the base code, but the rest is modified by KTF

//Imports most of the packages
import { app, session, BrowserWindow } from 'electron'
import path from 'path' //used for __dirname since it isn't available in ESM
import { fileURLToPath } from 'url'; //
import http from 'axios' // used to make server
import os from 'os' // used to get platform
import execa from 'execa' // used to run R
import fs from 'fs' // used to read and write files
import { randomPort, waitFor, getRPath } from './helpers.js' //helper functions for R and porting

const rPath = getRPath(os.platform()) //gets the path for R based on platform
const __dirname = path.dirname(fileURLToPath(import.meta.url)); //gets the directory name for the current file


//Settings to relaunch afte first install - Not totally needed
const SETTINGS_FILE = path.join(app.getPath('userData'), 'appSettings.json');
let appSettings = {};
// Read existing settings or initialize if not present
if (fs.existsSync(SETTINGS_FILE)) {
    appSettings = JSON.parse(fs.readFileSync(SETTINGS_FILE, 'utf8'));
} else {
    appSettings = { firstLaunch: true, firstUpdate: true };
    fs.writeFileSync(SETTINGS_FILE, JSON.stringify(appSettings));
}


// Auto updater section
import  pkg from 'electron-updater' 
const {autoUpdater} = pkg; //electron-updater is a common JS module, so we need to import it this way
autoUpdater.autoDownload = true; //automatically download updates
autoUpdater.autoInstallOnAppQuit = true; //automatically install updates on quit

//posts log files to AppData/Roaming/APPNAME/Logs
import log from 'electron-log'; //used for logging funcitonality
autoUpdater.logger = log; //set the logger to the electron-log logger
autoUpdater.logger.transports.file.level = 'info';

//autoUpdater event handling
autoUpdater.on('checking-for-update', () => {
//can add code here if needed
  });

autoUpdater.on('update-available', (event, releaseNotes, releaseName) => {
  // Notify user or handle the event
  createUpdateScreen()

  // Prepare to apply update
  setTimeout(() => {
    autoUpdater.quitAndInstall();
  }, 6000);
});

autoUpdater.on('update-not-available', () => {
//can add code here if needed
});

autoUpdater.on('update-downloaded', (event, releaseNotes, releaseName) => {
  const flagPath = path.join(app.getPath('userData'), 'updateFlag.txt');
  fs.writeFileSync(flagPath, 'updated');
  autoUpdater.quitAndInstall();
});

autoUpdater.on('error', (err) => {
  if (updateScreen) {
      updateScreen.webContents.send(err);
      updateScreen.close();
  }
});

// signal if a shutdown of the app was requested
// this is used to prevent an error window once the R session dies
//This section is from base code
let shutdown = false

const rpath = path.join(app.getAppPath(), rPath)
const libPath = path.join(rpath, 'library')
const rscript = path.join(rpath, 'bin', 'R')
const shinyAppPath = path.join(app.getAppPath(), 'shiny')
const backgroundColor = '#2c3e50'

// We have to launch a child process for the R shiny webserver
// Things we need to take into account:
  // The process dies during setup
// The process dies during app usuage (e.g. the OS kills the process)
// At the random port, another webserver is running
// at any given time there should be 0 or 1 shiny processes
let rShinyProcess = null

// tries to start a webserver
// attempt - a counter how often it was attempted to start a webserver
// use the progress call back to listen for intermediate status reports
// use the onErrorStartup callback to react to a critical failure during startup
// use the onErrorLater callback to handle the case when the R process dies
// use onSuccess to retrieve the shinyUrl
const tryStartWebserver = async (attempt, progressCallback, onErrorStartup,
                                 onErrorLater, onSuccess) => {
                                   if (attempt > 3) {
                                     await progressCallback({attempt: attempt, code: 'failed'})
                                     await onErrorStartup()
                                     return
                                   }

                                   if (rShinyProcess !== null) {
                                     await onErrorStartup() // should not happen
                                     return
                                   }

                                   let shinyPort = randomPort()

                                   await progressCallback({attempt: attempt, code: 'start'})

                                   let shinyRunning = false
                                   const onError = async (e) => {
                                     console.error(e)
                                     rShinyProcess = null
                                     if (shutdown) { // global state :(
                                       return
                                     }
                                     if (shinyRunning) {
                                       await onErrorLater()
                                     } else {
                                       await tryStartWebserver(attempt + 1, progressCallback, onErrorStartup, onErrorLater, onSuccess)
                                     }
                                   }

                                   let shinyProcessAlreadyDead = false
                                   rShinyProcess = execa(rscript,
                                                         ['--vanilla', '-f', path.join(app.getAppPath(), 'start-shiny.R')],
                                                         { env: {
                                                           'WITHIN_ELECTRON': '1', // can be used within an app to implement specific behaviour
                                                           'RHOME': rpath,
                                                           'R_HOME_DIR': rpath,
                                                           'RE_SHINY_PORT': shinyPort,
                                                           'RE_SHINY_PATH': shinyAppPath,
                                                           'R_LIBS': libPath,
                                                           'R_LIBS_USER': libPath,
                                                           'R_LIBS_SITE': libPath,
                                                           'R_LIB_PATHS': libPath} }).catch((e) => {
                                                             shinyProcessAlreadyDead = true
                                                             onError(e)
                                                           })
                                    
                                    //defines gloabl url for shiny app
                                   let url = `http://127.0.0.1:${shinyPort}`
                                   for (let i = 0; i <= 10; i++) {
                                     if (shinyProcessAlreadyDead) {
                                       break
                                     }
                                     await waitFor(500)
                                     try {
                                       const res = await http.head(url, {timeout: 1000})
                                       // TODO: check that it is really shiny and not some other webserver
                                       if (res.status === 200) {
                                         await progressCallback({attempt: attempt, code: 'success'})
                                         shinyRunning = true
                                         onSuccess(url)
                                         return
                                       }
                                     } catch (e) {

                                     }
                                   }
                                   await progressCallback({attempt: attempt, code: 'notresponding'})

                                   try {
                                     rShinyProcess.kill()
                                   } catch (e) {}
                                 }

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mainWindow
let loadingSplashScreen
let errorSplashScreen
let updateScreen = null;
//called when ready to launch
//INPUT: url for shiny app
//Creates mainwindow
const createWindow = (shinyUrl) => {
mainWindow = new BrowserWindow({
width: 1200,
height: 900,
show: false,
webPreferences: {
preload: path.join(__dirname,'preload.js'),
nodeIntegration: false,
contextIsolation: true
}
})
mainWindow.loadURL(shinyUrl)

// mainWindow.webContents.openDevTools()

mainWindow.on('closed', () => {
mainWindow = null
})
}

const splashScreenOptions = {
width: 800,
height: 600,
backgroundColor: backgroundColor,
webPreferences: {
preload: path.join(__dirname,'preload.js'),
nodeIntegration: false,
contextIsolation: true
}
}

const createSplashScreen = (filename) => {
let splashScreen = new BrowserWindow(splashScreenOptions)
splashScreen.loadURL(`file://${__dirname}/${filename}.html`)
splashScreen.on('closed', () => {
splashScreen = null
})
return splashScreen
}

//KTF added
const createUpdateScreen = () => {
    let updateScreen = new BrowserWindow({
        width: 400,
        height: 300,
        frame: false,
        transparent: false
    });
    updateScreen.loadFile(path.join(__dirname, 'loading_download.html'));
    updateScreen.on('closed', () => updateScreen = null);
    updateScreen.webContents.on('did-finish-load', () => {
        updateScreen.show();
    });
}

const createLoadingSplashScreen = () => {
loadingSplashScreen = createSplashScreen('loading')
}

const createErrorScreen = () => {
errorSplashScreen = createSplashScreen('failed')
}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', async () => {

// Set a content security policy
  session.defaultSession.webRequest.onHeadersReceived((_, callback) => {
  callback({
  responseHeaders: `
  default-src 'none';
  script-src 'self';
  img-src 'self' data:;
  style-src 'self';
  font-src 'self';
  `})
  })

  // Deny all permission requests
  session.defaultSession.setPermissionRequestHandler((_1, _2, callback) => {
  callback(false)
  })

  //Added this to account for relaunch after update from GPT
  const flagPath = path.join(app.getPath('userData'), 'updateFlag.txt');

  if (fs.existsSync(flagPath)) {
      fs.unlinkSync(flagPath); // Remove the flag file
      // Logic to restart the app
      app.relaunch();
      app.exit();
  }
  if (appSettings.firstLaunch) {
    appSettings.firstLaunch = false;
    fs.writeFileSync(SETTINGS_FILE, JSON.stringify(appSettings));
    
    // Logic to handle first-time launch after installation
    // For example, restart the app if needed
    app.relaunch();
    app.exit();
}

//runs autoUpdater
  autoUpdater.checkForUpdatesAndNotify();

  //This makes a promise in this asynch function that waits for the autoUpdater to finish
  //designed to make sure that the autoUpdater is done before loading the rest of the program
  const waitForAutoUpdate = new Promise((resolve, reject) => {
    autoUpdater.on('update-not-available', resolve);
    autoUpdater.on('update-available', resolve);
    autoUpdater.on('error', reject);
  });

  if (process.defaultApp) {
    // The app is running from the terminal (not packed)
    console.log("Launched from terminal, not launching updater");
    createLoadingSplashScreen();

  } else {
    //If the app is packed then check for updates and implement promise logic on check for updates. 
    try {
      // Wait for the autoUpdater to finish
        //way to make sure autoupdate is done before loading rest of program

      await waitForAutoUpdate;

      // Call your splash screen and the rest of the initialization
      createLoadingSplashScreen();


    } catch (error) {
      console.error('Error during auto update:', error);
      // Handle errors or perform fallback actions
      onErrorStartup();
    }
  }

  //The rest is basically from the Github code template
  //waits for a loading screen
  const emitSplashEvent = async (event, data) => {
  try {
  await loadingSplashScreen.webContents.send(event, data)
  } catch (e) {}
  }

  // pass the loading events down to the loadingSplashScreen window
  const progressCallback = async (event) => {
  await emitSplashEvent('start-webserver-event', event)
  }

  //If we encounter an error after startup (and main window isn't called), we call the error splash screen
  const onErrorLater = async () => {
  if (!mainWindow) { // fired when we quit the app
  return
  }
  createErrorScreen()
  await errorSplashScreen.show()
  mainWindow.destroy()
  }

  //If we encounter an error on startup, we call the error splash screen
  const onErrorStartup = async () => {
  await waitFor(1000) // TODO: hack, only emit if the loading screen is ready
  await emitSplashEvent('failed')
  }

  //attempt to start shiny server
  //passes the attempt number, progress callback, error startup callback, error later callback, and on success callback
  try {
  await tryStartWebserver(0, progressCallback, onErrorStartup, onErrorLater, (url) => {
  //if successful, create window, destroy screen, and show main window
  createWindow(url)
  loadingSplashScreen.destroy()
  loadingSplashScreen = null
  mainWindow.show()
  console.log("This is a different log message from the main process");

  })
  } catch (e) {
  await emitSplashEvent('failed')
  }

})


// Quit when all windows are closed.
app.on('window-all-closed', () => {
// On OS X it is common for applications and their menu bar
// to stay active until the user quits explicitly with Cmd + Q
// if (process.platform !== 'darwin') {
// }
// We overwrite the behaviour for now as it makes things easier
// remove all events

  shutdown = true

  if (process.platform !== 'darwin') {app.quit();}

  // kill the process, just in case
  // usually happens automatically if the main process is killed
  try {
  rShinyProcess.kill()
  } catch (e) {}
})

app.on('activate', () => {
//if (BrowserWindow.getAllWindows().length == 0) createWindow();

// On OS X it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (mainWindow === null) {
    createWindow()
  }
})
// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and import them here.


