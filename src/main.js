// Adapted from https://github.com/lawalter/r-shiny-electron-app?tab=readme-ov-file   -->
// Modified to work with ECMAScript modules (ESM) and Electron 12  KTF 5JAN24
// Based code from "Copyright (c) 2018 Dirk Schumacher, Noam Ross, Rich FitzJohn" at link above
// Generally the R server generation is from the base code, but the rest is modified by KTF



//Imports most of the packages
import { dialog, app, session, BrowserWindow, ipcMain } from 'electron'
import path from 'path' //used for __dirname since it isn't available in ESM
import { fileURLToPath } from 'url'; //
import http from 'axios' // used to make server
import os from 'os' // used to get platform
import execa from 'execa' // used to run R
import fs from 'fs' // used to read and write files
import { randomPort, waitFor, getRPath } from './helpers.js' //helper functions for R and porting

const rPath = getRPath(os.platform()) //gets the path for R based on platform
const __dirname = path.dirname(fileURLToPath(import.meta.url)); //gets the directory name for the current file

//Kill processes on relaunch (in case of bad shutdown)
import e1 from 'child_process'
const exec = e1.exec;
const pidFilePath = path.join(app.getPath('userData'), 'rPIDs.txt');

// Function to append the PID to a file
function storePid(pid) {
  fs.appendFileSync(pidFilePath, `${pid}\n`);
}

function killStoredProcesses() {
  const pidFilePath = path.join(app.getPath('userData'), 'rPIDs.txt');
  if (fs.existsSync(pidFilePath)) {
    const pids = fs.readFileSync(pidFilePath, 'utf-8').split('\n').filter(Boolean);
    pids.forEach(pid => {
      if (os.platform() === 'win32') {
        // Windows-specific command to forcefully terminate the process
        exec(`taskkill /PID ${pid} /F /T`, (err, stdout, stderr) => {
          if (err) {
            console.error(`Failed to kill process ${pid}: ${err}`);
            return;
          }
          console.log(`Process ${pid} killed successfully`);
        });
      } else {
        // macOS and Linux
        try {
          process.kill(pid, 'SIGTERM'); // Attempt to gracefully terminate the process
          console.log(`Process ${pid} has been terminated.`);
        } catch (err) {
          if (err.code === 'ESRCH') {
            console.log(`Process ${pid} does not exist.`);
          } else {
            console.error(`Error terminating process ${pid}:`, err);
          }
        }
      }
    });
    fs.writeFileSync(pidFilePath, ''); // Clear the file
  }
}


// Auto updater section
//import pkg from 'electron-updater'
//https://www.electron.build/electron-updater.class.appupdater

import { createRequire } from 'module';

const require = createRequire(import.meta.url);
const { autoUpdater } = require('electron-updater');



//set isDev to true for development
//FIX: DEV MODE
const isDev = false

if (isDev) {
  // Useful for some dev/debugging tasks, but download can
  // not be validated becuase dev app is not signed
  autoUpdater.updateConfigPath = path.join(__dirname, 'dev-app-update.yml');
}

//const { autoUpdater } = pkg; //electron-updater is a common JS module, so we need to import it this way
autoUpdater.autoDownload = true; //automatically download updates
//autoUpdater.autoInstallOnAppQuit = true; //automatically install updates on quit


//posts log files to AppData/Roaming/APPNAME/Logs
import log from 'electron-log'; //used for logging funcitonality
import { create } from 'domain';
autoUpdater.logger = log; //set the logger to the electron-log logger
autoUpdater.logger.transports.file.level = 'info';



//IPC event handling
// Listen for the 'kill-server' message from the renderer process
ipcMain.on('kill-server', () => {
  console.log('I did it');
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
// Define the file where PIDs will be stored to close R processes that stay open

// tries to start a webserver
// attempt - a counter how often it was attempted to start a webserver
// use the progress call back to listen for intermediate status reports
// use the onErrorStartup callback to react to a critical failure during startup
// use the onErrorLater callback to handle the case when the R process dies
// use onSuccess to retrieve the shinyUrl

const tryStartWebserver = async (attempt, progressCallback, onErrorStartup,
  onErrorLater, onSuccess) => {
  const { dialog } = await import('electron');

  loadingSplashScreen.webContents.once('did-finish-load', () => {
    console.log("Another Test")
    loadingSplashScreen.webContents.send('loading-event', "Test");
  })
  await progressCallback({'loading-event': "Test2"})

  if (attempt > 3) {
    await progressCallback({ attempt: attempt, code: 'failed' })
    await onErrorStartup()
    return
  }

  if (rShinyProcess !== null) {
    await onErrorStartup() // should not happen
    return
  }

  let shinyPort = randomPort()

  await progressCallback({ attempt: attempt, code: 'start' })
  // Notify the loading screen that we're starting the Shiny server

  let shinyRunning = false
  //time before it times out and says to close
  const serverStartupTimeout = 30000; // Timeout limit in milliseconds (e.g., 30000 ms for 30 seconds)

  // Setup a timeout to notify the user if the server hasn't started within the limit
  const timeoutId = setTimeout(() => {
    if (!shinyRunning) {
      console.error("R Shiny server startup timed out.");
      dialog.showMessageBox({
        type: 'error',
        title: 'Server Startup Timeout',
        message: 'The server failed to start within the expected time. Please restart the application.',
        buttons: ['Restart']
      }).then(() => {
        app.relaunch();
        try {
          rShinyProcess.kill()
        } catch (e) { }
        app.quit(); // Quit the app or you can provide an option to restart

      });
    }
  }, serverStartupTimeout);

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
  try {

    rShinyProcess = execa(rscript,
      ['--vanilla', '-f', path.join(app.getAppPath(), 'start-shiny.R')],
      {
        env: {
          'WITHIN_ELECTRON': '1', // can be used within an app to implement specific behaviour
          'RHOME': rpath,
          'R_HOME_DIR': rpath,
          'RE_SHINY_PORT': shinyPort,
          'RE_SHINY_PATH': shinyAppPath,
          'R_LIBS': libPath,
          'R_LIBS_USER': libPath,
          'R_LIBS_SITE': libPath,
          'R_LIB_PATHS': libPath
        }
      });
    storePid(rShinyProcess.pid)
    console.log(`Launched child process: PID: ${rShinyProcess.pid}`)
      

    await progressCallback({ attempt, code: 'error', message: 'Stored R Process' }); //CHANGED


  } catch (e) {
    shinyProcessAlreadyDead = true;
    console.log(`Shiny Died`)  //added this for troubleshooting
    clearTimeout(timeoutId);
    await onError(e); // Ensure onError is properly defined to handle th is
  }

  //defines gloabl url for shiny app
  let url = `http://127.0.0.1:${shinyPort}`
  for (let i = 0; i <= 29; i++) { //added from 10 to 20 attempts because 10 was too few
    if (shinyProcessAlreadyDead) {
      break
    }
    await waitFor(250) //CHANGED: Was 500, changed to 250
    try {
      const res = await http.head(url, { timeout: 1000 })
      // TODO: check that it is really shiny and not some other webserver
      if (res.status === 200) {
        await progressCallback({ attempt: attempt, code: 'success' })
        shinyRunning = true
        onSuccess(url)
        return
      }
    } catch (e) {

    }
  }


  await progressCallback({ attempt: attempt, code: 'notresponding' })

  try {
    rShinyProcess.kill()
  } catch (e) { }
}

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mainWindow
let loadingSplashScreen
let errorSplashScreen
let updateScreen

//called when ready to launch
//INPUT: url for shiny app
//Creates mainwindow
const createWindow = (shinyUrl) => {
  mainWindow = new BrowserWindow({
    show: false,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js'),
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
    preload: path.join(__dirname, 'preload.js'),
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
  return new Promise((resolve, reject) => {
    updateScreen = new BrowserWindow({
      width: 400,
      height: 300,
      frame: false,
      transparent: false,
      webPreferences: {
        preload: path.join(__dirname, 'preload.js'),
        nodeIntegration: true,
        contextIsolation: false
      }
    });

    updateScreen.loadFile(path.join(__dirname, 'loading_download.html'));


    updateScreen.webContents.once('did-finish-load', () => {
      updateScreen.show();
      console.log("did finish load");
      resolve(updateScreen);
    });

    updateScreen.on('closed', () => {
      updateScreen = null
      reject(new Error('UpdateScreen was closed before it could finish loading.'));
    });

  });
  // Ensure the window is ready before sending the message
}


const createLoadingSplashScreen = () => {
  loadingSplashScreen = createSplashScreen('loading')
}

const createErrorScreen = () => {
  errorSplashScreen = createSplashScreen('failed')
}
  // pass the loading events down to the loadingSplashScreen window
const progressCallback = async (event) => {
    loadingSplashScreen.webContents.once('did-finish-load', () => {

  try {
    if (loadingSplashScreen) {
      loadingSplashScreen.webContents.send('loading-event', event.message);
    }
  } catch (e) {
    console.error('Error sending loading event:', e);
  }
})
};
// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', async () => {  
  killStoredProcesses();

  // Set a content security policy
  //Commented out for API call
  session.defaultSession.webRequest.onHeadersReceived((_, callback) => {
    callback({
      responseHeaders: `
  "default-src 'self'; 
  script-src 'self';
  img-src 'self' data:;
  style-src 'self';
  font-src 'self';
  `})//  connect-src 'self' https://westpoint.instructure.com/; REMOVED THIS FOR NOW
  })

  // Deny all permission requests
  session.defaultSession.setPermissionRequestHandler((_1, _2, callback) => {
    callback(false)
  })

  try {
    await createUpdateScreen();
    // At this point, the updateScreen has finished loading
    console.log("wait is over");
    autoUpdater.checkForUpdatesAndNotify();
  
  } catch (error) {
    console.error('Failed to initialize update screen:', error);
  }

  //This makes a promise in this asynch function that waits for the autoUpdater to finish
  //designed to make sure that the autoUpdater is done before loading the rest of the program
  const waitForAutoUpdate = new Promise((resolve, reject) => {
    let updateTimeout;

    //runs autoUpdater
    //autoUpdater event handling
    autoUpdater.on('checking-for-update', () => {
      updateScreen.webContents.send('update-message', 'Checking for updates...');
      // waitFor(2000);
      //can add code here if needed
    });

    autoUpdater.on('update-not-available', () => {
      console.log('No update available.');
      resolve('No update available');
    });

    autoUpdater.on('update-available', () => {
      console.log('Update available. Downloading...');
      updateScreen.webContents.send('update-message', 'Update available. Downloading...');
    });

    autoUpdater.on('download-progress', (progressObj) => {
      let log_message = "Download speed: " + progressObj.bytesPerSecond;
      log_message = log_message + '\n Downloaded ' + progressObj.percent + '%';
      log_message = log_message + '\n (' + progressObj.transferred + "/" + progressObj.total + ')';

      // Here, you would typically send this progress information to your renderer process
      updateScreen.webContents.send('update-message', log_message);
    });

    autoUpdater.on('update-downloaded', () => {
      updateScreen.webContents.send('update-message', 'Update downloaded and ready to install.');

      try {
        if (rShinyProcess) {
          rShinyProcess.kill();
          console.log('R Shiny process killed successfully.');
        }
      } catch (e) {
        console.error('Failed to kill R Shiny process:', e);
      }

      // Ensure `quitAndInstall()` handles quitting and installing
      autoUpdater.quitAndInstall();
    });

    autoUpdater.on('error', (error) => {
      clearTimeout(updateTimeout); // Clear the timeou
      console.error('Error during the update process:', error);
      reject(error);
    });

    // Setup a timeout to reject the promise if the update takes too long
    updateTimeout = setTimeout(() => {
      autoUpdater.removeAllListeners(); // Remove listeners to prevent memory leaks
      reject(new Error('Update process timed out.'));
    }, 45000); // Timeout after 45 seconds
  });




  if (process.defaultApp) {
    // The app is running from the terminal (not packed)

    console.log("Launched from terminal, testing updater");


    updateScreen.close();

    //autoUpdater.emit('checking-for-update')

  } else {
    //If the app is packed then check for updates and implement promise logic on check for updates. 
    try {
      const updateStatus = await waitForAutoUpdate;
      updateScreen.close();

      console.log(updateStatus); // "No update available" or "Update downloaded"
    } catch (error) {
      console.error('Update process encountered an error:', error);
    }
  }

  createLoadingSplashScreen()
  console.log("create loading splash")
    // Wait for the splash screen to finish loading
  loadingSplashScreen.webContents.once('did-finish-load', () => {
    console.log('Loading splash screen is ready.');

    // Send a simple message to the splash screen
    loadingSplashScreen.webContents.send('loading-event', 'Initializing the application...');
  });


  //waits for a loading screen
  const emitSplashEvent = async (event, data) => {
    try {
      await loadingSplashScreen.webContents.send(event, data)
    } catch (e) { }
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
    await waitFor(500) // TODO: hack, only emit if the loading screen is ready
    await emitSplashEvent('failed')
  }

  //attempt to start shiny server
  //passes the attempt number, progress callback, error startup callback, error later callback, and on success callback
  try {
    await tryStartWebserver(0, progressCallback, onErrorStartup, onErrorLater, (url) => {
      //if successful, create window, destroy screen, and show main window
      console.log("create event");
      //pass a 'successs' message to the loading screen

      createWindow(url)
      loadingSplashScreen.destroy()
      loadingSplashScreen = null
      // Maximize the window before showing it
      mainWindow.maximize();
      mainWindow.show()
      //console.log("This is a different log message from the main process");

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

  if (process.platform !== 'darwin') { app.quit(); }

  // kill the process, just in case
  // usually happens automatically if the main process is killed
  try {
    rShinyProcess.kill()
  } catch (e) { }
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


