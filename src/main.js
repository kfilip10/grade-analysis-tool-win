// Adapted from  Adapted from https://github.com/lawalter/r-shiny-electron-app?tab=readme-ov-file   -->
// Modified to work with ECMAScript modules (ESM) and Electron 12 
// Base code: Copyright (c) 2018 Dirk Schumacher, Noam Ross, Rich FitzJohn

import { app, session, BrowserWindow } from 'electron'

import path from 'path'
//import * from 'update-electron-app'



import http from 'axios'
import os from 'os'
import execa from 'execa'

import { randomPort, waitFor, getRPath } from './helpers.js'

const rPath = getRPath(os.platform())
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
let autoUpdater = null;

//added to import using esm

import('electron-updater').then(module => {
  // load module
  autoUpdater = module.default.autoUpdater // AppUpdater exist, but leads to TypeScript error

  
  autoUpdater.autoDownload = true;
  autoUpdater.autoInstallOnAppQuit = true;
  autoUpdater.checkForUpdatesAndNotify();

  autoUpdater.on('update-available', () => {
   // Show loading screen
      if (!updateScreen) {
        createUpdateScreen()
    }
    // Prepare to apply update
    setTimeout(() => {
      autoUpdater.quitAndInstall();
    }, 6000);
    });
    autoUpdater.on('update-available', (event, releaseNotes, releaseName) => {
    // Notify user or handle the event
    if (!updateScreen) {
      createUpdateScreen()
    }
    // Prepare to apply update
    setTimeout(() => {
      autoUpdater.quitAndInstall();
    }, 6000);
  });
  
  autoUpdater.on('update-not-available', () => {
    if (updateScreen) {
        updateScreen.close();
    }
  });
  
  autoUpdater.on('error', (err) => {
    if (updateScreen) {
        updateScreen.webContents.send(err);
        updateScreen.close();
    }
  });

}).catch(error => {
  console.error('Failed to load electron-updater:', error);
});



//const handleSquirrelEvent = require('./squirrelEvents');

// signal if a shutdown of the app was requested
// this is used to prevent an error window once the R session dies
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


// Handle creating/removing shortcuts on Windows when installing/uninstalling.
// KTF - removed for now on 2JAN23
//if (require('electron-squirrel-startup')) { // eslint-disable-line global-require
//  app.quit()
//}


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

//called when ready to launch
//INPUT: url for shiny app
//Creates mainwindow
const createWindow = (shinyUrl) => {
mainWindow = new BrowserWindow({
width: 800,
height: 600,
show: false,
webPreferences: {
preload: path.join(__dirname,'preload.js'),
nodeIntegration: false,
contextIsolation: true
}
})
console.log(path.join(__dirname,'preload.js'))
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

//I added this
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

  //I added this


  //callas loading splash screen, which calls createsplashscreen with 'loading' as an argument (loading.html is called)

  //I added this


  createLoadingSplashScreen()

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
  })
  } catch (e) {
  await emitSplashEvent('failed')
  }


  //Updates
  /*

  */


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
  // Deactivated for now
})
// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and import them here.

//update handling

