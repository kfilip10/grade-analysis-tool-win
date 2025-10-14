// Adapted from https://github.com/lawalter/r-shiny-electron-app?tab=readme-ov-file   -->
// Modified to work with ECMAScript modules (ESM) and Electron 12  KTF 5JAN24
// Based code from "Copyright (c) 2018 Dirk Schumacher, Noam Ross, Rich FitzJohn" at link above
// Generally the R server generation is from the base code, but the rest is modified by KTF



//Imports most of the packages
import { dialog, app, session, BrowserWindow, ipcMain , Menu} from 'electron'
import path from 'path' //used for __dirname since it isn't available in ESM
import { fileURLToPath } from 'url'; //
import http from 'axios' // used to make server
import { spawn } from 'child_process'; // used to run R instead of execa

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
    console.log(`Found ${pids.length} stored PIDs to clean up`);
    
    pids.forEach(pid => {
      if (os.platform() === 'win32') {
        // Kill all child processes too with /T flag
        exec(`taskkill /PID ${pid} /F /T`, (err, stdout, stderr) => {
          if (err && !err.message.includes('not found')) {
            console.error(`Failed to kill process ${pid}: ${err}`);
          } else {
            console.log(`Process ${pid} and children killed successfully`);
          }
        });
      } else {
        try {
          process.kill(pid, 'SIGKILL'); // Use SIGKILL for forceful termination
          console.log(`Process ${pid} has been terminated.`);
        } catch (err) {
          if (err.code !== 'ESRCH') { // ESRCH means process doesn't exist
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



//set isDev to true for development testing updates
const isDevUpdates = false
let debugRConsole = false; // Set to true to show R console, false to hide


if (isDevUpdates) {
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
  app.quit()
});

// Add this with your other IPC handlers
ipcMain.on('restart-app', () => {
  console.log('Restart requested from renderer');
  
  // Kill R process first
  if (rShinyProcess && !rShinyProcess.killed) {
    try {
      rShinyProcess.kill();
    } catch (e) {
      console.error('Error killing R process during restart:', e);
    }
  }
  
  // Restart the app
  app.relaunch();
  app.quit();
});

// IPC handler for toggling R console
ipcMain.on('toggle-r-console', (event, showConsole) => {
  console.log(`R Console toggle requested: ${showConsole}`);
  
  if (rShinyProcess && !rShinyProcess.killed) {
    // If we want to show console and it's currently hidden
    if (showConsole && !debugRConsole) {
      // Kill current process and restart with console visible
      restartRProcessWithConsole(true);
    } 
    // If we want to hide console and it's currently visible
    else if (!showConsole && debugRConsole) {
      // Kill current process and restart with console hidden
      restartRProcessWithConsole(false);
    }
  }
});
// Function to restart R process with different console visibility
async function restartRProcessWithConsole(showConsole) {
  console.log(`Restarting R process with console ${showConsole ? 'visible' : 'hidden'}`);
  
  // Update the debug flag
  debugRConsole = showConsole;
  
  // Store the current URL
  const currentUrl = mainWindow ? mainWindow.webContents.getURL() : null;
  
  try {
    // Kill the current R process
    if (rShinyProcess) {
      shutdown = true; // Set shutdown flag to prevent auto-quit during restart
      rShinyProcess.kill();
      rShinyProcess = null;
      
      // Wait a moment for cleanup
      await new Promise(resolve => setTimeout(resolve, 1000));
      
      shutdown = false; // Reset shutdown flag
    }
    
    // Restart the webserver
    await tryStartWebserver(0, progressCallback, 
      async () => console.log('Failed to restart R process'),
      async () => console.log('R process died after restart'),
      (url) => {
        console.log(`R process restarted successfully at ${url}`);
        if (mainWindow && currentUrl !== url) {
          mainWindow.loadURL(url);
        }
      }
    );
    
  } catch (error) {
    console.error('Error restarting R process:', error);
    shutdown = false; // Reset shutdown flag on error
  }
}
// Add this function after the restartRProcessWithConsole function (around line 130)
function createMenu() {
  const template = [
    {
      label: 'Debug',
      submenu: [
        {
          label: 'Toggle R Console',
          type: 'checkbox',
          checked: debugRConsole,
          click: (menuItem) => {
            if (menuItem.checked) {
              dialog.showMessageBox(mainWindow, {
                type: 'info',
                title: 'R Console Warning',
                message: 'When you enable the R Console, closing the R terminal window will also close this application.',
                buttons: ['OK', 'Cancel'],
                defaultId: 0
              }).then((result) => {
                if (result.response === 0) { // OK clicked
                  restartRProcessWithConsole(true);
                } else { // Cancel clicked
                  menuItem.checked = false; // Uncheck the menu item
                }
              });
            } else {
              restartRProcessWithConsole(false);
            }
          }
        },
        {
          label: 'Open Developer Tools',
          accelerator: 'CmdOrCtrl+Shift+I',
          click: () => {
            if (mainWindow) {
              mainWindow.webContents.openDevTools();
            }
          }
        }
      ]
    }
  ];
  
  const menu = Menu.buildFromTemplate(template);
  Menu.setApplicationMenu(menu);
}
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
  await progressCallback({ attempt: attempt, code: 'start' })


  if (attempt > 3) {
    await progressCallback({ attempt: attempt, code: 'failed' })
    await onErrorStartup()
    return
  }

  if (rShinyProcess !== null) {
    await onErrorStartup() // should not happen
    return
  }

  //let shinyPort = randomPort()
  const net = require('net');
  function isPortAvailable(port) {
    return new Promise((resolve) => {
      const server = net.createServer();
      server.once('error', () => resolve(false));
      server.once('listening', () => {
        server.close(() => resolve(true));
      });
      server.listen(port);
    });
  }

  const shinyPort = await (async () => {
    let port;
    do {
      port = randomPort();
    } while (!(await isPortAvailable(port)));
    return port;
  })();

  await progressCallback({ attempt: attempt, code: 'start' })
  // Notify the loading screen that we're starting the Shiny server

  let shinyRunning = false
  //time before it times out and says to close
  const serverStartupTimeout = 90000; // Timeout limit in milliseconds (e.g., 30000 ms for 30 seconds)

// Setup a timeout to notify the user if the server hasn't started within the limit
const timeoutId = setTimeout(() => {
  if (!shinyRunning) {
    console.error("R Shiny server startup timed out.");
    dialog.showMessageBox({
      type: 'error',
      title: 'Server Startup Timeout',
      message: 'The server failed to start within the expected time. Please restart the application.',
      buttons: ['Restart', 'Close']
    }).then(({ response }) => {
      // `response` will be the index of the button clicked
      if (response === 0) { // 'Restart' button
        app.relaunch();
        try {
          rShinyProcess.kill();
        } catch (e) {
          console.error("Failed to kill R Shiny process:", e);
        }
        app.quit();
      }
      // If the 'Close' button or 'X' is clicked, do nothing
      app.quit();

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
    console.log('Environment Variables:', {
      'WITHIN_ELECTRON': '1',
      'RHOME': rpath,
      'R_HOME_DIR': rpath,
      'RE_SHINY_PORT': shinyPort,
      'RE_SHINY_PATH': shinyAppPath,
      'R_LIBS': libPath,
      'R_LIBS_USER': libPath,
      'R_LIBS_SITE': libPath,
      'R_LIB_PATHS': libPath
    });

    // Alternative approach for Windows - creates new console window only in debug mode
      if (os.platform() === 'win32') {
        if (debugRConsole) {
          // Show R console for debugging
          rShinyProcess = spawn('cmd', ['/c', 'start', '/wait', rscript, '--vanilla', '-f', path.join(app.getAppPath(), 'start-shiny.R')], {
            env: {
              ...process.env,
              'WITHIN_ELECTRON': '1',
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
        } else {
          // Hide R console for production
          rShinyProcess = spawn(rscript, ['--vanilla', '-f', path.join(app.getAppPath(), 'start-shiny.R')], {
            stdio: ['pipe', 'pipe', 'pipe'],
            windowsHide: true,
            detached: false,
            env: {
              ...process.env,
              'WITHIN_ELECTRON': '1',
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
        }
      } else {
        // Keep the existing spawn for non-Windows platforms
        rShinyProcess = spawn(rscript, 
          ['--vanilla', '-f', path.join(app.getAppPath(), 'start-shiny.R')], 
          {
            stdio: debugRConsole ? 'inherit' : ['pipe', 'pipe', 'pipe'],
            windowsHide: !debugRConsole,
            detached: false,
            env: {
              ...process.env,
              'WITHIN_ELECTRON': '1',
              'RHOME': rpath,
              'R_HOME_DIR': rpath,
              'RE_SHINY_PORT': shinyPort,
              'RE_SHINY_PATH': shinyAppPath,
              'R_LIBS': libPath,
              'R_LIBS_USER': libPath,
              'R_LIBS_SITE': libPath,
              'R_LIB_PATHS': libPath
            }
          }
        );
      }

      // Only add stdout/stderr listeners when not in debug mode (when console is hidden)
      if (!debugRConsole) {
        // Forward R console output to main process console
        rShinyProcess.stdout.on('data', (data) => {
          console.log(`R stdout: ${data}`);
        });

        rShinyProcess.stderr.on('data', (data) => {
          console.error(`R stderr: ${data}`);
        });
      }

      // Handle process exit
      rShinyProcess.on('exit', (code, signal) => {
        console.log(`R process exited with code ${code} and signal ${signal}`);
        rShinyProcess = null;
        
        // If the R console was visible and user closed it manually, close the app
        if (debugRConsole && !shutdown) {
          // Check if it was an error exit (non-zero exit code)
          if (code !== 0 && code !== null) {
            console.log(`R process crashed with exit code ${code}, pausing before shutdown...`);
            
            // Show dialog to give user time to capture console output
            dialog.showMessageBox(mainWindow, {
              type: 'warning',
              title: 'R Process Error',
              message: `The R process has crashed with exit code ${code}.\n\nPlease capture any console output now before clicking OK.`,
              detail: `Signal: ${signal}\nYou can copy text from the R console window before proceeding.`,
              buttons: ['Close Application', 'Keep App Running'],
              defaultId: 0,
              cancelId: 1
            }).then((result) => {
              if (result.response === 0) { // Close Application
                shutdown = true;
                if (mainWindow) {
                  mainWindow.close();
                }
                app.quit();
              } else { // Keep App Running
                console.log('User chose to keep app running after R crash');
                // Optionally restart R process or show error page
                createErrorScreen();
                if (errorSplashScreen) {
                  errorSplashScreen.show();
                }
                if (mainWindow) {
                  mainWindow.hide(); // Hide main window since R is not running
                }
              }
            });
          } else {
            // Normal exit (code 0 or null), close normally
            console.log('R console was closed normally, shutting down Electron app...');
            shutdown = true;
            
            // Close all windows and quit the app
            if (mainWindow) {
              mainWindow.close();
            }
            app.quit();
          }
        }
      });

      // error handler for runtime R errors:
      rShinyProcess.on('error', (error) => {
        console.error('R process error:', error);
        
        if (debugRConsole && !shutdown) {
          // Show dialog for runtime errors too
          dialog.showMessageBox(mainWindow, {
            type: 'error',
            title: 'R Process Runtime Error',
            message: 'The R process encountered a runtime error.',
            detail: `Error: ${error.message}\n\nPlease check the R console for details and capture any output before proceeding.`,
            buttons: ['Close Application', 'Keep App Running'],
            defaultId: 0,
            cancelId: 1
          }).then((result) => {
            if (result.response === 0) { // Close Application
              shutdown = true;
              if (mainWindow) {
                mainWindow.close();
              }
              app.quit();
            } else { // Keep App Running
              console.log('User chose to keep app running after R error');
              // The R process might still be running, so don't force quit
            }
          });
        }
      });

    storePid(rShinyProcess.pid);
    console.log(`Launched child process: PID: ${rShinyProcess.pid}`);
    await progressCallback({ attempt: attempt, type: 'info', message: `Launched R Shiny Process PID: ${rShinyProcess.pid}` });

  } catch (e) {
    shinyProcessAlreadyDead = true;
    console.log(`Shiny Died`)  //added this for troubleshooting
    await progressCallback({ attempt, code: 'info', message: 'Shiny Died' }); //CHANGED
    clearTimeout(timeoutId);
    await onError(e); // Ensure onError is properly defined to handle th is
  }

  //defines gloabl url for shiny app
  //defines global url for shiny app
  let url = `http://127.0.0.1:${shinyPort}`
  console.log('port ', shinyPort)

  // Wait a bit longer initially since R package loading takes time
  let initialWait = 5000; // Wait 5 seconds before first attempt
  await new Promise(resolve => setTimeout(resolve, initialWait));

  for (let i = 0; i <= 15; i++) {
    await progressCallback({ attempt: attempt, code: 'trying', url: url, i: i })
    try {
      // Replace the testUrl(url) call with:
      let isRunning = rShinyProcess && !rShinyProcess.killed && await (async () => {
        try {
          const res = await http.head(url, { timeout: 1000 })
          return res.status === 200
        } catch (e) {
          return false
        }
      })()
      if (isRunning) {
        clearTimeout(timeoutId); // Clear the timeout since server started successfully
        console.log(`Shiny server is running on ${url}`)
        shinyRunning = true
        await progressCallback({ attempt: attempt, code: 'success', url: url, i: i })
        await onSuccess(url)
        return
      }
    } catch (e) {
      // Connection failed, continue trying
      console.log(`Connection attempt ${i} failed, retrying...`)
    }
    // Increase wait time more gradually
    let waitTime = Math.min(1000 + (i * 200), 3000); // Max 3 second wait between attempts
    await new Promise(resolve => setTimeout(resolve, waitTime))
  }


  await progressCallback({ attempt: attempt, type: 'info',message: 'Shiny Not Responding' })

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
  createMenu(); // Add this line

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


// Update the progressCallback function around line 620:
const progressCallback = async (event) => {
  try {
    if (loadingSplashScreen) {
      if (loadingSplashScreen.webContents.isLoading()) {
        loadingSplashScreen.webContents.once('did-finish-load', () => {
          try {
            // Send the right message based on event properties
            if (event.message) {
              // If there's a direct message, send it as info
              loadingSplashScreen.webContents.send('info-event', event.message);
            } else if (event.code === 'start') {
              loadingSplashScreen.webContents.send('loading-event', `Starting R server (attempt ${event.attempt})...`);
            } else if (event.code === 'trying') {
              loadingSplashScreen.webContents.send('loading-event', `Testing connection ${event.i}/15...`);
            } else if (event.code === 'success') {
              loadingSplashScreen.webContents.send('loading-event', 'Connection successful! Loading application...');
            } else if (event.code === 'failed') {
              loadingSplashScreen.webContents.send('loading-event', 'Failed to start server after multiple attempts.');
            } else {
              // Fallback for other events
              loadingSplashScreen.webContents.send('info-event', JSON.stringify(event));
            }
          } catch (e) {
            console.error('Error sending event after load:', e);
          }
        });
      } else {
        // Same logic for when not loading
        if (event.message) {
          loadingSplashScreen.webContents.send('info-event', event.message);
        } else if (event.code === 'start') {
          loadingSplashScreen.webContents.send('loading-event', `Starting R server (attempt ${event.attempt})...`);
        } else if (event.code === 'trying') {
          loadingSplashScreen.webContents.send('loading-event', `Testing connection ${event.i}/15...`);
        } else if (event.code === 'success') {
          loadingSplashScreen.webContents.send('loading-event', 'Connection successful! Loading application...');
        } else if (event.code === 'failed') {
          loadingSplashScreen.webContents.send('loading-event', 'Failed to start server after multiple attempts.');
        } else {
          loadingSplashScreen.webContents.send('info-event', JSON.stringify(event));
        }
      }
    }
  } catch (e) {
    console.error('Error sending event:', e);
  }
};


// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', async () => {  
  killStoredProcesses();   //removed because I'm not sure it is helping

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
    });

    autoUpdater.on('update-not-available', () => {
      console.log('No update available.');
      clearTimeout(updateTimeout); // Clear timeout
      resolve('No update available');
    });

    autoUpdater.on('update-available', () => {
      console.log('Update available. Downloading...');
      updateScreen.webContents.send('update-message', 'Update available. Downloading...');
    });

    autoUpdater.on('download-progress', (progressObj) => {
      let percentRounded = Math.round(progressObj.percent);
      let log_message = "Download speed: " + progressObj.bytesPerSecond;
      log_message = log_message + '\n Downloaded ' + percentRounded + '%';
      log_message = log_message + '\n (' + Math.round(progressObj.transferred) + "/" + Math.round(progressObj.total)+ ')';

      // Here, you would typically send this progress information to your renderer process
      updateScreen.webContents.send('update-message', log_message);
    });

    autoUpdater.on('update-downloaded', () => {
      updateScreen.webContents.send('update-message', 'Update downloaded and ready to install.');
      clearTimeout(updateTimeout); // Clear timeout
      try {
          if (rShinyProcess) {
            rShinyProcess.kill(); // Kill the R process
            console.log('R Shiny process killed successfully.');
          }
      } catch (e) {
        console.error('Failed to kill R Shiny process:', e);
      }

      // Ensure Electron quits cleanly
      app.removeAllListeners('window-all-closed'); // Prevent default app.quit behavior
      app.quit(); // Quit the app completely

        // Proceed with the update installation
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


