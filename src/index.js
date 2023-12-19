
const { app, BrowserWindow } = require('electron');
const path = require('path');
const { autoUpdater } = require('electron-updater');
const handleSquirrelEvent = require('./squirrelEvents');

// if (handleSquirrelEvent()) {
//   app.quit();
//   return;
// }
//Basic flags
autoUpdater.autoDownload = true;
autoUpdater.autoInstallOnAppQuit = true;
const { dialog } = require('electron')

let mainWindow;
let loadingScreen;

// Handle creating/removing shortcuts on Windows when installing/uninstalling.
//if (require('electron-squirrel-startup')) {
//  app.quit();
//}
function createLoadingScreen() {
  loadingScreen = new BrowserWindow({
      width: 400,
      height: 300,
      frame: false,
      transparent: false
  });
  loadingScreen.loadFile(path.join(__dirname, 'loading_download.html'));
  loadingScreen.on('closed', () => loadingScreen = null);
  loadingScreen.webContents.on('did-finish-load', () => {
      loadingScreen.show();
  });
}

const createWindow = () => {
  // Create the browser window.
  const mainWindow = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js'),
      contextIsolation: true,
    },
  });

  // and load the index.html of the app.
  mainWindow.loadFile(path.join(__dirname, 'index.html'));
    // Check for updates

  // Open the DevTools.
  //mainWindow.webContents.openDevTools();
};

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.whenReady().then(() => {
  //createLoadingScreen();
  createWindow();
  //console.log(`The current update URL is: ${process.env.UPDATE_URL}`);
  autoUpdater.checkForUpdatesAndNotify();

    // Simulate update process
  //   setTimeout(() => {
  //     if (!loadingScreen) {
  //         createLoadingScreen();
  //     }
  //     // Simulate the end of the update process
  //     setTimeout(() => {
  //         if (loadingScreen) {
  //             loadingScreen.close();
  //         }
  //     }, 10000); // Close the loading screen after 10 seconds
  // }, 1000); // Show the loading screen after 5 seconds





  autoUpdater.on('update-available', (event, releaseNotes, releaseName) => {
    // Show loading screen
    if (!loadingScreen) {
        createLoadingScreen();
    }
    // Prepare to apply update
    setTimeout(() => {
        autoUpdater.quitAndInstall();
    }, 5000);
});

  app.on("activate", function () {
    if (BrowserWindow.getAllWindows().length == 0) createWindow();
  });
  

  // dialog.showMessageBox(mainWindow, {
  //   type: 'info',
  //   title: 'Information',
  //   message: `Checking for updates. Current version ${app.getVersion()}`,
  // })

});


// Quit when all windows are closed, except on macOS. There, it's common
// for applications and their menu bar to stay active until the user quits
// explicitly with Cmd + Q.
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') {
    app.quit();
  }
});


// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and import them here.

//update handling

autoUpdater.on('update-available', () => {
  if (mainWindow) {
      mainWindow.webContents.send('update_available');
  }
});

autoUpdater.on('update-not-available', () => {
  if (loadingScreen) {
      loadingScreen.close();
  }
});

autoUpdater.on('error', (err) => {
  if (loadingScreen) {
      loadingScreen.close();
  }

});
/*New Update Available*/




/* autoUpdater.on("update-available", (info) => {
  dialog.showMessageBox(mainWindow, {
    type: 'info',
    title: 'Information',
    message: `Update available and will be downloaded. Current version ${app.getVersion()}`,
  })

  let pth = autoUpdater.downloadUpdate();
  //mainWindow.showMessage(pth);

}); */

// autoUpdater.on("update-not-available", (info) => {
//   dialog.showMessageBox(mainWindow, {
//     type: 'info',
//     title: 'Information',
//     message: `No update available. Current version ${app.getVersion()}`,
//   })

// });

// /*Download Completion Message*/
// autoUpdater.on("update-downloaded", (info) => {
//   dialog.showMessageBox(mainWindow, {
//     type: 'info',
//     title: 'Information',
//     message: `Update downloaded. Current version ${app.getVersion()}`,
//   })

//   //mainWindow.showMessage(`Update downloaded. Current version ${app.getVersion()}`);
// });

// autoUpdater.on("error", (info) => {
//   dialog.showMessageBox(mainWindow, {
//     type: 'info',
//     title: 'Information',
//     message: (info),
//   })
// });