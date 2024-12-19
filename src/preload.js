const { contextBridge, ipcRenderer } = require('electron');

// Expose the IPC methods securely to the renderer
contextBridge.exposeInMainWorld('electron', {
  sendKillServer: () => ipcRenderer.send('kill-server'),
  onLoadingEvent: (callback) => ipcRenderer.on('loading-event', (event, message) => callback(message)),
  onInfoEvent: (callback) => ipcRenderer.on('info-event', (event, message) => callback(message)), // Added for info-event
});