const { contextBridge, ipcRenderer } = require('electron');

// Expose the IPC methods securely to the renderer
contextBridge.exposeInMainWorld('electron', {
  sendKillServer: () => ipcRenderer.send('kill-server'),
});