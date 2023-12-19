const path = require('path');
const spawn = require('child_process').spawn;
const appFolder = path.resolve(process.execPath, '..');
const updateExe = path.resolve(appFolder, 'Update.exe');
const exeName = path.basename(process.execPath);

const handleSquirrelEvent = () => {
    if (process.argv.length === 1) {
        return false;
    }

    const squirrelEvent = process.argv[1];
    switch (squirrelEvent) {
        case '--squirrel-install':
        case '--squirrel-updated':
            // Create or update shortcuts in Start Menu and Desktop
            spawn(updateExe, ['--createShortcut', exeName, '--shortcut-locations', 'Desktop,StartMenu'], { detached: true });
            return true;
        case '--squirrel-uninstall':
            // Remove shortcuts from Start Menu and Desktop
            spawn(updateExe, ['--removeShortcut', exeName], { detached: true });
            return true;
        case '--squirrel-obsolete':
            // This is called on the outgoing version of your app before
            // the new version is updated - it's the opposite of '--squirrel-install'
            return true;
    }

    return false;
};

module.exports = handleSquirrelEvent;
