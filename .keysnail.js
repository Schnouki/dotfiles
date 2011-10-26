// ========================== KeySnail Init File =========================== //

// You can preserve your code in this area when generating the init file using GUI.
// Put all your code except special key, set*key, hook, blacklist.
// ========================================================================= //
//{{%PRESERVE%
plugins.options["hok.hint_keys"] = "0123456789";

//}}%PRESERVE%
// ========================================================================= //

// ========================= Special key settings ========================== //

key.quitKey              = "C-g";
key.helpKey              = "<f1>";
key.escapeKey            = "C-q";
key.macroStartKey        = "<f3>";
key.macroEndKey          = "<f4>";
key.universalArgumentKey = "C-u";
key.negativeArgument1Key = "C--";
key.negativeArgument2Key = "C-M--";
key.negativeArgument3Key = "M--";
key.suspendKey           = "<f2>";

// ================================= Hooks ================================= //

hook.setHook('KeyBoardQuit', function (aEvent) {
    if (key.currentKeySequence.length) {
        return;
    }
    command.closeFindBar();
    var marked = command.marked(aEvent);
    if (util.isCaretEnabled()) {
        if (marked) {
            command.resetMark(aEvent);
        } else {
            if ("blur" in aEvent.target) {
                aEvent.target.blur();
            }
            gBrowser.focus();
            _content.focus();
        }
    } else {
        goDoCommand("cmd_selectNone");
    }
    if (KeySnail.windowType === "navigator:browser" && !marked) {
        key.generateKey(aEvent.originalTarget, KeyEvent.DOM_VK_ESCAPE, true);
    }
});


// ============================= Key bindings ============================== //

key.setGlobalKey(['C-x', 'd'], function (ev, arg) {
    document.getElementById("dtaCtxTDTA").doCommand();
}, 'DownThemAll oneClick');

key.setGlobalKey(['C-x', 'C-d'], function (ev, arg) {
    document.getElementById("dtaCtxDTA").doCommand();
}, 'DownThemAll');

key.setGlobalKey(['C-x', 'C-u'], function (ev, arg) {
    BrowserViewSourceOfDocument(content.document);
}, 'View page source');

key.setGlobalKey(['C-x', 'C-i'], function (ev, arg) {
    BrowserPageInfo();
}, 'View page info');

key.setGlobalKey(['C-x', 'a'], function (ev, arg) {
    var mi = document.getElementById("toggle_addon-bar");
    var val = mi.getAttribute("checked");
    if (val) {
        mi.setAttribute("checked", "");
    } else {
        mi.setAttribute("checked", "true");
    }
    mi.click();
}, 'Toggle addon toolbar');

key.setGlobalKey('C-s', function (ev) {
    command.iSearchForwardKs(ev);
}, 'Emacs like incremental search forward', true);

key.setViewKey('f', function (ev, arg) {
    ext.exec("hok-start-foreground-mode", arg, ev);
}, 'Start Hit a Hint foreground mode', true);

key.setViewKey('F', function (ev, arg) {
    ext.exec("hok-start-background-mode", arg, ev);
}, 'Start Hit a Hint background mode', true);

key.setViewKey(';', function (ev, arg) {
    ext.exec("hok-start-extended-mode", arg, ev);
}, 'Start Hit a Hint extended mode', true);

key.setViewKey(['C-c', 'C-f'], function (ev, arg) {
    ext.exec("hok-start-continuous-mode", arg, ev);
}, 'Start Hit a Hint continuous mode', true);

key.setViewKey('a', function (ev, arg) {
    ext.exec('tanything', arg, ev);
}, 'View all tabs ', true);
