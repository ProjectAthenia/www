// pull in desired CSS/SASS files
require( './scss/main.scss' );

// inject bundled Elm app into div#main
var Elm = require( './elm/Athenia/Main' );


var storageKey = "store";
var flags = localStorage.getItem(storageKey);

// full screen the app and inject the env flags
var app = Elm.Elm.Athenia.Main.init({flags: flags});

app.ports.storeCache.subscribe(function(val) {
    if (val === null) {
        localStorage.removeItem(storageKey);
    } else {
        localStorage.setItem(storageKey, JSON.stringify(val));
    }
    // Report that the new session was stored succesfully.
    setTimeout(function() { app.ports.onStoreChange.send(val); }, 0);
});
// Whenever localStorage changes in another tab, report it if necessary.
window.addEventListener("storage", function(event) {
    if (event.storageArea === localStorage && event.key === storageKey) {
        app.ports.onStoreChange.send(event.newValue);
    }
}, false);
app.ports.connectArticleSocket.subscribe(function(data) {
    var ws = new WebSocket('ws://dev-socket.projectathenia.com/articles/' + data[1] + '/iterations?token=' + data[0]);
    ws.onmessage = function(message) {
        console.log(message);
    };

    ws.onclose = console.error;
    ws.onerror = console.error;
});