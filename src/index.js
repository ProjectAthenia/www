// pull in desired CSS/SASS files
require( './scss/main.scss' );

// inject bundled Elm app into div#main
var Elm = require( './elm/Main' );

var storageKey = "store";
var storage = localStorage.getItem(storageKey);

// full screen the app and inject the env flags
var app = Elm.Elm.Main.init({flags: {
    storage: storage,
    config: {
        API_URL: process.env.API_URL,
        APP_NAME: process.env.APP_NAME,
        SOCKET_URL: process.env.SOCKET_URL,
        FOOTER_MESSAGE: process.env.FOOTER_MESSAGE,
    }
}});

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

let connectedSockets = [];

app.ports.connectArticleSocket.subscribe(function(data) {
    let articleId = data[1];

    var ws = new WebSocket('ws://dev-socket.projectathenia.com/articles/' + articleId + '/iterations?token=' + data[0]);
    ws.onmessage = function(message) {
        app.ports.articleUpdated.send(message.data);
    };

    ws.onclose = console.error;
    ws.onerror = console.error;

    connectedSockets[articleId] = ws;
});


app.ports.sendUpdateMessage.subscribe(function(data) {

    let articleId = data[1];

    var ws = connectedSockets[articleId];

    if (ws) {
        ws.send(data[0]);
    }
});