// pull in desired CSS/SASS files
require( './scss/main.scss' );

// inject bundled Elm app into div#main
var Elm = require( './elm/Main' );

var data = {
    auth_token : window.localStorage.getItem('auth_token'),
    retrieved_at : window.localStorage.getItem('retrieved_at'),
    api_url: process.env.API_URL,
    socket_url: process.env.SOCKET_URL,
};
// full screen the app and inject the env flags
var app = Athenia.Main.fullscreen(data);

app.ports.setLocalStorageValue.subscribe(setLocalStorageValue);

function setLocalStorageValue(data) {
    window.localStorage.setItem(data[0], data[1]);
}

app.ports.clearStorage.subscribe(clearStorage);

function clearStorage() {
    window.localStorage.clear()
}