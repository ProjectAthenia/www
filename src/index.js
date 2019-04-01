// pull in desired CSS/SASS files
require( './scss/main.scss' );

var stripe = Stripe(process.env.STRIPE_PUBLISHABLE_KEY);

// inject bundled Elm app into div#main
var Elm = require( './elm/Main' );

var storageKey = "store";
var storage = localStorage.getItem(storageKey);

// full screen the app and inject the env flags
var app = Elm.Elm.Main.init({flags: {
    storage: JSON.parse(storage),
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

let formElements = {};

app.ports.initStripeForm.subscribe(function(id) {
    var elements = stripe.elements();
    formElements[id] = elements.create('card');
    initElement(id);
});

function initElement(id) {
    if (document.getElementById(id)) {
        formElements[id].mount('#' + id);
    } else {
        setTimeout(() => initElement(id), 5);
    }
}

app.ports.createPaymentToken.subscribe(function(id) {

    stripe.createToken(formElements[id]).then(result => {
        app.ports.tokenCreated.send(result.token.id);
    }).catch(error => {
        app.ports.stripeError.send(error.message());
    });
});
