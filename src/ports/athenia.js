// Athenia root ports
export default function(app, storageKey) {
    console.log(app, storageKey);

    if (app.ports.fileSelected) {

        app.ports.fileSelected.subscribe(initFileSelect);

        function initFileSelect(id) {
            var node = document.getElementById(id);
            if (node === null) {
                // When the user navigates to this page there are some issues with the CMD coming before the dom update
                setTimeout(function () {
                    initFileSelect(id)
                }, 1);
            }

            if (node.files.length === 0) {
                return;
            }
            var file = node.files[0];
            var reader = new FileReader();


            // FileReader API is event based. Once a file is selected
            // it fires events. We hook into the `onload` event for our reader.
            reader.onload = (function(event) {
                // The event carries the `target`. The `target` is the file
                // that was selected. The result is base64 encoded contents of the file.
                var base64encoded = event.target.result;
                // We build up the `ImagePortData` object here that will be passed to our Elm
                // runtime through the `fileContentRead` subscription.
                var portData = {
                    contents: base64encoded,
                    filename: file.name,
                    fileUploaderId : id
                };

                // We call the `fileContentRead` port with the file data
                // which will be sent to our Elm runtime via Subscriptions.
                app.ports.fileContentRead.send(portData);
            });

            // Connect our FileReader with the file that was selected in our `input` node.
            reader.readAsDataURL(file);
        }
    }

    app.ports.storeCache.subscribe(function(val) {
        console.log('storageKey', storageKey);
        console.log('storeCache', val);
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

    if (process.env.STRIPE_PUBLISHABLE_KEY) {

        let formElements = {};
        let stripe = Stripe(process.env.STRIPE_PUBLISHABLE_KEY);


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
    }
}