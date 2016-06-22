# Freehand drawing - built with `elm`

### [Live Demo Link](http://rajasharan.github.io/elm-freehand-drawing)

### Dev setup
```sh
$ npm install -g elm
$ git clone https://github.com/rajasharan/elm-freehand-drawing
$ cd elm-freehand-drawing

$ elm reactor
Listening on http://localhost:8000/
```

### Compilation
```sh
$ elm make Main.elm --output elm.js

# Run local webserver using lite-server or python or any framework of choice
# Navigate to index.html where the local server is deployed
```

### Broadcast to Multiple Users via WebSockets
```sh
$ cd server
$ npm install
$ node server.js
WebSocketServer started on port 3000
```

### Connect to WebSocket Server
Click on the `server` icon and enter `<server-ip>:<port>` to connect to any running WebSocket server

### [License](/LICENSE)
The MIT License (MIT)
