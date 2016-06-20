# Freehand drawing - built with `elm`

### [Live Demo Link](http://rajasharan.github.io/elm-freehand-drawing)

### Dev setup
```sh
$ npm install -g elm
$ git clone https://github.com/rajasharan/elm-freehand-drawing
$ cd elm-freehand-drawing
$ elm reactor
#-> http://localhost:8000
```

### Compilation
```sh
$ elm make Main.elm --output elm.js
```

### Broadcast to Multiple Users via WebSockets
```sh
$ cd server
$ npm install
$ node server.js
WebSocketServer started on port 3000
```

### Connect to WebSocket Server
```html
Now append the <server-ip>:<port> as a hash Location
For e.g: http://rajasharan.github.io/elm-frontend-drawing/#ws://192.168.X.XX:3000
```

```html
Or if the elm frontend server is running in localhost:2000 and websocket server in localhost:3000
then url is: http://localhost:2000/#ws://localhost:3000

Open in multiple browsers to test.
```

### [License](/LICENSE)
The MIT License (MIT)
