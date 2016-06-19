var WebSocketServer = require('ws').Server;
var wss = new WebSocketServer({ port: 3000 });
console.log('WebSocketServer started on port 3000');

wss.on('connection', function connection(sender) {
    var id = sender._ultron.id;
    var kind = "initial";
    var obj = { id: id, kind: kind, x: null, y: null }
    sender.send(JSON.stringify(obj));
    console.log("Client Connected: ", id);

    sender.on('message', function incoming(message) {
        wss.clients.forEach(function each(client) {
            if (client !== sender) {
                client.send(message);
            }
        });
    });
});
