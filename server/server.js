var WebSocketServer = require('ws').Server;
var wss = new WebSocketServer({ port: 3000 });
console.log('WebSocketServer started on port 3000');

wss.on('connection', function connection(sender) {
  sender.send('connected');

  sender.on('message', function incoming(message) {
    var msg = message.split('(').join('[').split(')').join(']');
    //console.log('received: %s', msg);

    wss.clients.forEach(function each(client) {
        if (client !== sender) {
            client.send(msg);
        }
    });
  });
});
