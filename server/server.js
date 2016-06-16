var WebSocketServer = require('ws').Server;
var wss = new WebSocketServer({ port: 3000 });

wss.on('connection', function connection(ws) {
  ws.on('message', function incoming(message) {
    var msg = message.split('(').join('[').split(')').join(']');
    console.log('received: %s', msg);
    broadcast(ws, msg);
  });

  ws.send('something');
});

broadcast = function broadcast(sender, data) {
  wss.clients.forEach(function each(client) {
    if (client !== sender) {
        client.send(data);
    }
  });
};
