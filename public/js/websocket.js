// Create WebSocket connection.
const socket = new WebSocket('ws://localhost:3000/health');

// Connection opened
socket.addEventListener('open', function (event) {
    socket.send('Hello Server!');
});

// Listen for messages
socket.addEventListener('message', function (event) {
    console.log('Message from server ', event.data);
    $("#stats").html(JSON.stringify(JSON.parse(event.data), null, 2));
});
