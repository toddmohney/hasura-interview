// Create WebSocket connection.
const socket = new WebSocket('ws://localhost:3000/health');

// Connection opened
socket.addEventListener('open', function (event) {
    socket.send('Hello Server!');
});

// Listen for messages
socket.addEventListener('message', function (event) {
    const eventData = JSON.parse(event.data);

    $("#api-metrics").html(
        JSON.stringify(eventData.requestMetrics, null, 2)
    );

    $("#gc-metrics").html(
        JSON.stringify(eventData.gcMetrics, null, 2)
    );
});
