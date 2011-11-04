function addStatus(text){
    var date = new Date();
    document.getElementById('status').innerHTML = document.getElementById('status').innerHTML + date + ": " + text + "<br>";
}

function ready(){
    if ("WebSocket" in window) {
        // browser supports websockets
        var ws = new WebSocket("ws://localhost:8081/service");
        ws.onopen = function() {
            alert('connected!');
            // websocket is connected
            start(ws);
            addStatus("sent message to server: 'hello server'!");
        };
        ws.onmessage = function (evt) {
            var receivedMsg = evt.data;
            addStatus("server sent the following: '" + receivedMsg + "'");
        };
        ws.onclose = function() {
            // websocket was closed
            addStatus("websocket was closed");
        };
    } else {
        // browser does not support websockets
        addStatus("sorry, your browser does not support websockets.");
    }
}

function start(ws){
    ws.send("subscribe:time");
}