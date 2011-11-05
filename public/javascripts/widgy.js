function addStatus(text){
    document.getElementById('status').innerHTML = document.getElementById('status').innerHTML + text + "<br>";
}

function ready(){
    if ("WebSocket" in window) {
        // browser supports websockets
        var ws = new WebSocket("ws://localhost:8081/service");
        ws.onopen = function() {
            // websocket is connected
            start(ws);
        };
        ws.onmessage = function (evt) {
            var receivedMsg = evt.data;
            addStatus(receivedMsg);
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
    ws.send("subscribe:temperature");
}