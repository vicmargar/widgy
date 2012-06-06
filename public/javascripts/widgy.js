function process_message(JSONMessage){
    message = JSON.parse(JSONMessage);
    message_widgets = message["widgets"];

    for (var i in message_widgets){
        var message = message_widgets[i];
        var widget = JSON.parse(message);
        var widget_type = widget["widget"];

        w = widgets[widget_type];
        w.set(widget);
    };
}

function ready(){
    counter = new Counter();
    window.CounterView = new CounterView({model: counter});

    widgets = new Object();
    widgets["counter"] = counter;

    if ("WebSocket" in window) {
        // browser supports websockets
        var wshost = window.location.host.split(':')[0];
        var ws = new WebSocket("ws://" + wshost + ":8081");
        ws.onopen = function() {
            // websocket is connected
            start(ws);
        };
        ws.onmessage = function (evt) {
            var receivedMsg = evt.data;
            console.log(receivedMsg);
            process_message(receivedMsg);
        };
        ws.onclose = function() {
            // websocket was closed
            //addStatus("websocket was closed");
        };
    } else {
        // browser does not support websockets
        //addStatus("Sorry, your browser does not support websockets.");
    };

    $(function() {
		$( ".widget" ).draggable();
	});

}

function start(ws){
    ws.send("subscribe"); // In the future it will be able to subscribe to different dashboards
}