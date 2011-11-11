function process_message(JSONMessage){
    message = JSON.parse(JSONMessage);
    widget = widgets[message.widget];
    widget.set(message);
}

function ready(){
    time = new Time();
    window.TimeView = new TimeView({model: time});

    temperature = new Temperature();
    window.TemperatureView = new TemperatureView({model: temperature});

    widgets = new Object();
    widgets["time"] = time;
    widgets["temperature"] = temperature;

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

    // // set up block configuration
	// $('.block .configure').click(function(){
	// 	$('.block').addClass('flip');
	// });

    // // remove block configuration
	// $('.block .unconfigure').click(function(){
	// 	$('.block').removeClass('flip');
	// });

}

function start(ws){
    ws.send("subscribe:time");
    ws.send("subscribe:temperature");
}