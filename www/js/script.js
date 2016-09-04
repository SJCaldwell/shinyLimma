Shiny.addCustomMessageHandler("console", 
	function(message){
		console.log(message.code);
	}
);

Shiny.addCustomMessageHandler("jsCode", 
	function(message){
		eval(message.code);
	}
);