var Temperature = Backbone.Model.extend({
    url : '/widgets/temperature'
});

var TemperatureView = Backbone.View.extend({
    tagName : "div",
    className : "temperature-content",

    render : function() {
        $(this.el).html(this.model.get('temperature') + "&deg;C");
        $('#temperature > .front > .content').html(this.el);
        return this;
    },

    initialize: function() {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.model.fetch();

        $('#temperature > .front > .configure').click(function(){
		    $('#temperature').addClass('flip');
	    });

	    $('#temperature > .back > .unconfigure').click(function(){
		    $('#temperature').removeClass('flip');
	    });
    }
});