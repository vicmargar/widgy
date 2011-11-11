var Temperature = Backbone.Model.extend({
    url : '/widgets/temperature',
    default_location: "Oviedo"
});

var TemperatureView = Backbone.View.extend({
    tagName : "div",
    className : "temperature-content",

    render : function() {
        var default_location = this.model.default_location;
        $(this.el).html(default_location + " : " + this.model.get(default_location) + "&deg;C");
        $('#temperature > .front > .content').html(this.el);

        var locations = temperature.get('config').locations;
        var configOptions = "";
        var model = this.model;

        $.each(locations, function(locationidx, location){
            if (model.default_location == location){
                var selected = "selected='selected'";
            }else{
                var selected = "";
            }
            configOptions += "<option " + selected + ">" + location + "</option>";
        });

        var configHTML = "<select name='config'>" + configOptions + "</select>";
        $('#temperature > .back > .content').html(configHTML);

        $('#temperature .back').width($('#temperature .front').width());
        $('#temperature .back').height($('#temperature .front').height());

        return this;
    },

    initialize: function() {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.model.fetch();

        var model = this.model;
        var view = this;

        $('#temperature > .front > .configure').click(function(){
        $('#temperature').addClass('flip');
      });

      $('#temperature > .back > .unconfigure').click(function(){
            var selected_option = $("#temperature > .back > .content > select > option:selected").val();
            model.default_location = selected_option;
            view.render();
        $('#temperature').removeClass('flip');
      });
    }
});
