var Time = Backbone.Model.extend({
    url : '/widgets/time'
});

var TimeView = Backbone.View.extend({
    tagName : "div",
    className : "time-content",

    render : function() {
        hour = this.model.get('hour');
        minute = this.model.get('minute');
        second = this.model.get('second');
        $(this.el).html(hour + ":" + minute + ":" + second);
        $('#time').html(this.el);
        return this;
    },

    initialize: function() {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.model.fetch();
    }
});