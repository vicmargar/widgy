var Time = Backbone.Model.extend({
    url : '/widgets/time'
});

var TimeView = Backbone.View.extend({
    tagName : "span",
    className : "time-content",

    render : function() {
        hour = pad2(this.model.get('hour'));
        minute = pad2(this.model.get('minute'));
        second = pad2(this.model.get('second'));
        $(this.el).html("<span id='hour'>" + hour + "</span>:<span id='minute'>" + minute + "</span>:<span id='second'>" + second + "</span>");
        $('#time').html(this.el);
        return this;
    },

    initialize: function() {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.model.fetch();
    }
});

function pad2(number) {
    return (number < 10 ? '0' : '') + number
};