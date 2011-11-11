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
        $('#time > .front > .content').html(this.el);

        $('#time .back').width($('#time .front').width());
        $('#time .back').height($('#time .front').height());
        return this;
    },

    initialize: function() {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.model.fetch();

      $('#time > .front > .configure').click(function(){
        $('#time').addClass('flip');
      });

      $('#time > .back > .unconfigure').click(function(){
        $('#time').removeClass('flip');
      });
    }
});

function pad2(number) {
    return (number < 10 ? '0' : '') + number
};
