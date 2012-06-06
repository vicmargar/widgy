var Counter = Backbone.Model.extend({
    url : '/widgets/counter'
});

var CounterView = Backbone.View.extend({
    tagName : "div",
    className : "counter-content",

    render : function() {
        var count = this.model.get("count");
        console.log("rendering..." + count);
        $(this.el).html(count);
        $('#counter > .front > .content').html(this.el);
        return this;
    },

    initialize: function() {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.model.fetch();

        var model = this.model;
        var view = this;
    }
});