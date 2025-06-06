HTMLWidgets.widget({

  name: "basictabler",

  type: "output",

  factory: function(el, width, height) {

    var tableElement = el;
    var initialised = false;

    return {
      renderValue: function(widgetData) {

        if(!initialised) {
          if(widgetData.tableCss !== null) {
            docHead = document.head || document.getElementsByTagName("head")[0];
            var styles = document.createElement("style");
            styles.innerHTML = widgetData.tableCss;
            docHead.appendChild(styles);
          }
          initialised = true;
        }
        tableElement.innerHTML = widgetData.tableHtml;
      },

      resize: function(width, height) {
      },

    };
  }
});
