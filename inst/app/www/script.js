// if it feels like no updates you make to this file have any effect, clear
// your browser cache! It might be using an older version of this file.

function do_selectize(table_id) {
    return $('#'+table_id).find('select').selectize();
}

var renderCounter = 0;

function rendering_done(rendered_id) {
    renderCounter++;
    Shiny.setInputValue(rendered_id, renderCounter);
}

Shiny.addCustomMessageHandler('unbind-table', function(id) {
    Shiny.unbindAll($('#'+id).find('.shiny-input-container'));
});

Shiny.addCustomMessageHandler('fileInput-value', function(message) {
  var target = $('#'+message.id).parent().parent().parent().find('input[type=text]');
  target.val(message.value);
}); 

Shiny.addCustomMessageHandler('fileInput-label', function(message) {
  $('#'+message.id+"-label").text(message.value);
});

Shiny.addCustomMessageHandler('fileInput-button-label', function(message) {
  // this different style is required in order to not delete the actual fileInput
  $('#'+message.id).parent()[0].childNodes[0].nodeValue = message.value;
});

Shiny.addCustomMessageHandler('fileInput-hide-progressbar', function(message) {
  $('#'+message.id+"_progress").css("visibility", "hidden");
});

Shiny.addCustomMessageHandler('fileInput-progressbar-label', function(message) {
  $('#'+message.id+"_progress").children().text(message.value);
});
