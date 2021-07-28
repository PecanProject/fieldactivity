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