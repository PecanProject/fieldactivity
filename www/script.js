function do_selectize(table_id) {
    return $('#'+table_id).find('select').selectize();
}

var renderCounter = 0;

function rendering_done(rendered_id) {
    renderCounter++;
    Shiny.setInputValue(rendered_id, renderCounter);
}
