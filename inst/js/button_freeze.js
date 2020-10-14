// Disable / enable a button
Shiny.addCustomMessageHandler('togglewidgetShinyPivot', function(data) {
  if (data.type == 'disable') {
    $('#' + data.inputId).attr("disabled", true);
    $('#' + data.inputId).addClass('disabled');
  }
  if (data.type == 'enable') {
    $('#' + data.inputId).attr("disabled", false);
    $('#' + data.inputId).removeClass('disabled');
  }
});
