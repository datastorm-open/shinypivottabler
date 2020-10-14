// Disable / enable a button
Shiny.addCustomMessageHandler('method_combine_padding', function(data) {
  if (data.type == 'combine') {
    $('#' + data.inputId).addClass('combine_padding');
  }
  if (data.type == 'regular') {
    $('#' + data.inputId).removeClass('combine_padding');
  }
});
