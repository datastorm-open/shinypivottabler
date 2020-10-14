// bspopover
var extract_shinyBS = {inputBindings: {}};

// The following function refer to tooltips but are used in the creation of
// tooltips and popovers because there structure is so similar. type="popover"
// will create a popover.
extract_shinyBS.addTooltip = function(id, type, opts) {
  var $id = extract_shinyBS.getTooltipTarget(id);
  var dopts = {html: true};
  opts = $.extend(opts, dopts);

  if(type == "tooltip") {
    $id.tooltip("destroy");
    $id.tooltip(opts);
  } else if(type == "popover") {
    $id.popover("destroy");
    $id.popover(opts);
  }
}

// Makes adjustments to the tooltip and popover targets for specialized
// shiny inputs/outputs
extract_shinyBS.getTooltipTarget = function(id) {

  var $id = $("#" + id).closest(".shiny-input-container, .shiny-bound-output, .btn, .shiny-download-link");

  return $id;
}


// shinypivottable
// add margin when combine is selected
Shiny.addCustomMessageHandler('method_combine_padding', function(data) {
  if (data.type == 'combine') {
    $('#' + data.inputId).addClass('combine_padding');
  }
  if (data.type == 'regular') {
    $('#' + data.inputId).removeClass('combine_padding');
  }
});
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
