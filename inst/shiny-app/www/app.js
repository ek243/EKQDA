// Text selection and coding functionality
$(document).ready(function() {

  // Text selection handler (simplified - would need more complex implementation)
  $('#codable-text').on('mouseup', function() {
    var selection = window.getSelection();
    var selectedText = selection.toString().trim();

    if (selectedText.length > 0) {
      // In a real implementation, you'd capture the exact position
      // and send it to Shiny for coding
      Shiny.setInputValue('selected_text', selectedText);
      Shiny.setInputValue('text_selected', Math.random()); // Trigger
    }
  });

});
