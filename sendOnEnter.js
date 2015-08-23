jQuery(document).ready(function(){
  jQuery('#entry').keypress(function(evt){
    if (evt.keyCode == 13){
      jQuery('#send').click();
    }
  });
});

$(function() {
$('#shiny-tab-chatroom > div > div.col-sm-8 > div > div.box-body > div.form-group.shiny-input-container > div > div > div.selectize-input > div').click(function(){
 var value = $(this).text();
 var input = $('#entry');
 input.val(input.val() + value + ' ');
 input.focus();
 return false;
});
});

