jQuery( document ).ready(function() {
  jQuery('#minted_seats').on('change', function(){
    myseatget = jQuery(this).val()
    jQuery('#seat_size').html(myseatget);
    myshare = (parseFloat(myseatget) / parseFloat('6.906')).toFixed(3).toLocaleString();
    jQuery('#seat_ownership').html('~' + (parseFloat(myshare) * 100).toFixed(2).toLocaleString() + '%');
  });
});
