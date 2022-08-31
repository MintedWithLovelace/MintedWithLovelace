jQuery( document ).ready(function() {
  jQuery('#minted_seats').on('change', function(){
    totalsget = jQuery('#total').html();
    myseatget = jQuery(this).val()
    jQuery('#seat_size').html(myseatget);
    myshare = (parseFloat(myseatget) / parseFloat(totalsget)).toFixed(3).toLocaleString();
    jQuery('#seat_ownership').html('~' + (parseFloat(myshare) * 100).toFixed(2).toLocaleString() + '%');
  });
});
