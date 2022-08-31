jQuery( document ).ready(function() {
  function do_pie(){
    var seat_chart = [];
    var small_seats = 0.000;
    var seat_this;
    jQuery('.minted_seats').each(function(){
      seat_this = parseFloat(jQuery(this).val()).toFixed(2);
      seat_chart.push({data:[[0, parseFloat(seat_this)]], label: jQuery(this).html()});
      });
    seat_chart.push({data:[[0, parseFloat(small_seats)]], label: 'Smaller Seats'});
      jQuery.plot('#piechart', seat_chart, {
      series: {
          pie: {
              show : true,
            combine: {
              color: '#999',
              label: 'Smaller',
              threshold: 0.03
            },
            stroke : {
              color : '#008e92' // '#40ffef'
            },
            label : {
              show : true,
              threshold : 0.03
            }
         }
      },
      grid: {
          hoverable: true,
        clickable: true
      },
      legend: {
          show: false
      }
      });
  }
  (function() {
    do_pie()
  }))
  jQuery('#minted_seats').on('change', function(){
    totalsget = jQuery('#total').html();
    myseatget = jQuery(this).val()
    jQuery('#seat_size').html(myseatget);
    myshare = (parseFloat(myseatget) / parseFloat(totalsget)).toFixed(3).toLocaleString();
    jQuery('#seat_ownership').html('~' + (parseFloat(myshare) * 100).toFixed(2).toLocaleString() + '%');
  });
  jQuery('#refresh').on('click', function(){
      jQuery('#minted_seats option:contains("~ Select a Seat ~")').prop('selected', true);
      jQuery('#seat_size').html('0');
      jQuery('#seat_ownership').html('0');
      do_pie();
  });
});
