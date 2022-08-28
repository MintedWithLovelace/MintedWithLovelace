jQuery( document ).ready(function() {
  var hdid = 'project_id';
  var totalSeats = '0.000';
  var policyID = '0db17bcab4abb013b666faf42d0d148ea25bafaa894b9f3b8163f6e1';
  var net = 'mainnet';
  var p_a = '089cV29';
  var p_b = 'dJudlrdDcFq';
  var p_c = 'mBrqfqvqMF';
  var api_uri = 'https://cardano-' + net + '.blockfrost.io/api/v0/assets/'
  var getTotal = true;
  function get_asset_meta(assetId){
    return jQuery.ajax({
      url: api_uri + assetId,
      async: false,
      beforeSend: function(xhr) {
        xhr.setRequestHeader(hdid, net+'vI9'+p_a+p_b+p_c+'0')
      },
      success: function(data){
        thisSeatName = convertFromHex(data['asset_name'])
        thisSeatSize = parseFloat(data['onchain_metadata']['Seat Size']).toFixed(3).toLocaleString();
        if( $('#' + thisSeatName).length ){
          jQuery('#' + thisSeatName).val(thisSeatSize)
              }
        else{
          jQuery('#minted_seats').append('<option class="minted_seats" value="' + thisSeatSize + '" id="' + thisSeatName + '">' + thisSeatName + '</option>');
        }
        totalSeats = (parseFloat(totalSeats) + parseFloat(thisSeatSize)).toFixed(3).toLocaleString();
        jQuery('#total').html(totalSeats);
        var sel = jQuery('#minted_seats');
        var selected = sel.val();
        var opts_list = sel.find('option');
        opts_list.sort(function(a, b) { return jQuery(a).text() > jQuery(b).text() ? 1 : -1; });
        sel.html('').append(opts_list);
        sel.val(selected);
      }
    });
  }
  function get_assets(policyID){
      jQuery.ajax({
      url: api_uri + 'policy/' + policyID,
      async: false,
      beforeSend: function(xhr){
          xhr.setRequestHeader(hdid, net+'vI9'+p_a+p_b+p_c+'0')
        },
      success: function(data){
          var totalSeats;
          jQuery.each(data, function(index, value) {
          get_asset_meta(value['asset'], true)
              });
      }
    });
  }
  function convertFromHex(hex) {
      var hex = hex.toString();
      var str = '';
      for (var i = 0; i < hex.length; i += 2)
          str += String.fromCharCode(parseInt(hex.substr(i, 2), 16));
      return str;
  }
  function convertToHex(str) {
      var hex = '';
      for(var i=0;i<str.length;i++) {
          hex += ''+str.charCodeAt(i).toString(16);
      }
      return hex;
  }
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
  (function(next) {
    get_assets(policyID);
    next()
  }(function() {
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
