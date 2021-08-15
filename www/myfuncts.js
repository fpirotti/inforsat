var currentMap=null;
var currentMapDOM=null;
var spyGlassActivelayers = [];

var getN = function(obj, n=0){
    return obj[Object.keys(obj)[n]]
};

var baselayerChanged = function (e) {
   currentMap = e.layer; 
   currentMapDOM = getN(currentMap._layers)._container;
   currentMap.bringToBack();
};

var spyGlassActivate = function(e) { 
   var lid = e.prev().children().first().children()[0].layerId;
   var iof = spyGlassActivelayers.indexOf(lid);
   if(iof == -1) spyGlassActivelayers.push(lid);
};

var spyGlassDeactivate = function(e) { 
   var lid = e.prev().children().first().children()[0].layerId;
   var iof = spyGlassActivelayers.indexOf(lid);
   if(iof != -1)  spyGlassActivelayers.splice(iof, 1);
};

var spyGlass = function(e) {

   var m = e.layerPoint; 
   for (var i = 0; i < spyGlassActivelayers.length; i++) {
      var obj = myMap._layers[ spyGlassActivelayers[i] ]._layers;
      var cont = obj[Object.keys(obj)[0]]._container;  
      cont.style['clip-path']=`circle(120px at ${m.x}px ${m.y}px)`;  
      //cont.style['clip-path']=`rect(0px, 0px, 0px, ${m.x}px);`;  
   }
 
};

var changeLayerOpacity = function(e) { 
  var lid = e.prev().prev().children().first().children()[0].layerId;
  var lll = myMap._layers[lid];
  
  if(typeof lll !== 'undefined') { 
      var obj = lll._layers;
      var cont = obj[Object.keys(obj)[0]]; //._container; 
      cont.setOpacity(e[0].value/100);
  }
};

$(window).on('load', function() { 
  

 
});

