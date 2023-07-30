function vumeter(elem, config){

    // Settings
    var max             = config.max || 100;
    var boxCount        = config.boxCount || 10;
    var boxCountRed     = config.boxCountRed || 2;
    var boxCountYellow  = config.boxCountYellow || 3;
    var boxGapFraction  = config.boxGapFraction || 0.2;
//    var jitter          = config.jitter || 0.02;

    // Colours
    var redOn     = 'rgba(255,47,30,1.0)';
    var redOff    = 'rgba(64,12,8,1.0)';
    var yellowOn  = 'rgba(255,215,5,1.0)';
    var yellowOff = 'rgba(64,53,0,1.0)';
    var orangeOn  = 'rgba(215,215,5,1.0)';
    var orangeOff = 'rgba(53,53,0,1.0)';
    var greenOn   = 'rgba(53,255,30,1.0)';
    var greenOff  = 'rgba(13,64,8,1.0)';

    var PdPurple  = 'rgba(244,48,240,1.0)';
    var PdRed     = 'rgba(252,40,40,1.0)';
    var PdOrange  = 'rgba(250,171,71,1.0)';
    var PdYellow  = 'rgba(232,232,40,1.0)';
    var PdGreen   = 'rgba(20,232,20,1.0)';
    var colBlue1 = 'rgba(68, 85, 0, 1.0)';
    var colBlue2 = 'rgba(0, 102, 128, 1.0)';
    var colBlue3 = 'rgba(0, 136, 170, 1.0)';
    var colBlue4 = 'rgba(0, 170, 212, 1.0)';
    var colBlue5 = 'rgba(0, 204, 255, 1.0)';
    var colBlue6 = 'rgba(42, 212, 255, 1.0)';
    var colBlue7 = 'rgba(85, 221, 255, 1.0)';
    var colBlue8 = 'rgba(128, 229, 255, 1.0)';
    var colBlue9 = 'rgba(170, 238, 255, 1.0)';
    var colBlue10 = 'rgba(213, 246, 255, 1.0)';


    
    console.log('boxCount: ' + boxCount);
    // Derived and starting values

    var colors = [];
    setPdColors();

    var vuMeter = elem.get(0);
    console.log('vuMeter: ' + vuMeter);

    var vuLedContainer = document.createElement("div");
    vuLedContainer.style.height = "100%";
    vuLedContainer.style.width = "100%";
    vuLedContainer.style.padding = "2px";
    vuLedContainer.style.display = "flex";
    vuLedContainer.style.flexDirection = "column";
    vuLedContainer.style.justifyContent = "space-between";

    vuMeter.appendChild(vuLedContainer);
        
    var leds = [];
    for (i = 39;i>=0;i--) {
        leds[i] = document.createElement("span");
        leds[i].style.width = "100%";
        leds[i].style.height = "100%";
        leds[i].style.border = "thin solid #222";
        leds[i].style.backgroundColor = "#222";
        if (i < 39) { leds[i].style.borderTopStyle = "none"; }
        vuLedContainer.appendChild(leds[i]);
    }
    var lastVal=0;
    var valLookup = [0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 10, 10, 11, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 19, 19, 20, 21, 22, 23, 24, 25, 27, 29, 31, 33, 34, 35, 36, 37, 37, 38, 39, 39, 39, 40];

    // Main draw function
    var draw = function() {
        console.log('redraw!' + vuMeter.getAttribute("db-val"));
        var targetDB = 100+parseInt(vuMeter.getAttribute("db-val"), 10)
        if (targetDB > 112)
            targetDB=112;
        else { if (targetVal < 0) tarvetVal = 0; }
        
        var targetVal = valLookup[targetDB];
        console.log('redraw! lastVal: ' + lastVal + ', targetVal: ' + targetVal);

        if (targetVal != lastVal) {
            if (targetVal > lastVal) {
                for (var i = lastVal;i < targetVal;i++) {
                    leds[i].style.backgroundColor = colors[i];
                    console.log('i: ' + i + ', on: ' + leds[i].style.backgroundColor);
                }
            }
            else {
                for (var i = targetVal;i < lastVal;i++) {
                    leds[i].style.backgroundColor = "#222";
                    console.log('i: ' + i + ', off: ' + leds[i].style.backgroundColor);
                }
            }
        
            lastVal = targetVal;
            //        requestAnimationFrame(draw);
        }
    };

    const mySetAttribute = vuMeter.setAttribute;
    // override setAttribte
    vuMeter.setAttribute = function (key, value) {
//        console.log("--trace, key: " + key + ', value: ' + value);
        // use call, to set the context and prevent illegal invocation errors
        mySetAttribute.call(vuMeter, key, value);
//        if (key == 'db-val') drawBoxes(c, value);
        if (key == 'db-val') draw();
    };

    function setPdColors () {
        for (i = 0;i<16;i++) { colors[i] = PdGreen; }
        for (i = 16;i<26;i++) { colors[i] = PdYellow; }
        for (i = 26;i<28;i++) { colors[i] = PdOrange; }
        for (i = 28;i<39;i++) { colors[i] = PdRed; }
        colors[39] = PdPurple;
    }

    // Trigger the animation
    draw();
}
