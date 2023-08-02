function toggle (elem, config) {
    var colorOff        = config.colorOff || "black";
    var backgroundOff   = config.backgroundOff || "white";
    var colorOn         = config.colorOff || "black";
    var backgroundOn    = config.backgroundOn || 'rgba(120,120,120,1.0)';
    var labelOn         = config.labelOn || 'On';
    var labelOff        = config.labelOff || 'Off';

    myToggle = elem.get(0);
    console.log("toggle " + elem);
    // override setAttribte


    console.log('myToggle: ' + myToggle);

    const mySetAttribute = myToggle.setAttribute;
    
    console.log('myToggle.setAttribute: ' + myToggle.setAttribute);

    myToggle.setAttribute = function (key, value) {
        console.log("--trace, key: " + key + ', value: ' + value);
        // use call, to set the context and prevent illegal invocation errors
        mySetAttribute.call(myToggle, key, value);
        if (value == 1.0) { console.log("1.0!"); }
        if (value == 0.0) { console.log("0.0!"); }
        if (key == 'tg-val') drawToggle(value);
    };

    console.log('myToggle.setAttribute (after): ' + myToggle.setAttribute);

    console.log('mySetAttribute: ' + mySetAttribute);

    
    function drawToggle (val) {
        console.log("draw: " + val);
        if (val == 1.0) {
            console.log("on");
            myToggle.innerHtml = labelOn;
            myToggle.color = colorOn;
            myToggle.background = backgroundOn;
        }
        else {
            console.log("off")
            myToggle.innerHtml = labelOff;
            myToggle.color = colorOff;
            myToggle.background = backgroundOff;
        }
    }
}
