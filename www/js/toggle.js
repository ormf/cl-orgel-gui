function toggle (elem, config) {
    var colorOff        = config.colorOff || "black";
    var backgroundOff   = config.backgroundOff || "white";
    var colorOn         = config.colorOn || "black";
    var backgroundOn    = config.backgroundOn || 'rgba(120,120,120,1.0)';
    var labelOn         = config.labelOn || 'On';
    var labelOff        = config.labelOff || 'Off';

    myToggle = elem.get(0);
    console.log("toggle " + elem);
    // override setAttribte

    console.log("labelOn: " + config.labelOn);
    console.log("labelOff: " + config.labelOff);
    console.log("config: " + config);

    console.log('myToggle: ' + myToggle);

    const mySetAttribute = myToggle.setAttribute;
    
    console.log('myToggle.setAttribute: ' + myToggle.setAttribute);

    myToggle.setAttribute = function (key, value) {
        console.log("--trace, key: " + key + ', value: ' + value);
        // use call, to set the context and prevent illegal invocation errors
        mySetAttribute.call(myToggle, key, value);
        // if (value == 1.0) { console.log("1.0!"); }
        // if (value == 0.0) { console.log("0.0!"); }
        if (key == 'data-val') drawToggle(value);
    };
    
    function drawToggle (val) {
        console.log("draw: " + val);
        if (val == 1.0) {
            console.log("labelOn: " + labelOn + ", colorOn: " + colorOn + ", backgroundOn: " + backgroundOn)
            myToggle.textContent = labelOn;
            myToggle.style.color = colorOn;
            myToggle.style.background = backgroundOn;
        }
        else {
            console.log("labelOff: " + labelOff + ", colorOff: " + colorOff + ", backgroundOff: " + backgroundOff)
            myToggle.textContent = labelOff;
            myToggle.style.color = colorOff;
            myToggle.style.background = backgroundOff;
        }
    }
}
