function toggle (elem, config) {
    var colorOff        = config.colorOff || "black";
    var backgroundOff   = config.backgroundOff || "white";
    var colorOn         = config.colorOn || "black";
    var backgroundOn    = config.backgroundOn || 'rgba(120,120,120,1.0)';
    var labelOn         = config.labelOn || 'On';
    var labelOff        = config.labelOff || 'Off';

    myToggle = elem.get(0);
//    console.log("toggle " + elem);

    // override setAttribute
    const mySetAttribute = myToggle.setAttribute;
    
//    console.log('myToggle.setAttribute: ' + myToggle.setAttribute);
    console.log('myToggle setAttribute: ' + myToggle.getAttribute('id'));

    myToggle.setAttribute = function (key, value) {
        // use call, to set the context and prevent illegal invocation errors
        mySetAttribute.call(myToggle, key, value);
//        console.log('ToggleID.setAttribute: ' + myToggle.getAttribute('id'));
        if (key == 'data-val') drawToggle(myToggle, value);
    };
    
    function drawToggle (toggle, val) {
        console.log('draw: ' + val + ', toggle: ' + toggle.getAttribute('id'));
        if (val == 1.0) {
            toggle.textContent = labelOn;
            toggle.style.color = colorOn;
            toggle.style.background = backgroundOn;
        }
        else {
            toggle.textContent = labelOff;
            toggle.style.color = colorOff;
            toggle.style.background = backgroundOff;
        }
    }
}
