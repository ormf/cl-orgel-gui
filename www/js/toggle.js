function toggle (elem, config) {
    var colorOff        = config.colorOff || "black";
    var backgroundOff   = config.backgroundOff || "white";
    var labelOff        = config.labelOff || 'Off';
    var valueOff        = config.valueOff || '0.0';
    var colorOn         = config.colorOn || "black";
    var backgroundOn    = config.backgroundOn || 'rgba(120,120,120,1.0)';
    var labelOn         = config.labelOn || 'On';
    var valueOn        = config.valueOn || '1.0';

    var myToggle = elem.get(0);
//    console.log("toggle " + elem);

    // override setAttribute
    const mySetAttribute = myToggle.setAttribute;

//    console.log('myToggle.setAttribute: ' + myToggle.setAttribute);
//    console.log('myToggle setAttribute: ' + myToggle.getAttribute('id'));

    myToggle.setAttribute = function (key, value) {
//        console.log("--trace, key: " + key + ', value: ' + value);
//        console.log('myToggle setAttribute: ' + myToggle.getAttribute('id'));
        // use call, to set the context and prevent illegal invocation errors
        mySetAttribute.call(myToggle, key, value);
        //        if (key == 'data-db') drawBoxes(c, value);
        if (key == 'data-val') drawToggle(myToggle, value);

    }

    
//     myToggle.setAttribute = function (key, value) {
//         // use call, to set the context and prevent illegal invocation errors
//         mySetAttribute.call(myToggle, key, value);
// //        console.log('ToggleID.setAttribute: ' + myToggle.getAttribute('id'));
//         if (key == 'data-val') drawToggle(myToggle, value);
//     };
    
    var  drawToggle = function (toggle, val) {
//        console.log('draw: ' + val + ', valueOn: ' + valueOn);
        if (val == valueOn) {
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
