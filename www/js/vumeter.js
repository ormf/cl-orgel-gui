function vumeter(elem, config){

    // Settings
    var max             = config.max || 100;
    var boxCount        = config.boxCount || 10;
    var boxCountRed     = config.boxCountRed || 2;
    var boxCountYellow  = config.boxCountYellow || 3;
    var boxGapFraction  = config.boxGapFraction || 0.2;
//    var jitter          = config.jitter || 0.02;

    // Colours
    var redOn     = 'rgba(255,47,30,0.9)';
    var redOff    = 'rgba(64,12,8,0.9)';
    var yellowOn  = 'rgba(255,215,5,0.9)';
    var yellowOff = 'rgba(64,53,0,0.9)';
    var greenOn   = 'rgba(53,255,30,0.9)';
    var greenOff  = 'rgba(13,64,8,0.9)';

    console.log('boxCount: ' + boxCount);
    // Derived and starting values

    var canvas = elem.get(0)

    var width = canvas.width;
    var height = canvas.height;
    var curVal = 0;

    
    console.log( 'width: ' + width + ', height: ' + height);
    
    console.log( 'elem: ' + elem + ', canvas: ' + canvas);

    // const VuValChange = new Event('set-data-val', {
    //     bubbles: false,
    //     cancelable: false,
    //     composed: false
    // })
    // 
    // 
    // function setDataListener () {
    //     console.log("I'm listening on a set-data-val-event");
    //                };
    // 
    // elem.dispatchEvent(VuValChange);
    // elem.addEventListener('set-data-val', setDataListener);

    // Canvas starting state
    
    const mySetAttribute = canvas.setAttribute;
    // override setAttribte
    canvas.setAttribute = function (key, value) {
//        console.log("--trace, key: " + key + ', value: ' + value);
        // use call, to set the context and prevent illegal invocation errors
        mySetAttribute.call(canvas, key, value);
        if (key == 'data-val') drawBoxes(c, value);
    };
//     console.log (elem.setAttribute);
    // Gap between boxes and box height
    var boxHeight = height / (boxCount + (boxCount+1)*boxGapFraction);
    var boxGapY = boxHeight * boxGapFraction;

    var boxWidth = width - (boxGapY*2);
    var boxGapX = (width - boxWidth) / 2;


    var c = canvas.getContext('2d');

    c.save();
    c.beginPath();
    c.rect(0, 0, width, height);
    c.fillStyle = '#333';
    c.fill();
    c.restore();
    drawBoxes(c, 60);

    
//    drawBoxes(c, 50);
    
    // Main draw loop
    var draw = function(){
        
        var targetVal = parseInt(canvas.dataset.val, 10);


        // Gradual approach
        // Apply jitter
        if (curVal < 0) {
            curVal = 0;
        }
//            console.log('context: ' + c);
        c.save();
        c.beginPath();
        c.rect(0, 0, width, height);
        c.fillStyle = '#333';
        c.fill();
        c.restore();
        drawBoxes(c, targetVal);
        
        curVal = targetVal;
        requestAnimationFrame(draw);
    };

    // Draw the boxes
    function drawBoxes(c, val){
        c.save(); 
        c.translate(boxGapX, boxGapY);
        for (var i = 0; i < boxCount; i++){
            var id = getId(i);
            
            c.beginPath();
            c.rect(0, 0, boxWidth, boxHeight);
            c.fillStyle = getBoxColor(id, val);
            c.fill();
            c.translate(0, boxHeight + boxGapY);
        }
        c.restore();
    }

    // Get the color of a box given it's ID and the current value
    function getBoxColor(id, val){
        // on colours
//        console.log('id: ' + id + ', val: ' + val);
        if (id > boxCount - boxCountRed){
            return isOn(id, val)? redOn : redOff;
        }
        if (id > boxCount - boxCountRed - boxCountYellow){
            return isOn(id, val)? yellowOn : yellowOff;
        }
        return isOn(id, val)? greenOn : greenOff;
    }

    function getId(index){
        // The ids are flipped, so zero is at the top and
        // boxCount-1 is at the bottom. The values work
        // the other way around, so align them first to
        // make things easier to think about.
        return Math.abs(index - (boxCount - 1)) + 1;
    }

    function isOn(id, val){
        // We need to scale the input value (0-max)
        // so that it fits into the number of boxes
        var maxOn = Math.ceil((val/max) * boxCount);
        return (id <= maxOn);
    }

    // Trigger the animation
//    draw();
}

function vumeter2 (elem, config) {
    console.log(elem);

}
