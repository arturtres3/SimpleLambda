import { compileSTLC } from "./output/Main/index.js";
import { compileSTLCDefs } from "./output/Main/index.js";
import { compileSTLCSimple } from "./output/Main/index.js";
import { compileL2 } from "./output/Main/index.js";
import { compileL2Defs } from "./output/Main/index.js";
import { compileLC } from "./output/Main/index.js";
import { compileLCDefs } from "./output/Main/index.js";
import { compileOmega } from "./output/Main/index.js";
import { compileOmegaDefs } from "./output/Main/index.js";

//console.log(testFunction("if true then false else false"))

const compileButton = document.getElementById("compile")
const textIn = document.getElementById("textIn")
const textOut = document.getElementById("textOut")
const targetSelect = document.getElementsByClassName("targetSelect")[0]

const stlcTargetSelect = document.getElementById("STLC")
const stlcExt = document.getElementById("ExtSTLC")
const extLabel = document.getElementById("ExtLabel")
const defsSwitch = document.getElementById("Defs")
const openSimBtn = document.getElementById("open")

var changeEvent = new Event('change');

function goToLink(url) {
    var win = window.open(url, '_blank');
    win.focus();
  }

targetSelect.addEventListener('change', function() {
    if (stlcTargetSelect.checked) {
        stlcExt.style.display = "inline-block"
        extLabel.style.display = "inline-block"
    }else{
        stlcExt.style.display = "none"
        extLabel.style.display = "none"
        stlcExt.checked = false
        stlcExt.dispatchEvent(changeEvent);
    }
  });

stlcExt.addEventListener('change', function() {
    if (this.checked) {
        defsSwitch.checked = false
        defsSwitch.disabled = true
    }else{
        defsSwitch.disabled = false
    }
  });


compileButton.addEventListener('click', () => {
    switch (document.querySelector('input[name="language"]:checked').value){

        // STLC
        case "0": if (defsSwitch.checked){
                        textOut.value = compileSTLCDefs(textIn.value) 
                    }else{
                        if(stlcExt.checked) {
                            textOut.value = compileSTLC(textIn.value) 
                        }else{
                            textOut.value = compileSTLCSimple(textIn.value)  
                        }
                    }
                    break;
        // Lambda 2
        case "1": if (defsSwitch.checked){
                        textOut.value = compileL2Defs(textIn.value)
                    }else{
                        textOut.value = compileL2(textIn.value)
                    }
                    break;
        // Lambda Omega
        case "2": if (defsSwitch.checked){
                        textOut.value = compileOmegaDefs(textIn.value)
                    }else{
                        textOut.value = compileOmega(textIn.value)
                    }
                    break;
        // Lambda C
        case "3": if (defsSwitch.checked){
                        textOut.value = compileLCDefs(textIn.value)
                    }else{
                        textOut.value = compileLC(textIn.value)
                    }
                    break;

        default : textOut.value = "incompleto"

    }
})


openSimBtn.addEventListener('click', () => {

    textOut.select()
    // navigator.clipboard.writeText(textOut.value);
    document.execCommand('copy')

    switch (document.querySelector('input[name="language"]:checked').value){

        // STLC
        case "0": goToLink("https://www.inf.ufrgs.br/~rma/simuladores/lambdaTyped.html")
                    break;
        // Lambda 2
        case "1": goToLink("https://www.inf.ufrgs.br/~rma/simuladores/lambda2.html")
                    break;
        // Lambda Omega
        case "2": goToLink("https://www.inf.ufrgs.br/~rma/simuladores/lambdaOmega.html")
                    break;
        // Lambda C
        case "3": goToLink("https://www.inf.ufrgs.br/~rma/simuladores/lambdac.html")
                    break;

        default: break;

    }
})