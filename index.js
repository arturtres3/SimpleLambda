import { compileLambda } from "./output/Main/index.js";
import { compileLambdaDefs } from "./output/Main/index.js";

import { compileSTLC } from "./output/Main/index.js";
import { compileSTLCNewSim } from "./output/Main/index.js";
import { compileSTLCDefs } from "./output/Main/index.js";
import { compileSTLCDefsNewSim } from "./output/Main/index.js";
import { compileSTLCSimple } from "./output/Main/index.js";
import { compileSTLCSimpleNewSim } from "./output/Main/index.js";

import { compileL2 } from "./output/Main/index.js";
import { compileL2NewSim } from "./output/Main/index.js";
import { compileL2Defs } from "./output/Main/index.js";
import { compileL2DefsNewSim } from "./output/Main/index.js";

import { compileOmega } from "./output/Main/index.js";
import { compileOmegaNewSim } from "./output/Main/index.js";
import { compileOmegaDefs } from "./output/Main/index.js";
import { compileOmegaDefsNewSim } from "./output/Main/index.js";

import { compileLC } from "./output/Main/index.js";
import { compileLCDefs } from "./output/Main/index.js";


const compileButton = document.getElementById("compile")
const textIn = document.getElementById("textIn")
const textOut = document.getElementById("textOut")
const targetSelect = document.getElementsByClassName("targetSelect")[0]

const stlcTargetSelect = document.getElementById("STLC")
const stlcExt = document.getElementById("ExtSTLC")
const extLabel = document.getElementById("ExtLabel")

const lambdaTargetSelect = document.getElementById("Lambda")
const newSimInput = document.getElementById("NewSim")
const newSimLabel = document.getElementById("NewSimLabel")

const openSimBtn = document.getElementById("open")

const defsSwitch = document.getElementById("Defs")
const newSimSwitch = document.getElementById("NewSim")

var changeEvent = new Event('change');

let simulator = [0, false]

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

targetSelect.addEventListener('change', function() {
    if (!lambdaTargetSelect.checked) {
        newSimInput.style.display = "inline-block"
        newSimLabel.style.display = "inline-block"
    }else{
        newSimInput.style.display = "none"
        newSimLabel.style.display = "none"
        newSimInput.checked = false
        newSimInput.dispatchEvent(changeEvent);
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

    let selector = document.querySelector('input[name="language"]:checked').value + "," +
                    defsSwitch.checked + "," +
                    newSimSwitch.checked  


    // selector = "linguagem, Definições?, NovoSimulador?"

    switch (selector) {

        // Lambda Sem Tipos

        case "0,false,false":   textOut.value = compileLambda(textIn.value) 
                        break;

        case "0,true,false":    textOut.value = compileLambdaDefs(textIn.value) 
                        break;
        
        case "0,false,true":
        case "0,true,true" :    
                        break;

        // STLC
        
        case "1,false,false":   if(stlcExt.checked) {
                                    textOut.value = compileSTLC(textIn.value) 
                                }else{
                                    textOut.value = compileSTLCSimple(textIn.value)  
                                }
                        break;

        case "1,true,false":    textOut.value = compileSTLCDefs(textIn.value)
                        break;

        case "1,false,true":    if(stlcExt.checked) {
                                    textOut.value = compileSTLCNewSim(textIn.value)
                                }else{
                                    textOut.value = compileSTLCSimpleNewSim(textIn.value)
                                }
                        break;

        case "1,true,true":     textOut.value = compileSTLCDefsNewSim(textIn.value)
                        break;

        // Lambda 2

        case "2,false,false":   textOut.value = compileL2(textIn.value)
                        break;
        case "2,true,false":    textOut.value = compileL2Defs(textIn.value)
                        break;
        case "2,false,true":    textOut.value = compileL2NewSim(textIn.value)
                        break;     
        case "2,true,true":     textOut.value = compileL2DefsNewSim(textIn.value)
                        break;

        // Lambda Omega

        case "3,false,false":   textOut.value = compileOmega(textIn.value)
                        break;
        case "3,true,false":    textOut.value = compileOmegaDefs(textIn.value)
                        break;     
        case "3,false,true":    textOut.value = compileOmegaNewSim(textIn.value)
                        break;
        case "3,true,true":     textOut.value = compileOmegaDefsNewSim(textIn.value)
                        break;

        // Lambda C

        case "4,false,false":   textOut.value = compileLC(textIn.value)
                break;
        case "4,true,false":    textOut.value = compileLCDefs(textIn.value)
                break;     
        case "4,false,true":    textOut.value = compileLC(textIn.value)     
                break;
        case "4,true,true":     textOut.value = compileLCDefs(textIn.value)
                break;


        default:        textOut.value = "?"
    }

    //console.log(selector);


    simulator[0] = document.querySelector('input[name="language"]:checked').value
    simulator[1] = newSimSwitch.checked

})


openSimBtn.addEventListener('click', () => {

    textOut.select()
    // navigator.clipboard.writeText(textOut.value);
    document.execCommand('copy')

    if(simulator[1]){
        goToLink("https://www.inf.ufrgs.br/~ebchandelier/")
        return
    }

    switch (simulator[0]){
        
        // Sem Tipos
        case "0": goToLink("https://www.inf.ufrgs.br/~rma/simuladores/lambda.html")
                    break;
        // STLC
        case "1": goToLink("https://www.inf.ufrgs.br/~rma/simuladores/lambdaTyped.html")
                    break;
        // Lambda 2
        case "2": goToLink("https://www.inf.ufrgs.br/~rma/simuladores/lambda2.html")
                    break;
        // Lambda Omega
        case "3": goToLink("https://www.inf.ufrgs.br/~rma/simuladores/lambdaOmega.html")
                    break;
        // Lambda C
        case "4": goToLink("https://www.inf.ufrgs.br/~rma/simuladores/lambdac.html")
                    break;

        default: break;
    }
})