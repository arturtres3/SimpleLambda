import { compileOmega } from "./output/Main/index.js";
import { compileSTLC } from "./output/Main/index.js";
import { compileSTLCSimple } from "./output/Main/index.js";
import { compileOmegaDefsUsed } from "./output/Main/index.js";
import { compileOmegaDefs } from "./output/Main/index.js";

//console.log(testFunction("if true then false else false"))

const compileButton = document.getElementById("compile")
const textIn = document.getElementById("textIn")
const textOut = document.getElementById("textOut")


compileButton.addEventListener('click', () => {
    switch (document.querySelector('input[name="language"]:checked').value){

        // STLC
        case "0": textOut.value = compileSTLCSimple(textIn.value)
                    break;
        // STLC ext
        case "1": textOut.value = compileSTLC(textIn.value)
                    break;
        // Lambda 2
        case "2": textOut.value = compileOmega(textIn.value)
                    break;
        // Lambda Omega
        case "3": textOut.value = compileOmega(textIn.value)
                    break;
        // Lambda C
        case "4": textOut.value = "?"//compileOmega(textIn.value)
                    break;
        // Omega todas definições 
        case "5": textOut.value = compileOmegaDefs(textIn.value)
                    break;
        // Omega definições
        case "6": textOut.value = compileOmegaDefsUsed(textIn.value)
                    break;
        default : textOut.value = "incompleto"

    }
})