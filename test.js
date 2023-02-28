import { testFunction } from "./output/Main/index.js";
import { compileSTLC } from "./output/Main/index.js";
import { compileSTLCSimple } from "./output/Main/index.js";

//console.log(testFunction("if true then false else false"))

const compileButton = document.getElementById("compile")
const textIn = document.getElementById("textIn")
const textOut = document.getElementById("textOut")


compileButton.addEventListener('click', () => {
    switch (document.querySelector('input[name="language"]:checked').value){
        case "0": textOut.value = compileSTLC(textIn.value)
                    console.log("normal ");
                    break;

        case "1": textOut.value = compileSTLCSimple(textIn.value)
                    console.log("simples ");
                    break;


    }
})