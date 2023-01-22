import { testFunction } from "./output/Main/index.js";

//console.log(testFunction("if true then false else false"))

const compileButton = document.getElementById("compile")
const textIn = document.getElementById("textIn")
const textOut = document.getElementById("textOut")


compileButton.addEventListener('click', () => {
    textOut.value = testFunction(textIn.value)
})