import { Elm } from "./Main.elm";

const words = require("./collins-scrabble-words-2019.json");
const node = { node: document.getElementById("root") };

const app = Elm.Main.init({ node });

app.ports.getWordCheck.subscribe(function (word) {
  app.ports.gotWordCheck.send(checkWord(word));
});

function checkWord(word) {
  return words.includes(word.toUpperCase());
}
