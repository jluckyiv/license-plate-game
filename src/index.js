import { Elm } from "./Main.elm";
const fzy = require("fzy.js");
const sortBy = require("lodash.sortby");

const words = require("./wordlist.json");
const node = { node: document.getElementById("root") };

const app = Elm.Main.init({ node });

app.ports.getPlateCheck.subscribe(function (word) {
  app.ports.gotPlateCheck.send(checkPlate(word));
});

function checkPlate(plate) {
  const matches = words.filter((word) => fzy.hasMatch(plate, word));
  const sorted = sortBy(matches, (word) => -fzy.score(plate, word));
  return sorted;
}
