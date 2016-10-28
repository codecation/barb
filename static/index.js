
// pull in desired CSS/SASS files
require('./css/index.scss');

// inject bundled Elm app into div#main

var Elm = require('../src/Main');
var app = Elm.Main.embed(document.getElementById('main'));

app.ports.requestImageDetails.subscribe(function(_) {
  debugger;
});
