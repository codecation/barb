require('./css/index.scss');

var Elm = require('../src/Main');
var app = Elm.Main.embed(document.getElementById('main'));

var setImageDimensions = function(img) {
  imageDimensions = {
    "width": img.width,
    "height": img.height
  }
}

var imageDimensions = {};

var getUploadedImageData = function() {
  var canvas = document.createElement('canvas');
  var context = canvas.getContext('2d');
  var img = document.getElementsByClassName('images-original_image_container-image')[0];
  setImageDimensions(img);
  canvas.width = imageDimensions.width;
  canvas.height = imageDimensions.height;
  context.drawImage(img, 0, 0);
  return context.getImageData(0, 0, img.width, img.height).data;
}

var getGeneratedImageData = function() {
  var canvas = document.getElementsByClassName('images-image_container-generated_image_canvas')[0].childNodes[0].childNodes[0];
  return canvas.getContext('2d').getImageData(0, 0, imageDimensions.width, imageDimensions.height).data;
};

app.ports.requestUploadedImage.subscribe(function(_) {
  var uploadedImageData = Array.from(getUploadedImageData());
  app.ports.uploadedImage.send(uploadedImageData)
});

app.ports.requestCandidateImage.subscribe(function(_) {
  var generatedImageData = Array.from(getGeneratedImageData());
  app.ports.candidateImage.send(generatedImageData);
});
