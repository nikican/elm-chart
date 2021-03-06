// pull in desired CSS/SASS files
require( './styles/main.scss' );
var $ = jQuery = require( '../../node_modules/jquery/dist/jquery.js' );           // <--- remove if Bootstrap's JS not needed
require( '../../node_modules/bootstrap-sass/assets/javascripts/bootstrap.js' );   // <--- remove if Bootstrap's JS not needed 
var data = require('../data');

// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );
Elm.Main.fullscreen({
  values: data
});
