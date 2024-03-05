import '../css/styles.css'
import { Elm } from '../elm/Main.elm'
import type Picture  from 'Picture.ts'
import * as pdfMake from "pdfmake/build/pdfmake";
import * as pdfFonts from 'pdfmake/build/vfs_fonts';

(<any>pdfMake).vfs = pdfFonts.pdfMake.vfs;


// Start the Elm application.
const app = Elm.Main.init({
  node: document.querySelector('main')
})

// Change between themes manually
app.ports.changeTheme.subscribe(function (data) {
  window.document.documentElement.setAttribute('data-theme', data)
})


// Here's where we subscribe to the renderThePDF port
app.ports.renderThePDF.subscribe(function (documentDefinition: Object) {
  // This is where we render the PDF and send it back to Elm as a data URL
  pdfMake.createPdf(documentDefinition).download('xyz.pdf')
})
