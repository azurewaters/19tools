import '../css/styles.css'
import { Elm } from '../elm/Main.elm'
import Picture  from 'Picture.ts'
//import * as pdfMake from "pdfmake/build/pdfmake";
//import * as pdfFonts from 'pdfmake/build/vfs_fonts';

//(<any>pdfMake).vfs = pdfFonts.pdfMake.vfs;


import { PDFDocument, StandardFonts, PageSizes, rgb } from 'pdf-lib'

// Start the Elm application.
const app = Elm.Main.init({
  node: document.querySelector('main')
})

// Change between themes manually
app.ports.changeTheme.subscribe(function (data) {
  window.document.documentElement.setAttribute('data-theme', data)
})

// Here's where we subscribe to the renderThePDF port
// app.ports.renderThePDF.subscribe(function (data: Object) {
  //   // This is where we render the PDF and send it back to Elm as a File object
  //   let documentGenerator = pdfMake.createPdf(data.documentDefinition)
  //   documentGenerator.getBlob((blob: Blob) => {
    //     let file:File = new File([blob], data.documentName, {type: "application/pdf"})
    //     app.ports.gotThePDF.send(file)
    //   })
    // })


app.ports.renderThePDFWithPictures.subscribe(async function (data: Object) {
  // This is where we render the PDF and send it back to Elm as a File object using the PDFLib library
  //  What we have been supplied with here are a list of Picture objects
  //  Put each of those pictures into a PDF document, with the picture nthon the left and its description on the right of the picture.
  const document:PDFDocument = await PDFDocument.create()
  const helveticaFont = await document.embedFont(StandardFonts.Helvetica)

  //  Now, loop through all the pictures and put them and their descriptions into the PDF
  let pictures: Picture[] = data.pictures as Picture[]
  pictures.forEach(async (picture:Picture) => {

    try {
      const page = document.addPage([ PageSizes.A4[1], PageSizes.A4[0] ])  //  Landscape
      page.setFont(helveticaFont)

      //  Sizes
      // const { pageWidth, pageHeight } = page.getSize()
      const pageWidth = page.getWidth()
      const pageHeight = page.getHeight()
      const textWidth = 200
      const availableWidth = pageWidth - textWidth

      //  Add the picture to the PDF
      //  First, we need to check whethere the picture is a JPG or a PNG
      let embeddedImage:PDFImage
      if (picture.name.endsWith('.jpg') || picture.name.endsWith('.jpeg')) {
        embeddedImage = await document.embedJpg(picture.contentInURLFormat)
      } else if (String.endsWith(picture.name, '.png')) {
        embeddedImage = await document.embedPng(picture.contentInURLFormat)
      }
      //  Next, we need to scale the picture to fit the page's available width and height
      let scaled = embeddedImage.scaleToFit(availableWidth, pageHeight)
      page.drawImage(embeddedImage, {
        x: 0,
        y: (pageHeight - scaled.height) / 2,
        width: scaled.width,
        height: scaled.height
      })

      //  Now, add the description of the picture on the right of the picture
      page.drawText(picture.description, {
        x: scaled.width + 20,
        y: ((pageHeight - scaled.height) / 2) + (scaled.height - 20),
        font: helveticaFont,
        size: 12,
        maxWidth: textWidth - 40,
        lineHeight: 15,
        color: rgb(0, 0, 0)
      })
    } catch (e) {
      alert(e)
    }
  });

  //  Now, download the file as a Blob
  const pdfBlob:Blob = await document.save()
  const file:File = new File([pdfBlob], data.documentName, {type: "application/pdf"})
  app.ports.gotThePDF.send(file)
})
