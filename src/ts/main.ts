import "../css/styles.css";
import { Elm } from "../elm/Main.elm";
import { PDFDocument, StandardFonts, PageSizes, rgb, PDFImage } from "pdf-lib";

// Start the Elm application.
const app = Elm.Main.init({
  node: document.querySelector("main"),
});

// Change between themes manually
app.ports.changeTheme.subscribe(function (data: string) {
  window.document.documentElement.setAttribute("data-theme", data);
});

//  Resize pictures
app.ports.resizePicture.subscribe(async function (data: {
  name: string;
  contents: string;
}) {
  const img = new Image();
  img.onload = () => {
    // Get the measurements of the image
    const imageWidth = img.naturalWidth;
    const imageHeight = img.naturalHeight;

    //  A4's dimensions in pixels
    const landscapeA4Width = 842 * 1.3333333333333333;
    const landscapeA4Height = 595 * 1.3333333333333333;
    const availableLandscapeA4Height =
      landscapeA4Height - 60 * 1.333333333333333;

    //  If the image is too large, resize it
    if (
      imageWidth > landscapeA4Width ||
      imageHeight > availableLandscapeA4Height
    ) {
      //  Calculate the new dimensions of the image in points if it were to fit the width and available height
      let newWidth: number;
      let newHeight: number;
      if (imageWidth > imageHeight) {
        newWidth = landscapeA4Width;
        newHeight = (imageHeight / imageWidth) * landscapeA4Width;
      } else {
        newHeight = availableLandscapeA4Height;
        newWidth = (imageWidth / imageHeight) * availableLandscapeA4Height;
      }

      //  Now, draw the image according to these new dimensions
      const canvas = document.createElement("canvas");
      canvas.width = newWidth;
      canvas.height = newHeight;

      // Draw the image onto the canvas
      const ctx = canvas.getContext("2d");
      if (ctx) {
        ctx.drawImage(img, 0, 0, canvas.width, canvas.height);
        var resizedDataUrl = canvas.toDataURL();
        app.ports.gotResizedPicture.send({
          name: data.name,
          contents: resizedDataUrl,
          width: newWidth,
          height: newHeight,
        });
      }
    } else {
      //  If the image is not too large, just dispatch the event with the original image
      app.ports.gotResizedPicture.send({
        name: data.name,
        contents: data.contents,
        width: imageWidth,
        height: imageHeight,
      });
    }
  };
  img.src = data.contents;
});

//  Render the PDF
app.ports.renderTheProofOfRelationship.subscribe(async function (data: {
  pictures: Picture[];
}) {
  const pictures: Picture[] = data.pictures;
  if (pictures.length > 0) {
    const document = await PDFDocument.create();
    const helveticaFont = await document.embedFont(StandardFonts.Helvetica);

    //  Add the title page
    const titlePage = document.addPage([PageSizes.A4[1], PageSizes.A4[0]]);
    titlePage.setFont(helveticaFont);
    titlePage.drawText("Proof of Relationship", {
      x: 100,
      y: 400,
      font: helveticaFont,
      size: 20,
      color: rgb(0, 0, 0),
    });
    //  Loop through all the pictures and put them and their descriptions into the PDF
    pictures.forEach(async (picture: Picture) => {
      try {
        const page = document.addPage([PageSizes.A4[1], PageSizes.A4[0]]); //  Landscape
        page.setFont(helveticaFont);
        //  Sizes
        const pageWidthInPoints = page.getWidth();
        const pageHeightInPoints = page.getHeight();

        //  Debug
        page.drawText(
          `pageWidthInPoints: ${pageWidthInPoints}, pageHeightInPoints: ${pageHeightInPoints}, pictureWidthInPoints: ${
            picture.width / 1.3333333333333333
          }, pictureHeightInPoints: ${picture.height / 1.3333333333333333}`,
          {
            x: 0,
            y: 0,
            maxWidth: pageWidthInPoints,
          },
        );

        //  Add the picture at the centre and top of the page
        let eI: PDFImage = await document.embedPng(picture.contents);
        page.drawImage(eI, {
          x: (pageWidthInPoints - picture.width / 1.3333333333333333) / 2,
          y: 60,
          width: picture.width / 1.3333333333333333,
          height: picture.height / 1.3333333333333333 - 60,
        });

        //  Add the description of the picture
        page.drawText(picture.description, {
          x: 100,
          y: 40,
          font: helveticaFont,
          size: 12,
          maxWidth: pageWidthInPoints - 200,
          lineHeight: 15,
          color: rgb(0, 0, 0),
        });
      } catch (error) {
        alert("An error occurred here: " + error);
        console.error("Error adding picture to PDF: " + error);
      }
    });

    //  Save the document and send it back to Elm
    const pdf = await document.save();
    const file = new File([pdf], "proof_of_relationship.pdf", {
      type: "application/pdf",
    });
    app.ports.gotTheProofOfRelationship.send(file);
  }
});
