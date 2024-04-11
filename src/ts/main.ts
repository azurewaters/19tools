import "../css/styles.css";
import { Elm } from "../elm/Main.elm";
import { PDFDocument, StandardFonts, PageSizes, rgb, PDFImage } from "pdf-lib";

// Start the Elm application.
const app = Elm.Main.init({
  node: document.querySelector("main"),
});

window.document.documentElement.setAttribute("data-theme", "light");

// Change between themes manually
app.ports.changeTheme.subscribe(function (data: string) {
  window.document.documentElement.setAttribute("data-theme", data);
});

//  Resize pictures
app.ports.resizePicture.subscribe(async function (data: {
  name: string;
  contents: string;
}) {
  console.log(data);
  const img = new Image();
  img.onload = () => {
    //  ALL THE MEASUREMENTS ARE IN PIXELS

    //  A4's dimensions in pixels
    const pointToPixelRatio = 1.333;
    const landscapeA4Width = PageSizes.A4[1] * pointToPixelRatio;
    const availableLandscapeA4Height =
      (PageSizes.A4[0] - 60) * pointToPixelRatio;

    // Get the measurements of the image
    const imageWidth = img.naturalWidth;
    const imageHeight = img.naturalHeight;

    //  If the image is too large, resize it
    if (
      imageWidth > landscapeA4Width ||
      imageHeight > availableLandscapeA4Height
    ) {
      //  The image is larger than the A4 page, so we need to resize it
      let widthRatio = landscapeA4Width / imageWidth;
      let heightRatio = availableLandscapeA4Height / imageHeight;
      let ratio = Math.min(widthRatio, heightRatio);
      let newWidth = Math.floor(imageWidth * ratio);
      let newHeight = Math.floor(imageHeight * ratio);

      //  Now, draw the image according to these new dimensions
      const canvas = document.createElement("canvas");
      canvas.width = newWidth;
      canvas.height = newHeight;

      // Draw the image onto the canvas
      const ctx = canvas.getContext("2d");
      if (ctx) {
        ctx.drawImage(img, 0, 0, canvas.width, canvas.height);
        var resizedDataUrl = canvas.toDataURL("image/png");
        app.ports.gotResizedPicture.send({
          name: data.name,
          contents: resizedDataUrl,
          width: newWidth,
          height: newHeight,
        });
      }
    } else {
      //  If the image is smaller than the , just dispatch the event with the original image
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
  sponsorsName: string;
  pictures: Picture[];
}) {
  const pictures: Picture[] = data.pictures;
  if (pictures.length > 0) {
    const document = await PDFDocument.create();
    const helveticaFont = await document.embedFont(StandardFonts.Helvetica, {
      subset: true,
    });
    const helveticaBoldFont = await document.embedFont(
      StandardFonts.HelveticaBold,
      { subset: true },
    );

    //  Add the title page
    const titlePage = document.addPage([PageSizes.A4[1], PageSizes.A4[0]]);
    titlePage.setFont(helveticaFont);

    titlePage.drawText("Proof of Relationship - Photographs", {
      x: 100,
      y: 500,
      font: helveticaBoldFont,
      size: 20,
      color: rgb(0, 0, 0),
    });

    titlePage.drawText(
      `This document contains ${data.pictures.length} photographs that prove that my sponsor, ${data.sponsorsName}, and I are in a relationship.`,
      {
        x: 100,
        y: 450,
        font: helveticaFont,
        size: 12,
        color: rgb(0, 0, 0),
        maxWidth: PageSizes.A4[1] - 200,
      },
    );

    //  Loop through all the pictures and put them and their descriptions into the PDF
    pictures.forEach(async (picture: Picture) => {
      try {
        //  ALL THE MEASUREMENTS ARE IN POINTS
        //  Add a new page
        const page = document.addPage([PageSizes.A4[1], PageSizes.A4[0]]); //  Landscape

        //  Calculate the new dimensions of the picture, preserving the aspect ratio
        let pageWidth = page.getWidth();
        let availableHeight = page.getHeight() - 60;

        //  The picture's dimensions in points
        let pictureWidth = picture.width * 0.75;
        let pictureHeight = picture.height * 0.75;

        //  Add the picture at the centre and top of the page
        let eI: PDFImage = await document.embedPng(picture.contents);
        page.drawImage(eI, {
          x: (pageWidth - pictureWidth) / 2,
          y: 60 + (availableHeight - pictureHeight) / 2,
          width: pictureWidth,
          height: pictureHeight,
        });

        //  Add the description of the picture
        page.setFont(helveticaFont);
        page.drawText(picture.description, {
          x: 100,
          y: 40,
          font: helveticaFont,
          size: 12,
          maxWidth: pageWidth - 200,
          lineHeight: 15,
          color: rgb(0, 0, 0),
        });
      } catch (error) {
        console.log(error);
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
