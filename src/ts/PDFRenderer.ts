import { PDFDocument, StandardFonts, PageSizes, rgb, PDFImage } from "pdf-lib";

class PDFRenderer extends HTMLElement {
  static observedAttributes = ["pictures"];

  constructor() {
    super();
    alert("PDFRenderer created");
  }

  connectedCallback() {}

  attributeChangedCallback(name: string, _: any, newValue: any) {
    alert(`Attribute ${name} changed.`);
    if (name === "pictures") {
      //  Here's where we render a PDF
      //  First, read the JSON. It contains a bunch of Picture objects.
      //  Next, start a PDFDocument. All pages are in A4 landscape size.
      //  Add a default page with text that says "Proof of Relationship".
      //  Then, loop through the Picture objects and add their contents in the centre and and at the top, and their description below them.
      //  Finally, save the document and raise an event with the PDF encoded as a dataURL as the detail.
      this.handlePicturesChange(newValue);
    }
  }

  handlePicturesChange = async (json: string) => {
    const parsedJSON = JSON.parse(json);
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
    let pictures: Picture[] = parsedJSON.pictures as Picture[];
    pictures.forEach(async (picture: Picture) => {
      try {
        const page = document.addPage([PageSizes.A4[1], PageSizes.A4[0]]); //  Landscape
        page.setFont(helveticaFont);

        //  Sizes
        const pageWidthInPoints = page.getWidth();
        const pageHeightInPoints = page.getHeight();

        //  Add the picture at the centre and top of the page
        let eI: PDFImage = await document.embedPng(picture.contents);
        page.drawImage(eI, {
          x: (pageWidthInPoints - eI.height) / 2,
          y: pageHeightInPoints,
          width: eI.width,
          height: eI.height,
        });

        //  Add the description of the picture
        page.drawText(picture.description, {
          x: 100,
          y: 80,
          font: helveticaFont,
          size: 12,
          maxWidth: pageWidthInPoints - 200,
          lineHeight: 15,
          color: rgb(0, 0, 0),
        });
      } catch (error) {
        console.error("Error adding picture to PDF: " + error);
      }
    });

    //  Save the document and raise an event with the PDF encoded as a dataURL as the detail
    const pdf = await document.save();
    const file = new File([pdf], "proof_of_relationship.pdf", {
      type: "application/pdf",
    });
    this.dispatchEvent(new CustomEvent("rendered", { detail: { file: file } }));
    alert("PDF created");
  };
}

customElements.define("pdf-renderer", PDFRenderer);
