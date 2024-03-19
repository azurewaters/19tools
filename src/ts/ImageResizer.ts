class ImageResizer extends HTMLElement {
  static observedAttributes = ["src"];

  constructor() {
    super();
  }

  // connectedCallback() {}

  attributeChangedCallback(name: string, _: any, newValue: any): void {
    if (name === "src") {
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
            this.dispatchEvent(
              new CustomEvent("resized", {
                bubbles: true,
                detail: {
                  dataURL: resizedDataUrl,
                  width: newWidth,
                  height: newHeight,
                },
              }),
            );
          }
        } else {
          //  If the image is not too large, just dispatch the event with the original image
          this.dispatchEvent(
            new CustomEvent("resized", {
              detail: {
                dataURL: newValue,
                width: imageWidth,
                height: imageHeight,
              },
            }),
          );
        }
      };
      img.src = newValue;
    }
  }
}

customElements.define("image-resizer", ImageResizer);
