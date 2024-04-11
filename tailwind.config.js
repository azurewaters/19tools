module.exports = {
  content: [
    "./index.html",
    "./src/js/main.ts",
    "./src/css/styles.css",
    "./src/elm/**/*.elm",
  ],
  theme: {
    extend: {
      fontFamily: {
        sans: ["Inter var", "system-ui", "sans-serif"],
      },
    },
  },
  plugins: [
    require("@tailwindcss/typography"),
    require("@tailwindcss/forms"),
    require("@tailwindcss/aspect-ratio"),
    require("@tailwindcss/container-queries"),
  ],
  daisyui: {
    themes: ["light", "dark"],
  },
};
