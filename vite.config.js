import { defineConfig } from 'vite'
import elmPlugin from 'vite-plugin-elm'
// import { ViteWebfontDownload } from 'vite-plugin-webfont-dl'
import viteImagemin from 'vite-plugin-imagemin'
import viteCompression from 'vite-plugin-compression'

export default defineConfig({
  plugins: [
    elmPlugin(),
    // ViteWebfontDownload(['https://fonts.googleapis.com/css2?family=Noto+Sans:ital,wght@0,100..900;1,100..900&display=swap']),
    viteImagemin({
      gifsicle: {
        optimizationLevel: 3,
        interlaced: false
      },
      optipng: {
        optimizationLevel: 7
      },
      mozjpeg: {
        quality: 80,
        progressive: true
      },
      pngquant: {
        quality: [0.8, 0.9],
        speed: 10
      },
      svgo: {
        plugins: ['preset-default']
      },
      webp: {
        quality: 80,
        method: 4,
        nearLossless: 95
      }
    }),
    viteCompression({
      algorithm: 'brotliCompress'
    })
  ]
})
