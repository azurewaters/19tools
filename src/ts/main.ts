import '../css/styles.css'
import { Elm } from '../elm/Main.elm'

// Start the Elm application.
const app = Elm.Main.init({
  node: document.querySelector('main')
})

// Change between themes manually
app.ports.changeTheme.subscribe(function (data) {
  window.document.documentElement.setAttribute('data-theme', data)
})

// Here's where we subscribe to the renderThePDF port
app.ports.renderThePDF.subscribe(function (data) {
  // This is where we would use the data to render the PDF
  console.log(data)
  window.alert('This is where we would render the PDF')
})
