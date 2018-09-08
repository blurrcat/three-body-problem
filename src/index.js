import 'purecss/build/pure.css'
import 'colors.css/css/colors.css'
import { Elm } from './Main.elm'

const main = document.createElement('div')
document.body.append(main)
Elm.Main.init({
  node: main
})
