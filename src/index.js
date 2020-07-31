import '../node_modules/purecss/build/base.css'
import '../node_modules/purecss/build/buttons.css'
import { Elm } from './Main.elm'

const main = document.createElement('div')
document.body.append(main)
Elm.Main.init({
  node: main
})
