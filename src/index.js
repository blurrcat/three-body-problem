import './base.css'
import '../node_modules/purecss/build/base-min.css'
import '../node_modules/purecss/build/buttons-min.css'

import { Elm } from './Main.elm'

const main = document.createElement('div')
document.body.append(main)
Elm.Main.init({
  node: main
})
