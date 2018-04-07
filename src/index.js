import 'purecss/build/pure.css'
import 'colors.css/css/colors.css'
import Elm from './Main.elm'

const main = document.createElement('div')
Elm.Main.embed(main)
document.body.append(main)
