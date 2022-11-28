module Main.Where 
import Graphic.Gloss

picture = Color red (Circle 100)
main = display window background picture

background = White
window = InWindow "LI1" (800,801) (0,0)