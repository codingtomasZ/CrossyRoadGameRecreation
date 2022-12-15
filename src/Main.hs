module Main where

import LI12223
import Tarefa1_2022li1g062
import Tarefa2_2022li1g062
import Tarefa4_2022li1g062
import Tarefa6_2022li1g062
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe


main :: IO ()
main = do play displayMode white 1 (jogo_teste) drawCharacter eventChange timeChange 
-- play  .tela.  .cordefundo.  .fps.  .estadoinicial.  .funções.
