{- |
Module      : Tarefa6_2022li1g062
Description : Desenvolvimento da parte gráfica do jogo
Copyright   : Tomas Henrique Alves Melo <a104529@alunos.uminho.pt>
              José Diogo Azevedo Martins <a104443@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/2023.
-}

module Tarefa6_2022li1g062 where

import LI12223
import Test.HUnit
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe


type State = (Int,(Float,Float))

-- P E R S O N A G E N S --

chicken :: Picture
chicken = color white (thickCircle 4 15)

pig :: Picture
pig = color pink (thickCircle 4 15)

cow :: Picture
cow = color black (thickCircle 4 15)

drawCharacter ::State -> Picture
drawCharacter (0, (x,y)) = translate x y chicken
drawCharacter (1, (x,y)) = translate x y pig
drawCharacter (2, (x,y)) = translate x y cow

-- T E R R E N O S--

desenha_ter :: Terreno -> Picture 
desenha_ter Rio v = RioP
desenha_ter Estrada v = EstradaP
desenha_ter Relva = RelvaP

rioP :: Picture
rioP =

estradaP :: Picture
estradaP =

relvaP :: Picture
relvaP =

-- O B S T Á C U L O S --

desenha_obs :: Obstaculo -> Picture 
desenha_obs Tronco = TroncoP
desenha_obs Carro = CarroP
desenha_obs Arvore = ArvoreP

troncoP :: Picture
troncoP = color brown (Polygon [(0,0),(40,0),(40,40),(0,40)])

carroP :: Picture
carroP = color red (Polygon [(0,0),(40,0),(40,40),(0,40)])

arvoreP :: Picture
arvoreP = color green (Polygon [(0,0),(40,0),(40,40),(0,40)])



