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

-- D A D O S --


type GameState = (Int, Jogo)

jogo_teste = (0, (Jogo (Jogador (0,0)) (Mapa 5 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Relva,[Arvore, Nenhum, Nenhum, Arvore, Arvore])])))

-- D I S P L A Y  &  P L A Y --

--FUNÇÃO PLAY NO FICHEIRO MAIN.HS

-- play  .tela.  .cordefundo.  .fps.  .estadoinicial.  .funções.
-- RETIRADO  
displayMode :: Display
displayMode = InWindow "Game" (640,640) (0,0)


-- P E R S O N A G E N S --


chicken :: Picture
chicken = color white (thickCircle 4 15)

pig :: Picture
pig = color pink (thickCircle 4 15)
    where pink = makeColor 0.5 0 0.5 1 
cow :: Picture
cow = color (greyN 0.5) (thickCircle 4 15)

drawCharacter :: GameState -> Picture
drawCharacter (0, (Jogo (Jogador (x,y)) map)) = chicken
drawCharacter (1, (Jogo (Jogador (x,y)) map)) = pig
drawCharacter (2, (Jogo (Jogador (x,y)) map)) = cow

-- M A P A -- 

desenha_linha :: [Picture] -> Picture
desenha_linha (h:t) = 

translacao_aux :: (Int,Int)

desenha_linha_aux :: Terreno -> [Obstaculo] -> [Picture]
desenha_linha_aux terreno [] = []
desenha_linha_aux terreno (h:t)
    | h == (Carro || Arvore || Tronco) = (desenha_obs h):(desenha_linha_aux terreno t)
    | h == Nenhum = (desenha_nenhum terreno h):(desenha_linha_aux terreno t)



-- T E R R E N O S--

{-
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
-}

-- O B S T Á C U L O S --

desenha_obs :: Obstaculo -> Picture 
desenha_obs Tronco = troncoP
desenha_obs Carro = carroP
desenha_obs Arvore = arvoreP

troncoP :: Picture
troncoP = color brown (Polygon [(0,0),(40,0),(40,40),(0,40)])
    where brown = makeColor 0.5 0.5 0 1
carroP :: Picture
carroP = color red (Polygon [(0,0),(40,0),(40,40),(0,40)])

arvoreP :: Picture
arvoreP = color green (Polygon [(0,0),(40,0),(40,40),(0,40)])

desenha_nenhum :: Terreno -> Obstaculo -> Picture
desenha_nenhum (Rio v) Nenhum = color blue (Polygon [(0,0),(40,0),(40,40),(0,40)])
desenha_nenhum Relva Nenhum = color green (Polygon [(0,0),(40,0),(40,40),(0,40)])
desenha_nenhum (Estrada v) Nenhum = color (greyN 0.3) (Polygon [(0,0),(40,0),(40,40),(0,40)])

-- E V E N T O S --


            -- M O V I M E N T O S --

associa_dir :: Event -> Jogada
associa_dir (EventKey (SpecialKey KeyRight) Down _ _ ) = Move Direita
associa_dir (EventKey (SpecialKey KeyLeft) Down _ _ ) = Move Esquerda
associa_dir (EventKey (SpecialKey KeyUp) Down _ _ ) = Move Cima
associa_dir (EventKey (SpecialKey KeyDown) Down _ _ ) = Move Baixo
associa_dir _ = Parado


playChange :: Jogada -> GameState -> GameState
playChange (Move Direita) (n, (Jogo (Jogador (x,y)) map)) = (n, (Jogo (Jogador (x+1,y)) map))
playChange (Move Esquerda) (n, (Jogo (Jogador (x,y)) map)) = (n, (Jogo (Jogador (x-1,y)) map))
playChange (Move Cima) (n, (Jogo (Jogador (x,y)) map)) = (n, (Jogo (Jogador (x,y+1)) map))
playChange (Move Baixo) (n, (Jogo (Jogador (x,y)) map)) = (n, (Jogo (Jogador (x,y-1)) map))
playChange  Parado s = s

eventChange :: Event -> GameState -> GameState
eventChange event state =  playChange jogada state
    where jogada = associa_dir event 

timeChange :: Float -> GameState -> GameState
timeChange f (n, (Jogo (Jogador (x,y)) map)) = (n, (Jogo (Jogador (x,y-1)) map))
