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


type GameState = (Integer, Jogo)

jogo_teste = (0, (Jogo (Jogador (0,0)) (Mapa 5 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Relva,[Arvore, Nenhum, Nenhum, Arvore, Arvore])])))


-- D I S P L A Y  &  P L A Y --

    --FUNÇÃO PLAY NO FICHEIRO MAIN.HS
  
displayMode :: Display
displayMode = InWindow "Game" (1000,1000) (450,0)

-- J O G O -- 


start_game :: GameState -> Picture
start_game (c, (Jogo (Jogador (x,y)) (Mapa l ((terreno, o):t)))) = pictures ( map_picList )
    where map_picList = [(picture_mapa (Mapa l ((terreno, o):t)) (-250,-250)),(drawCharacter c (-50,-250))]


-- P E R S O N A G E N S --


drawCharacter :: Integer -> (Float,Float) -> Picture
drawCharacter 0 (x,y) = translate x y chicken
drawCharacter 1 (x,y) = translate x y pig
drawCharacter 2 (x,y) = translate x y cow


chicken :: Picture
chicken = color white (Polygon [(0,0),(100,0),(100,100),(0,100)])

pig :: Picture
pig = color pink (Polygon [(0,0),(100,0),(100,100),(0,100)])
    where pink = makeColor 0.5 0 0.5 1 
    
cow :: Picture
cow = color (greyN 0.5) (Polygon [(0,0),(100,0),(100,100),(0,100)])


-- M A P A -- 


picture_mapa :: Mapa -> (Float,Float) -> Picture
picture_mapa (Mapa l ((terreno, obs):t)) (x,y) = pictures (desenha_mapa (Mapa l ((terreno, obs):t)) (x,y))

desenha_mapa :: Mapa -> (Float,Float) -> [Picture]
desenha_mapa (Mapa l ((terreno, (h:t')):t)) (x,y) = (desenha_mapa_terrenos (Mapa l ((terreno, (h:t')):t)) (x,y)) ++ (desenha_mapa_obstaculos (Mapa l ((terreno, (h:t')):t)) (x,y))

desenha_mapa_obstaculos :: Mapa -> (Float,Float) -> [Picture]
desenha_mapa_obstaculos (Mapa l []) (x,y) = []
desenha_mapa_obstaculos (Mapa l ((terreno, (h:t')):t)) (x,y) = ((picture_linha_obstaculos (h:t') (x,y)):(desenha_mapa_obstaculos (Mapa l t ) (x,y+100)))

desenha_mapa_terrenos :: Mapa -> (Float,Float) -> [Picture]
desenha_mapa_terrenos (Mapa l []) (x,y) = []
desenha_mapa_terrenos (Mapa l ((terreno, obs):t)) (x,y) = ((picture_linha_terrenos terreno (x,y) l):(desenha_mapa_terrenos (Mapa l t ) (x,y+100)))
 

        -- L I N H A  T E R R E N O  --


picture_linha_terrenos :: Terreno -> (Float,Float) -> Int -> Picture
picture_linha_terrenos terreno (x,y) l = pictures (desenha_linha_terrenos terreno (x,y) l)


desenha_linha_terrenos :: Terreno -> (Float,Float) -> Int -> [Picture]
desenha_linha_terrenos _ _ 0 = []
desenha_linha_terrenos terreno (x,y) l = (desenha_ter terreno (x,y)):(desenha_linha_terrenos terreno ((x+100),y) (l-1) )


            -- T E R R E N O S--


desenha_ter :: Terreno -> (Float,Float) -> Picture 
desenha_ter (Rio v) (x,y) = (translate x y rioP)
desenha_ter (Estrada v) (x,y)= (translate x y estradaP)
desenha_ter Relva (x,y)= (translate x y relvaP)

rioP :: Picture
rioP = color blue (Polygon [(0,0),(100,0),(100,100),(0,100)])

estradaP :: Picture
estradaP = color (greyN 0.3) (Polygon [(0,0),(100,0),(100,100),(0,100)])

relvaP :: Picture
relvaP = color green (Polygon [(0,0),(100,0),(100,100),(0,100)])


        -- L I N H A  O B S T A C U L O -- 


picture_linha_obstaculos :: [Obstaculo] -> (Float,Float) -> Picture
picture_linha_obstaculos (h:t) (x,y) = pictures (desenha_linha_obstaculos (h:t) (x,y))


desenha_linha_obstaculos :: [Obstaculo] -> (Float,Float) -> [Picture]
desenha_linha_obstaculos [] _ = []
desenha_linha_obstaculos (h:t) (x,y) = (desenha_obs h (x,y)):(desenha_linha_obstaculos t ((x+100),y))


            -- O B S T Á C U L O S --


desenha_obs :: Obstaculo -> (Float,Float) -> Picture 
desenha_obs Tronco (x,y) = (translate x y troncoP)
desenha_obs Carro (x,y) = (translate x y carroP)
desenha_obs Arvore (x,y) = (translate x y arvoreP)
desenha_obs Nenhum (x,y) = (translate x y nenhumP)

troncoP :: Picture
troncoP = color brown (Polygon [(0,0),(100,0),(100,100),(0,100)])
    where brown = makeColor 0.5 0.5 0 1

carroP :: Picture
carroP = color red (Polygon [(0,0),(100,0),(100,100),(0,100)])

arvoreP :: Picture
arvoreP = color darkgreen (Polygon [(0,0),(100,0),(100,100),(0,100)])
    where darkgreen = makeColor 0 0.5 0.4 1

nenhumP :: Picture
nenhumP = Blank 


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
