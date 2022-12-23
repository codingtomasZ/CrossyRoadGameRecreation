{- |
Module      : Tarefa6_2022li1g062
Description : Desenvolvimento da parte gráfica do jogo
Copyright   : Tomas Henrique Alves Melo <a104529@alunos.uminho.pt>
              José Diogo Azevedo Martins <a104443@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/2023.
-}

module Tarefa6_2022li1g062 where

import LI12223
import Tarefa1_2022li1g062
import Tarefa2_2022li1g062
import Tarefa3_2022li1g062
import Tarefa4_2022li1g062
import Tarefa5_2022li1g062
import Test.HUnit
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe

-- D A D O S --


type GameState = (Integer, Integer, Integer, Jogo)
--menu/game/end  ; characterchoice ; win/lose ; Game

estado_teste = (0, 0, 0, jogo_inicial)
jogo_inicial = (Jogo (Jogador (4,0)) (Mapa 10 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Arvore]), (Estrada (2), [Nenhum, Nenhum, Carro, Carro, Carro, Nenhum, Nenhum, Nenhum, Carro, Carro]), (Estrada (-1), [Carro, Carro, Nenhum, Nenhum, Nenhum, Carro, Carro, Nenhum, Nenhum, Nenhum]), (Relva,[Arvore, Nenhum, Nenhum, Nenhum, Arvore, Arvore, Nenhum, Nenhum, Arvore, Arvore]), (Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum, Tronco]), (Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco, Nenhum, Nenhum, Tronco, Tronco, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore]), (Relva, [Arvore, Arvore, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Arvore]), (Relva,[Arvore, Arvore, Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Arvore])]))

-- D I S P L A Y  &  P L A Y --

    --FUNÇÃO PLAY NO FICHEIRO MAIN.HS
  
displayMode :: Display
displayMode = FullScreen

-- J O G O -- 

menu_choice :: GameState -> Picture
menu_choice ( 0, 0 , 0 , jogo_inicial ) = menu_inicial -- começo do jogo [primeiro Int]
menu_choice ( 1, 0 , 0 , jogo_inicial ) = menu_character -- escolha do personagem [prrimeiro Int], nenhum personagem selecionado [segundo Int] 
menu_choice ( 2, c , 0 , jogo_inicial ) = start_game (2, c , 0 , jogo_inicial) -- jogar jogo [segundo Int], o personagem selecionado [segundo Int], está vivo (win) [terceiro Int], o mapa a ser jogado 
menu_choice ( 2, c , 1 , game ) = menu_lost -- jogar jogo , personagem escolhido, estado de derrota, mapa não ativo

start_game :: GameState -> Picture
start_game (2, c , 0 , (Jogo (Jogador (x,y)) (Mapa l ((terreno, o):t)))) = pictures ( map_picList )
    where map_picList = [(picture_mapa (Mapa l (reverse((terreno, o):t))) (pontox_inicial, pontoy_inicial)),(drawCharacter c (pontox_inicial+100*(fromIntegral x), pontoy_inicial+100*(fromIntegral y)))]
          largura = (fromIntegral l)*100 
          altura  = fromIntegral (length ((terreno, o):t))*100
          pontox_inicial = -(((fromIntegral l)*100)/2)
          pontoy_inicial = - (((altura)/2))

-- M E N U S -- 
    
        -- I N I C I A L --

menu_inicial :: Picture 
menu_inicial = pictures ([(translate (-600) 50 welcome)] ++ [(translate (-200) (-300) start)])

welcome :: Picture
welcome = scale 2 1.5 (Color white (Text "WELCOME"))

start :: Picture
start = scale 0.3 0.2 (Color blue (Text "Press Enter to Start") )

        --  C H A R A C T E R -- 

menu_character :: Picture
menu_character = pictures ([translate (-700) (-100) char_chicken] ++ [translate (-100) (-100) char_pig] ++ [translate 500 (-100) char_cow])

char_chicken :: Picture
char_chicken = scale 2 2 chicken

char_pig :: Picture
char_pig = scale 2 2 pig

char_cow :: Picture
char_cow = scale 2 2 cow


        -- L O S T --

menu_lost :: Picture
menu_lost = pictures ([(translate (-650) 50 lost)] ++ [(translate (-1000) (-300) restart)])

lost :: Picture
lost =  scale 2 1.5 (Color red (Text "YOU LOST"))

restart :: Picture
restart = scale 0.3 0.2  (Color white (Text ("Press Enter to go to Main menu" ++ "\n" ++ "Press Backspace to choose a new character" ++ "\n" ++ "Press Space to Restart" )) )


-- P E R S O N A G E N S --


drawCharacter :: Integer -> (Float,Float) -> Picture
drawCharacter 1 (x,y) = translate x y chicken
drawCharacter 2 (x,y) = translate x y pig
drawCharacter 3 (x,y) = translate x y cow


chicken :: Picture
chicken = color white (Polygon [(0,0),(100,0),(100,100),(0,100)])

pig :: Picture
pig = color pink (Polygon [(0,0),(100,0),(100,100),(0,100)])
    where pink = makeColor 0.5 0 0.3 1 
    
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


eventChange :: Event -> GameState -> GameState
eventChange event (0, 0 , 0 , game) = menu_inicialChange event (0, 0 , 0 , game)
eventChange event (1 , 0 , 0 , game) = menu_characterChange event (1 , 0 , 0 , game)
eventChange event ( 2 , c , 0 , game) = wl_game event (2, c , 0 , game) 
eventChange event ( 2 , c , 1 , game) = menu_lostChange event ( 2 , c , 1 , game) 


            -- M O V I M E N T O S   N O   M E N U   I N I C I A L --

menu_inicialChange :: Event -> GameState -> GameState
menu_inicialChange (EventKey (SpecialKey KeyEnter) Down _ _ ) (0, 0 , 0 , game) = (1, 0 , 0 , game)
menu_inicialChange _ state = state

            -- M O V I M E N T O S   N O   M E N U   P E R S O N A G E M --

menu_characterChange :: Event -> GameState -> GameState
menu_characterChange (EventKey (Char '1') Down _ _ ) (1, 0 , 0 , game) =  (2, 1 , 0 , game)
menu_characterChange (EventKey (Char '2') Down _ _ ) (1, 0 , 0 , game) =  (2, 2 , 0 , game)
menu_characterChange (EventKey (Char '3') Down _ _ ) (1, 0 , 0 , game) =  (2, 3 , 0 , game)
menu_characterChange (EventKey (SpecialKey KeyBackspace) Down _ _ ) (1, 0 , 0 , game) = (0, 0 , 0 , game) 
menu_characterChange _ s = s

            -- M O V I M E N T O S   N O   M A P A --

wl_game :: Event -> GameState -> GameState 
wl_game event (2, c , 0 , game) = if jogoTerminou game == True
                                then (2, c , 1 , game)
                                else playChange jogada (2, c , 0 , game)
    where jogada = associa_dir event 


playChange :: Jogada -> GameState -> GameState
playChange (Move Direita) (2 , c , 0 , Jogo (Jogador (x,y)) map) = (2 , c , 0 , Jogo (Jogador (x+1,y)) map)
playChange (Move Esquerda) (2 , c , 0 , Jogo (Jogador (x,y)) map) = (2 , c , 0 , Jogo (Jogador (x-1,y)) map)
playChange (Move Cima) (2 , c , 0 , Jogo (Jogador (x,y)) map) = (2 , c , 0 , Jogo (Jogador (x,y+1)) map)
playChange (Move Baixo) (2 , c , 0 , Jogo (Jogador (x,y)) map) = (2 , c , 0 , Jogo (Jogador (x,y-1)) map)
playChange  Parado s = s

associa_dir :: Event -> Jogada
associa_dir (EventKey (SpecialKey KeyRight) Down _ _ ) = Move Direita
associa_dir (EventKey (SpecialKey KeyLeft) Down _ _ ) = Move Esquerda
associa_dir (EventKey (SpecialKey KeyUp) Down _ _ ) = Move Cima
associa_dir (EventKey (SpecialKey KeyDown) Down _ _ ) = Move Baixo
associa_dir _ = Parado

            -- M O V I M E N T O S   N O   M E N U   L O S T --

menu_lostChange :: Event -> GameState -> GameState
menu_lostChange (EventKey (SpecialKey KeySpace) Down _ _ ) ( 2 , c , 1 , game) = ( 2 , c , 0 , jogo_inicial)
menu_lostChange (EventKey (SpecialKey KeyBackspace) Down _ _ ) ( 2 , c , 1 , game) = ( 1 , 0 , 0 , jogo_inicial)
menu_lostChange (EventKey (SpecialKey KeyEnter) Down _ _ ) ( 2 , c , 1 , game) = (0, 0, 0, jogo_inicial)
menu_lostChange _ s = s

            -- T E M P O --

timeChange :: Float -> GameState -> GameState
timeChange f (m, c , wl , (Jogo (Jogador (x,y)) (Mapa l linhas))) = (m, c , wl ,((Jogo (Jogador (x,y)) (Mapa l (moveObs l linhas)) )))
