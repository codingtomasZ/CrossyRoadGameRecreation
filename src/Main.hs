{- |
Module      : Tarefa6_2022li1g062
Description : Desenvolvimento da parte gráfica do jogo
Copyright   : Tomas Henrique Alves Melo <a104529@alunos.uminho.pt>
              José Diogo Azevedo Martins <a104443@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/2023.
-}



module Main where

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


main = do relva <- loadBMP "Relva.bmp"
          tronco <- loadBMP "tronco.bmp"
          rio <- loadBMP "rio.bmp"
          estrada <- loadBMP "estrada.bmp" 
          pictures <- [relva, tronco, rio, estrada]
          play 
            displayMode 
            black 1 
            (desenha pictures)
            menu_choice 
            eventChange 
            timeChange

desenha :: [Picture] -> GameState
desenha pic = (pic, estado_teste)


-- D A D O S --


type GameState  = ([Picture],(Integer, Integer, Integer, [Integer], Jogo))
--menu/game/end  ; characterchoice ; win/lose/pause ; pontos ; Game

estado_teste = (2 , 2 , 0 , [0,0] , jogo_inicial)
jogo_inicial = (Jogo (Jogador (4,1)) (Mapa 10 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Arvore]), (Estrada (2), [Nenhum, Nenhum, Carro, Carro, Carro, Nenhum, Nenhum, Nenhum, Carro, Carro]), (Estrada (-1), [Carro, Carro, Nenhum, Nenhum, Nenhum, Carro, Carro, Nenhum, Nenhum, Nenhum]), (Relva,[Arvore, Nenhum, Nenhum, Nenhum, Arvore, Arvore, Nenhum, Nenhum, Arvore, Arvore]), (Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum, Tronco]), (Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco, Nenhum, Nenhum, Tronco, Tronco, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore]), (Relva, [Arvore, Arvore, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Arvore]), (Relva,[Arvore, Arvore, Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Arvore])]))

-- D I S P L A Y  &  P L A Y --

    --FUNÇÃO PLAY NO FICHEIRO MAIN.HS
  
displayMode :: Display
displayMode = FullScreen

-- J O G O -- 

menu_choice :: GameState -> Picture
menu_choice (pic,( 0, 0 , 0 , p , jogo_inicial )) = menu_inicial -- começo do jogo [primeiro Int]
menu_choice (pic,( 1, 0 , 0 , p , jogo_inicial )) = menu_character -- escolha do personagem [prrimeiro Int], nenhum personagem selecionado [segundo Int] 
menu_choice (pic,( 2, c , 0 , p , jogo_inicial )) = start_game (pic,(2, c , 0 , p, jogo_inicial)) -- jogar jogo [segundo Int], o personagem selecionado [segundo Int], está vivo (win) [terceiro Int], o mapa a ser jogado 
menu_choice (pic,( 2, c , 1 , p , game )) = menu_lost p -- jogar jogo , personagem escolhido, estado de derrota, mapa não ativo

start_game :: GameState -> Picture
start_game (pic,(2, c , 0 , p , (Jogo (Jogador (x,y)) (Mapa l ((terreno, o):t))))) = pictures ( map_picList ++ [(translate (-750) 250 (pontos p))] ) 
          where map_picList = [(picture_mapa (Mapa l (reverse((terreno, o):t))) (pontox_inicial, pontoy_inicial)),(drawCharacter c (pontox_inicial+100*(fromIntegral x), pontoy_inicial+100*(fromIntegral y)))]
                largura = (fromIntegral l)*100 
                altura  = fromIntegral (length ((terreno, o):t))*100
                pontox_inicial = -(((fromIntegral l)*100)/2) 
                pontoy_inicial = - (((altura)/2))


            -- P O N T O S   N O   J O G O --

pontos :: [Integer] -> Picture
pontos p = pictures ( [Color points_color (Polygon [(0,0),(0,200),(200,200),(200,0)])] ++ [translate x 50(Text (show (last p)))])
    where points_color = makeColor 1 0 0.9 0.9
          x = if ((last p) > 9) == False then 65 else 27



-- M E N U S -- 
    
        -- I N I C I A L --

menu_inicial :: Picture 
menu_inicial = pictures ([(translate (-600) 50 welcome)] ++ [(translate (-200) (-300) start)])

welcome :: Picture
welcome = scale 2 1.5 (Color white (Text "WELCOME"))

start :: Picture
start = scale 0.3 0.2 (Color blue (Text "Press Space to Start") )

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


menu_lost :: [Integer] -> Picture
menu_lost p 
    | (last p < melhor_pontuacao p) = pictures ([(translate (-830) 150 lost)] ++ [translate (-150) (-100) (record p)] ++ [(translate (-260) (-300) restart)])
    | otherwise = pictures ([(translate (-830) 150 lost)] ++ [translate (-525) (-100) (new_record p)] ++ [(translate (-260) (-300) restart)])


lost :: Picture
lost =  scale 2.5 2 (Color red (Text "YOU LOST"))

restart = pictures(([translate 0 0 restart3]) ++ ([translate (-200) (-50) restart2]) ++ ([translate (-100) (-100) restart1]))

restart1 :: Picture
restart1 = scale 0.3 0.2  (Color white (Text ("Press Enter to go to Main menu")) )

restart2 :: Picture
restart2 = scale 0.3 0.2  (Color white (Text ("Press Backspace to choose a new character")) )

restart3 :: Picture
restart3 = scale 0.3 0.2  (Color white (Text ("Press Space to Restart" )) )


            -- P O N T O S -- 


                -- N O V O   R E C O R D E --
new_record :: [Integer] -> Picture
new_record p = scale 0.6 0.4 (Color yellow (Text ("Novo recorde de " ++ (show p_r) ++ " pontos!" )))
     where p_r = last p 


                -- R E C O R D E (A N T E R I O R) -- 

record :: [Integer] -> Picture
record p = scale 0.5 0.3 (Color white (Text ("Recorde: "++( show record_p))))
    where record_p = melhor_pontuacao p 

melhor_pontuacao :: [Integer] -> Integer
melhor_pontuacao [] = 0
melhor_pontuacao [p] = p 
melhor_pontuacao (h:t) 
    | h >= head t = melhor_pontuacao ([h]++(tail t))
    | h < head t = melhor_pontuacao t 


-- P E R S O N A G E N S --


drawCharacter :: Integer -> (Float,Float) -> Picture
drawCharacter 1 (x,y) = translate x y chicken
drawCharacter 2 (x,y) = translate x y pig
drawCharacter 3 (x,y) = translate x y cow


chicken :: Picture
chicken = color white (Polygon [(0,0),(100,0),(100,100),(0,100)])

pig :: Picture
pig = color pink (Polygon [(0,0),(100,0),(100,100),(0,100)])
    where pink = makeColor 1 0 0.9 0.9 
    
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
desenha_ter Relva (x,y)= (translate x y relva)

rioP :: Picture
rioP = color blue (Polygon [(0,0),(100,0),(100,100),(0,100)])

estradaP :: Picture
estradaP = color (greyN 0.3) (Polygon [(0,0),(100,0),(100,100),(0,100)])

relvaP :: Picture
relvaP = color blue (Polygon [(0,0),(100,0),(100,100),(0,100)])


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
eventChange event (pic,( 0 , 0 , 0 , p , game)) = menu_inicialChange event (pic,(0, 0 , 0 , p , game))
eventChange event (pic,( 1 , 0 , 0 , p , game)) = menu_characterChange event (pic,(1 , 0 , 0 , p , game))
eventChange event (pic,( 2 , c , 0 , [0] , game)) = wl_game event (pic,(2, c , 0 , [0] , game)) 
eventChange event (pic,( 2 , c , 0 , p , game)) = wl_game event (pic,(2, c , 0 , p , game))
eventChange event (pic,( 2 , c , 1 , p , game)) = menu_lostChange event (pic,( 2 , c , 1 , p , game)) 


            -- M O V I M E N T O S   N O   M E N U   I N I C I A L --


menu_inicialChange :: Event -> GameState -> GameState
menu_inicialChange (EventKey (SpecialKey KeySpace) Down _ _ ) (pic,(0, 0 , 0 , [] , game)) = (pic,(1, 0, 0 , [], game))
menu_inicialChange (EventKey (SpecialKey KeySpace) Down _ _ ) (pic,(0, 0 , 0 , [0] , game)) = (pic,(1, 0, 0 , [0], game))
menu_inicialChange (EventKey (SpecialKey KeySpace) Down _ _ ) (pic,(0, 0 , 0 , p , game)) = (pic,(1, 0 , 0 , p , game))
menu_inicialChange _ state = state


            -- M O V I M E N T O S   N O   M E N U   P E R S O N A G E M --


menu_characterChange :: Event -> GameState -> GameState
menu_characterChange (EventKey (Char '1') Down _ _ ) (pic,(1, 0 , 0 , p , game)) =  (pic,(2, 1 , 0 , p ,game))
menu_characterChange (EventKey (Char '2') Down _ _ ) (pic,(1, 0 , 0 , p , game)) =  (pic,(2, 2 , 0 , p , game))
menu_characterChange (EventKey (Char '3') Down _ _ ) (pic,(1, 0 , 0 , p , game)) =  (pic,(2, 3 , 0 , p , game))
menu_characterChange (EventKey (SpecialKey KeyBackspace) Down _ _ ) (pic,(1, 0 , 0 , p , game)) = (pic,(0, 0 , 0 , p , game)) 
menu_characterChange _ s = s

            -- M O V I M E N T O S   N O   M A P A --

wl_game :: Event -> GameState -> GameState 
wl_game event (pic,(2, c , 0 , p , game)) = if jogoTerminou game == True
                                then (pic,(2, c , 1 , p , game))
                                else playChange jogada (pic,(2, c , 0 , p , game))
    where jogada = associa_dir event 


playChange :: Jogada -> GameState -> GameState
playChange jogada (pic,(2 , c , 0 , [0] , Jogo (Jogador (x,y)) (Mapa l linhas))) = (pic, (2 , c , 0 , (contagem_pontos [0] jogada) , (validoMovimento(Jogo (Jogador (x,y)) ( Mapa l (atropelamento (Jogo (Jogador (x,y)) (Mapa l linhas)) jogada))) jogada)))
playChange jogada (pic,(2 , c , 0 , p , Jogo (Jogador (x,y)) (Mapa l linhas))) = (pic, (2 , c , 0 , p_n , (validoMovimento (Jogo (Jogador (x,y)) ( Mapa l (atropelamento (Jogo (Jogador (x,y)) (Mapa l linhas)) jogada))) jogada)))
    where p_n = contagem_pontos p jogada


contagem_pontos :: [Integer] -> Jogada -> [Integer]
contagem_pontos p jogada 
    | last p == 0 && jogada == Move Baixo = p 
    | length p == 1 && jogada == Move Cima = [(head p) +1] 
    | length p == 1 && jogada == Move Baixo = [(head p)-1] 
    | jogada == Move Cima = (init p) ++ [ (last p) + 1 ]
    | jogada == Move Baixo = (init p) ++ [ (last p) - 1 ]
    | otherwise = p 


associa_dir :: Event -> Jogada
associa_dir (EventKey (SpecialKey KeyRight) Down _ _ ) = Move Direita
associa_dir (EventKey (SpecialKey KeyLeft) Down _ _ ) = Move Esquerda
associa_dir (EventKey (SpecialKey KeyUp) Down _ _ ) = Move Cima
associa_dir (EventKey (SpecialKey KeyDown) Down _ _ ) = Move Baixo
associa_dir _ = Parado


            -- M O V I M E N T O S   N O   M E N U   L O S T --

menu_lostChange :: Event -> GameState -> GameState
menu_lostChange (EventKey (SpecialKey KeySpace) Down _ _ ) (pic,( 2 , c , 1 , p , game)) = (pic,( 2 , c , 0 , p++[0] , jogo_inicial))
menu_lostChange (EventKey (SpecialKey KeyBackspace) Down _ _ ) (pic,( 2 , c , 1 , p , game)) = (pic,( 1 , 0 , 0 , p++[0] , jogo_inicial))
menu_lostChange (EventKey (SpecialKey KeyEnter) Down _ _ ) (pic,( 2 , c , 1 , p , game)) = (pic,(0, 0 , 0 , p++[0] , jogo_inicial))
menu_lostChange _ s = s

            -- T E M P O --

timeChange :: Float -> GameState -> GameState
timeChange f (pic,(m, c , wl , p ,(Jogo (Jogador (x,y)) (Mapa l linhas)))) = if jogoTerminou (Jogo (Jogador (x,y)) (Mapa l linhas)) == True 
                                                                    then (pic,(2, c , 1 , p , (Jogo (Jogador (x,y)) (Mapa l linhas)))) 
                                                                    else timeChange_aux f (pic,(m, c , wl , p ,(Jogo (Jogador (x,y)) (Mapa l linhas))))  

timeChange_aux :: Float -> GameState -> GameState
timeChange_aux f (pic,(m, c , wl , p , (Jogo (Jogador (x,y)) (Mapa l linhas)))) = (pic,(m, c , wl , p , (animaJogoTempo (Jogo (Jogador (x,y)) (Mapa l linhas)))))
