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
          carro <- loadBMP "carro.bmp"
          arvore <- loadBMP "arvore.bmp"
          chicken <- loadBMP "chicken.bmp"
          pig <- loadBMP "pig.bmp"
          cow <- loadBMP "cow.bmp"
          play 
            displayMode 
            black 
            1 
            (desenha [relva, tronco, rio, estrada, carro, arvore, chicken, pig, cow])
            menu_choice 
            eventChange 
            timeChange

desenha :: [Picture] -> GameState
desenha pic = (pic, estado_teste)


-- D A D O S --


type GameState  = ([Picture],(Integer, Float, Integer, [Integer], Jogo))
--menu/game/end  ; characterchoice ; win/lose/pause ; pontos ; Game

estado_teste = (2 , 1, 3 , [0] , jogo_inicial)
jogo_inicial = (Jogo (Jogador (4,1)) (Mapa 10 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Arvore]), (Estrada (2), [Nenhum, Nenhum, Carro, Carro, Carro, Nenhum, Nenhum, Nenhum, Carro, Carro]), (Estrada (-1), [Carro, Carro, Nenhum, Nenhum, Nenhum, Carro, Carro, Nenhum, Nenhum, Nenhum]), (Relva,[Arvore, Nenhum, Nenhum, Nenhum, Arvore, Arvore, Nenhum, Nenhum, Arvore, Arvore]), (Rio (-1),[Tronco, Nenhum, Nenhum, Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum, Tronco]), (Rio (2), [Tronco, Tronco, Nenhum, Tronco, Tronco, Nenhum, Nenhum, Tronco, Tronco, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore]), (Relva, [Arvore, Arvore, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Arvore]), (Relva,[Arvore, Arvore, Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore, Arvore])]))

-- D I S P L A Y  &  P L A Y --

    --FUNÇÃO PLAY NO FICHEIRO MAIN.HS
  
displayMode :: Display
displayMode = FullScreen

-- J O G O -- 

menu_choice :: GameState -> Picture
menu_choice (pic,( 0, 0 , 0 , p , jogo_inicial )) = menu_inicial -- começo do jogo [primeiro Int]
menu_choice ([relva, tronco, rio, estrada, carro, arvore, chicken, pig, cow],( 1, 0 , 0 , p , jogo_inicial )) = menu_character [chicken, pig, cow] 0 -- escolha do personagem [prrimeiro Int], nenhum personagem selecionado [segundo Int] 
menu_choice ([relva, tronco, rio, estrada, carro, arvore, chicken, pig, cow],( 1, 0.1 , 0 , p , jogo_inicial )) = menu_character [chicken, pig, cow] 0.1 
menu_choice ([relva, tronco, rio, estrada, carro, arvore, chicken, pig, cow],( 1, 0.2 , 0 , p , jogo_inicial )) = menu_character [chicken, pig, cow] 0.2 
menu_choice ([relva, tronco, rio, estrada, carro, arvore, chicken, pig, cow],( 1, 0.3 , 0 , p , jogo_inicial )) = menu_character [chicken, pig, cow] 0.3 
menu_choice (pic,( 2, c , 0 , p , jogo_inicial )) = start_game (pic,(2, c , 0 , p, jogo_inicial)) -- jogar jogo [segundo Int], o personagem selecionado [segundo Int], está vivo (win) [terceiro Int], o mapa a ser jogado 
menu_choice (pic,( 2, c , 3 , p , game )) = pictures ( [ start_game (pic,( 2, c , 0 , p , game )) ] ++ [menu_pause] ++ [(translate (-750) 250 (pontos p))]) 
menu_choice (pic,( 2, c , 1 , p , game )) = menu_lost p -- jogar jogo , personagem escolhido, estado de derrota, mapa não ativo


start_game :: GameState -> Picture
start_game ([relva, tronco, rio, estrada, carro, arvore, chicken, pig, cow],(2, c , 0 , p , (Jogo (Jogador (x,y)) (Mapa l ((terreno, o):t))))) = pictures ( map_picList ++ [(translate (-750) 250 (pontos p))] ) 
          where map_picList = [(picture_mapa [relva, tronco, rio, estrada, carro, arvore] (Mapa l (reverse((terreno, o):t))) (pontox_inicial, pontoy_inicial)),(drawCharacter [chicken, pig, cow] c (pontox_inicial+100*(fromIntegral x), pontoy_inicial+100*(fromIntegral y)))]
                largura = (fromIntegral l)*100 
                altura  = fromIntegral (length ((terreno, o):t))*100
                pontox_inicial = -(((fromIntegral l)*100)/2) 
                pontoy_inicial = - (((altura)/2))


            -- P O N T O S   N O   J O G O --

pontos :: [Integer] -> Picture
pontos p = pictures ( [Color blue (Polygon [(0,0),(0,200),(200,200),(200,0)])] ++ [translate 10 10 (Color azure (Polygon [(0,0),(0,180),(180,180),(180,0)]))] ++ [translate x 55 (Text (show (last p)))] ++ [translate 68 20 (scale 0.2 0.1 (Text "points") )])
          where x = if ((last p) > 9) == False then 65 else 27



-- M E N U S -- 
    
        -- I N I C I A L --

menu_inicial :: Picture 
menu_inicial = pictures ([(translate (-600) 50 welcome)] ++ [(translate (-200) (-300) start)])

welcome :: Picture
welcome = scale 2 1.5 (Color white (Text "WELCOME"))

start :: Picture
start = scale 0.3 0.2 (Color blue (Text "Press Space to Start") )

        --  C H A R A C T E R -- 

menu_character ::  [Picture] -> Float -> Picture
menu_character [chicken, pig, cow] 0 = pictures   ([translate (-900) (300) char_txtL] ++ [translate (-550) (200) char_txtS] ++ [translate (-700) (-250) (char_chicken [chicken, pig, cow])]     ++ [translate (-100) (-250) (char_pig [chicken, pig, cow])]     ++ [translate 500 (-250) (char_cow [chicken, pig, cow])])
menu_character [chicken, pig, cow] 0.1 = pictures ([translate (-900) (300) char_txtL] ++ [translate (-550) (200) char_txtS] ++ [translate (-700) (-250) (sel_char_chicken [chicken, pig, cow])] ++ [translate (-100) (-250) (char_pig [chicken, pig, cow])]     ++ [translate 500 (-250) (char_cow [chicken, pig, cow])])
menu_character [chicken, pig, cow] 0.2 = pictures ([translate (-900) (300) char_txtL] ++ [translate (-550) (200) char_txtS] ++ [translate (-700) (-250) (char_chicken [chicken, pig, cow])]     ++ [translate (-100) (-250) (sel_char_pig [chicken, pig, cow])] ++ [translate 500 (-250) (char_cow [chicken, pig, cow])])
menu_character [chicken, pig, cow] 0.3 = pictures ([translate (-900) (300) char_txtL] ++ [translate (-550) (200) char_txtS] ++ [translate (-700) (-250) (char_chicken [chicken, pig, cow])]     ++ [translate (-100) (-250) (char_pig [chicken, pig, cow])]     ++ [translate 500 (-250) (sel_char_cow [chicken, pig, cow])])


char_chicken ::  [Picture] -> Picture
char_chicken [chicken, pig, cow] = pictures ([scale 2 2 (chickenP [chicken, pig, cow])] ++ [translate 100 100 char_circle])
char_pig ::  [Picture] -> Picture
char_pig [chicken, pig, cow] =  pictures ([scale 2 2 (pigP [chicken, pig, cow])] ++ [translate 100 100 char_circle])
char_cow ::  [Picture] -> Picture
char_cow [chicken, pig, cow] = pictures ([scale 2 2 (cowP [chicken, pig, cow])] ++ [translate 100 100 char_circle])

sel_char_chicken ::  [Picture] -> Picture
sel_char_chicken [chicken, pig, cow] = pictures ([scale 2 2 (chickenP [chicken, pig, cow])] ++ [translate 100 100 char_circleSelected])
sel_char_pig ::  [Picture] -> Picture
sel_char_pig [chicken, pig, cow] =  pictures ([scale 2 2 (pigP [chicken, pig, cow])] ++ [translate 100 100 char_circleSelected])
sel_char_cow ::  [Picture] -> Picture
sel_char_cow [chicken, pig, cow] = pictures ([scale 2 2 (cowP [chicken, pig, cow])] ++ [translate 100 100 char_circleSelected])


char_circle :: Picture
char_circle = color blue (ThickCircle 200 30)
char_circleSelected :: Picture
char_circleSelected = color yellow (ThickCircle 200 30)

char_txtL :: Picture
char_txtL = color yellow(scale 1 0.8 (Text ("CHOOSE YOUR CHARACTER")))

char_txtS :: Picture
char_txtS = color white(scale 0.4 0.3 (Text ("[<- ->] to change and [space] to select")))

        -- P A U S E --

menu_pause :: Picture
menu_pause = pictures ([ color (withAlpha 0.90 white) (Polygon [(-1000,-2000),(1000,-2000),(1000,2000),(-1000,2000)]) ] ++ [translate (-540) (-50) (scale 3 2.8 (Text "Pause"))]  ++ [translate (-290) (-150) (scale 0.5 0.3 (Text "Space to Continue"))])

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
restart2 = scale 0.3 0.2  (Color white (Text ("Press Right Button to choose a new character")) )

restart3 :: Picture
restart3 = scale 0.3 0.2  (Color white (Text ("Press Space to Restart" )) )


            -- P O N T O S -- 


                -- N O V O   R E C O R D E --
new_record :: [Integer] -> Picture
new_record p = scale 0.6 0.4 (Color yellow (Text ("New record of " ++ (show p_r) ++ " points!" )))
     where p_r = last p 


                -- R E C O R D E (A N T E R I O R) -- 

record :: [Integer] -> Picture
record p = scale 0.5 0.3 (Color white (Text ("Record: "++( show record_p))))
    where record_p = melhor_pontuacao p 

melhor_pontuacao :: [Integer] -> Integer
melhor_pontuacao [] = 0
melhor_pontuacao [p] = p 
melhor_pontuacao (h:t) 
    | h >= head t = melhor_pontuacao ([h]++(tail t))
    | h < head t = melhor_pontuacao t 


-- P E R S O N A G E N S --


drawCharacter :: [Picture] -> Float -> (Float,Float) -> Picture
drawCharacter [chicken, pig, cow] 1 (x,y) = translate x y (chickenP [chicken, pig, cow])
drawCharacter [chicken, pig, cow] 2 (x,y) = translate x y (pigP [chicken, pig, cow])
drawCharacter [chicken, pig, cow] 3 (x,y) = translate x y (cowP [chicken, pig, cow])


chickenP :: [Picture] -> Picture
chickenP [chicken, pig, cow] = translate 50 50 (scale 0.28 0.28 chicken)

pigP :: [Picture] -> Picture
pigP [chicken, pig, cow] = translate 50 50 (scale 0.3 0.3 pig)
    
cowP :: [Picture] -> Picture
cowP [chicken, pig, cow] = translate 50 50 (scale 0.22 0.22 cow)



-- M A P A -- 


picture_mapa :: [Picture] -> Mapa -> (Float,Float) -> Picture
picture_mapa [relva, tronco, rio, estrada, carro, arvore] (Mapa l ((terreno, obs):t)) (x,y) = pictures (desenha_mapa [relva, tronco, rio, estrada, carro, arvore] (Mapa l ((terreno, obs):t)) (x,y))

desenha_mapa :: [Picture] -> Mapa -> (Float,Float) -> [Picture]
desenha_mapa [relva, tronco, rio, estrada, carro, arvore] (Mapa l ((terreno, (h:t')):t)) (x,y) = (desenha_mapa_terrenos [relva, rio, estrada] (Mapa l ((terreno, (h:t')):t)) (x,y)) ++ (desenha_mapa_obstaculos [tronco, carro, arvore] (Mapa l ((terreno, (h:t')):t)) (x,y))

desenha_mapa_obstaculos :: [Picture] -> Mapa -> (Float,Float) -> [Picture]
desenha_mapa_obstaculos [tronco, carro, arvore] (Mapa l []) (x,y) = []
desenha_mapa_obstaculos [tronco, carro, arvore] (Mapa l ((terreno, (h:t')):t)) (x,y) = ((picture_linha_obstaculos [tronco, carro, arvore] v (h:t') (x,y)):(desenha_mapa_obstaculos [tronco, carro, arvore] (Mapa l t ) (x,y+100)))
        where v = velocidade_da_linha (terreno, (h:t'))

desenha_mapa_terrenos :: [Picture] -> Mapa -> (Float,Float) -> [Picture]
desenha_mapa_terrenos [relva, rio, estrada] (Mapa l []) (x,y) = []
desenha_mapa_terrenos [relva, rio, estrada] (Mapa l ((terreno, obs):t)) (x,y) = ((picture_linha_terrenos [relva, rio, estrada] terreno (x,y) l):(desenha_mapa_terrenos [relva, rio, estrada] (Mapa l t) (x,y+100)))
 

        -- L I N H A  T E R R E N O  --


picture_linha_terrenos :: [Picture] -> Terreno -> (Float,Float) -> Int -> Picture
picture_linha_terrenos [relva, rio, estrada] terreno (x,y) l = pictures (desenha_linha_terrenos [relva, rio, estrada] terreno (x,y) l)


desenha_linha_terrenos :: [Picture] -> Terreno -> (Float,Float) -> Int -> [Picture]
desenha_linha_terrenos [relva, rio, estrada] _ _ 0 = []
desenha_linha_terrenos [relva, rio, estrada] terreno (x,y) l = (desenha_ter [relva, rio, estrada] terreno (x,y)):(desenha_linha_terrenos [relva, rio, estrada] terreno ((x+100),y) (l-1) )


            -- T E R R E N O S--


desenha_ter :: [Picture] -> Terreno -> (Float,Float) -> Picture 
desenha_ter [relva, rio, estrada] (Rio v) (x,y) = (translate x y (rioP [relva, rio, estrada]))
desenha_ter [relva, rio, estrada] (Estrada v) (x,y)= (translate x y (estradaP [relva, rio, estrada]))
desenha_ter [relva, rio, estrada] Relva (x,y)= (translate x y (relvaP [relva, rio, estrada]))

rioP :: [Picture] -> Picture
rioP [relva, rio, estrada] = translate 50 50 (scale 0.555555555 0.555555555 rio)

estradaP :: [Picture] -> Picture
estradaP [relva, rio, estrada] = translate 50 50 (rotate 90 (scale 0.555555555 0.555555555 estrada))

relvaP :: [Picture] -> Picture
relvaP [relva, rio, estrada] = translate 50 50 (scale 0.555555555 0.555555555 relva)


        -- L I N H A  O B S T A C U L O -- 


picture_linha_obstaculos :: [Picture] -> Velocidade -> [Obstaculo] -> (Float,Float) -> Picture
picture_linha_obstaculos [tronco, carro, arvore] v (h:t) (x,y) = pictures (desenha_linha_obstaculos [tronco, carro, arvore] v (h:t) (x,y))


desenha_linha_obstaculos :: [Picture] -> Velocidade -> [Obstaculo] -> (Float,Float) -> [Picture]
desenha_linha_obstaculos [tronco, carro, arvore] _ [] _ = []
desenha_linha_obstaculos [tronco, carro, arvore] v (h:t) (x,y) = (desenha_obs [tronco, carro, arvore] v h (x,y)):(desenha_linha_obstaculos [tronco, carro, arvore] v t ((x+100),y))


            -- O B S T Á C U L O S --


desenha_obs :: [Picture] -> Velocidade -> Obstaculo -> (Float,Float) -> Picture 
desenha_obs [tronco, carro, arvore] v Tronco (x,y) = (translate x y (troncoP [tronco, carro, arvore]))
desenha_obs [tronco, carro, arvore] v Carro (x,y) = (translate x y (carroP [tronco, carro, arvore] v))
desenha_obs [tronco, carro, arvore] v Arvore (x,y) = (translate (x+50) (y+50) (arvoreP [tronco, carro, arvore]))
desenha_obs [tronco, carro, arvore] v Nenhum (x,y) = (translate x y nenhumP)

troncoP :: [Picture] -> Picture
troncoP [tronco, carro, arvore] = translate 50 50 (rotate 90 (scale 0.40 0.555555555 tronco))

carroP :: [Picture] -> Velocidade -> Picture
carroP [tronco, carro, arvore] v = translate 50 50 (rotate r (scale 0.20 0.20 carro))
        where r = if v > 0 then 270 else 90

arvoreP :: [Picture] -> Picture
arvoreP [tronco, carro, arvore] = (scale 0.20 0.20 arvore)

nenhumP :: Picture
nenhumP = Blank 


-- E V E N T O S --


eventChange :: Event -> GameState -> GameState
eventChange event (pic,( 0 , 0 , 0 , p , game)) = menu_inicialChange event (pic,(0, 0 , 0 , p , game))
eventChange event (pic,( 1 , c , 0 , p , game)) = menu_characterChange event (pic,(1 , c , 0 , p , game))
eventChange event (pic,( 2 , c , 0 , [0] , game)) = wl_game event (pic,(2, c , 0 , [0] , game)) 
eventChange event (pic,( 2 , c , 0 , p , game)) = wl_game event (pic,(2, c , 0 , p , game))
eventChange event (pic,( 2 , c , 1 , p , game)) = menu_lostChange event (pic,( 2 , c , 1 , p , game)) 
eventChange event (pic,( 2 , c , 3 , p , game)) = menu_pauseChange event (pic,( 2 , c , 3 , p , game))

            -- M O V I M E N T O S   N O   M E N U   I N I C I A L --


menu_inicialChange :: Event -> GameState -> GameState
menu_inicialChange (EventKey (SpecialKey KeySpace) Down _ _ ) (pic,(0, 0 , 0 , [] , game)) = (pic,(1, 0, 0 , [], game))
menu_inicialChange (EventKey (SpecialKey KeySpace) Down _ _ ) (pic,(0, 0 , 0 , [0] , game)) = (pic,(1, 0, 0 , [0], game))
menu_inicialChange (EventKey (SpecialKey KeySpace) Down _ _ ) (pic,(0, 0 , 0 , p , game)) = (pic,(1, 0 , 0 , p , game))
menu_inicialChange _ state = state


            -- M O V I M E N T O S   N O   M E N U   P E R S O N A G E M --



menu_characterChange :: Event -> GameState -> GameState
menu_characterChange (EventKey (SpecialKey KeyRight) Down _ _ ) (pic, (1, 0 , 0 , p , game)) = (pic,(1, (0.1) , 0 , p ,game))
menu_characterChange (EventKey (SpecialKey KeyLeft) Down _ _ ) (pic, (1 , 0 , 0 , p , game)) = (pic,(1, (0.3) , 0 , p ,game))
menu_characterChange (EventKey (SpecialKey KeyRight) Down _ _ ) (pic, (1 , 0.3 , 0 , p , game)) = (pic,(1, (0.1) , 0 , p ,game))
menu_characterChange (EventKey (SpecialKey KeyLeft) Down _ _ ) (pic, (1 , 0.1 , 0 , p , game)) = (pic,(1, (0.3) , 0 , p ,game))
menu_characterChange (EventKey (SpecialKey KeyRight) Down _ _ ) (pic, (1, c , 0 , p , game)) = (pic,(1, (c+0.1) , 0 , p ,game))
menu_characterChange (EventKey (SpecialKey KeyLeft) Down _ _ ) (pic, (1 , 0.3 , 0 , p , game)) = (pic,(1, (0.2) , 0 , p ,game))
menu_characterChange (EventKey (SpecialKey KeyLeft) Down _ _ ) (pic, (1 , c , 0 , p , game)) = (pic,(1, (c-0.1) , 0 , p ,game))
menu_characterChange (EventKey (SpecialKey KeySpace) Down _ _ ) (pic, (1 , 0.1 , 0 , p , game)) = (pic, (2, 1 , 0 , p ,game))
menu_characterChange (EventKey (SpecialKey KeySpace) Down _ _ ) (pic, (1 , 0.2 , 0 , p , game)) = (pic, (2, 2 , 0 , p ,game))
menu_characterChange (EventKey (SpecialKey KeySpace) Down _ _ ) (pic, (1 , 0.3 , 0 , p , game)) = (pic, (2, 3 , 0 , p ,game))
menu_characterChange (EventKey (MouseButton RightButton) Down _ _ ) (pic,(1 , _ , 0 , p , game)) = (pic,(0, 0 , 0 , p , game)) 
menu_characterChange _ s = s


            -- M O V I M E N T O S   N O   M A P A --

wl_game :: Event -> GameState -> GameState 
wl_game (EventKey (SpecialKey KeySpace) Down _ _ ) (pic,(2, c , 0 , p , game)) = (pic,(2, c , 3 , p , game))
wl_game event (pic,(2, c , 0 , p , game)) = if jogoTerminou game == True
                                then (pic,(2, c , 1 , p , game))
                                else playChange jogada (pic,(2, c , 0 , p , game))
    where jogada = associa_dir event 


playChange :: Jogada -> GameState -> GameState
playChange jogada (pic,(2 , c , 0 , [0] , Jogo (Jogador (x,y)) (Mapa l linhas))) = (pic, (2 , c , 0 , (contagem_pontos [0] jogada) , (validoMovimento (Jogo (Jogador (x,y)) ( Mapa l linhas)) jogada)))
playChange jogada (pic,(2 , c , 0 , p , Jogo (Jogador (x,y)) (Mapa l linhas))) =   (pic, (2 , c , 0 ,            p_n               , (validoMovimento (Jogo (Jogador (x,y)) ( Mapa l linhas)) jogada)))
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
menu_lostChange (EventKey (MouseButton RightButton) Down _ _ ) (pic,( 2 , c , 1 , p , game)) = (pic,( 1 , 0 , 0 , p++[0] , jogo_inicial))
menu_lostChange (EventKey (SpecialKey KeyEnter) Down _ _ ) (pic,( 2 , c , 1 , p , game)) = (pic,(0, 0 , 0 , p++[0] , jogo_inicial))
menu_lostChange _ s = s

            -- M O V I M E N T O S   N O   M E N U   P A U S E --

menu_pauseChange :: Event -> GameState -> GameState
menu_pauseChange (EventKey (SpecialKey KeySpace) Down _ _ ) (pic,( 2 , c , 3 , p , game)) = (pic,( 2 , c , 0 , p , game))
menu_pauseChange (EventKey (MouseButton RightButton) Down _ _ ) (pic,( 2 , c , 3 , p , game)) = (pic,( 0 , 0 , 0 , p , game))
menu_pauseChange _ s = s


            -- T E M P O --


timeChange :: Float -> GameState -> GameState
timeChange f (pic,(2, c , 0 , p ,(Jogo (Jogador (x,y)) (Mapa l linhas)))) = if jogoTerminou (Jogo (Jogador (x,y)) (Mapa l linhas)) == True 
                                                                    then (pic,(2, c , 1 , p , (Jogo (Jogador (x,y)) (Mapa l linhas)))) 
                                                                    else timeChange_aux f (pic,(2, c , 0 , p ,(Jogo (Jogador (x,y)) (Mapa l linhas))))
timeChange f (pic,(m, c , wl , p ,(Jogo (Jogador (x,y)) (Mapa l linhas)))) = (pic,(m, c , wl , p , (Jogo (Jogador (x,y)) (Mapa l linhas)))) 



timeChange_aux :: Float -> GameState -> GameState
timeChange_aux f (pic,(m, c , wl , p , (Jogo (Jogador (x,y)) (Mapa l linhas)))) = (pic,(m, c , wl ,  p , deslizaJogo (animated_map)))
            where animated_map = animaJogoTempo (Jogo (Jogador (x,y)) (Mapa l linhas))