module Tarefa5_2022li1g062_Spec where

import LI12223
import Tarefa5_2022li1g062
import Test.HUnit



testsT1 :: Test
testsT1 = TestLabel "Teste coordenadas" $ test ["Mesmo mapa. Soma das coordenadas igual" ~: Jogo (Jogador (2,0)) (Mapa 5 [(Estrada 2,[Carro,Carro,Nenhum,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Arvore]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Tronco])])~=? deslizaJogo (Jogo (Jogador (2,0))  (Mapa 5 [(Relva,[Arvore, Nenhum, Nenhum, Nenhum, Arvore]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Relva,[Arvore, Nenhum, Nenhum, Arvore, Arvore])]))]

testsT2 :: Test 
testsT2 = TestLabel "Teste coordenadas" $ test ["Mesmo mapa. Soma das coordenadas igual" ~: Jogo (Jogador (1,1)) (Mapa 5 [(Estrada 2,[Carro,Carro,Nenhum,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Tronco])]) ~=?  deslizaJogo (Jogo (Jogador (1,1))  (Mapa 5 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Relva,[Arvore, Nenhum, Nenhum, Arvore, Arvore])]))]

testsT3 :: Test
testsT3 = TestLabel "Jogador encontra-se no topo do mapa em Relva" $ test ["Topo do mapa" ~:  Jogo (Jogador (2,5)) (Mapa 5 [(Rio 2,[Nenhum,Nenhum,Tronco,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Tronco])]) ~=? deslizaJogo (Jogo (Jogador (2,5))  (Mapa 5 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Relva,[Arvore, Nenhum, Nenhum, Arvore, Arvore])]))]

testsT4 :: Test
testsT4 = TestLabel "Jogador encontra-se no topo do mapa em Rio" $ test ["Topo do mapa" ~: Jogo (Jogador (2,5)) (Mapa 5 [(Rio 2,[Nenhum,Nenhum,Tronco,Tronco,Nenhum]),(Rio 2,[Nenhum,Tronco,Tronco,Tronco,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Tronco])]) ~=? deslizaJogo (Jogo (Jogador (2,5))  (Mapa 5 [(Rio 2 , [Nenhum , Tronco , Tronco , Tronco, Nenhum]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Relva,[Arvore, Nenhum, Nenhum, Arvore, Arvore])]))]

testsT5 :: Test
testsT5 = TestLabel "Jogador encontra-se no topo do mapa em Estrada" $ test ["Topo do mapa" ~: Jogo (Jogador (2,5)) (Mapa 5 [(Rio 2,[Nenhum,Nenhum,Tronco,Tronco,Nenhum]),(Estrada 2,[Carro,Carro,Carro,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Tronco])]) ~=? deslizaJogo (Jogo (Jogador (2,5))  (Mapa 5 [(Estrada 2, [Carro, Carro, Carro, Nenhum , Nenhum]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Relva,[Arvore, Nenhum, Nenhum, Arvore, Arvore])]))]

testsT6 :: Test 
testsT6 = TestLabel "Jogador encontra-se na base do mapa em Relva" $ test ["Base do mapa" ~: Jogo (Jogador (2,0)) (Mapa 5 [(Estrada 2,[Carro,Carro,Nenhum,Carro,Nenhum]),(Estrada 2,[Carro,Carro,Carro,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Tronco])]) ~=? deslizaJogo (Jogo (Jogador (2,0))  (Mapa 5 [(Estrada 2, [Carro, Carro, Carro, Nenhum , Nenhum]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Relva,[Arvore, Nenhum, Nenhum, Arvore, Arvore])]))]

testsT7 :: Test
testsT7 = TestLabel "Jogador encontra-se na base do mapa em Rio" $ test ["Base do mapa" ~: Jogo (Jogador (1,0)) (Mapa 5 [(Rio 2,[Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Estrada 2,[Carro,Carro,Carro,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Tronco])]) ~=? deslizaJogo (Jogo (Jogador (1,0))  (Mapa 5 [(Estrada 2, [Carro, Carro, Carro, Nenhum , Nenhum]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Rio 2 , [Tronco, Tronco , Tronco , Nenhum,Nenhum])]))]

testsT8 :: Test 
testsT8 = TestLabel "Jogador encontra-se na base do mapa em Estrada" $ test ["Base do mapa" ~: Jogo (Jogador (1,0)) (Mapa 5 [(Rio 2,[Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Estrada 2,[Carro,Carro,Carro,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Tronco])]) ~=? deslizaJogo (Jogo (Jogador (1,0))  (Mapa 5 [(Estrada 2, [Carro, Carro, Carro, Nenhum , Nenhum]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Estrada (-1), [Carro, Nenhum, Carro, Nenhum , Nenhum])]))]

testsT9 :: Test
testsT9 = TestLabel "Teste coordenadas" $ test ["Diferente mapa. Soma das coordenadas igual. Geram a mesma linha de (Terreno,[Obstaculo])" ~: Jogo (Jogador (2,4)) (Mapa 5 [(Relva,[Nenhum,Nenhum,Nenhum,Arvore,Nenhum]),(Rio 2,[Nenhum,Tronco,Tronco,Tronco,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Tronco])]) ~=? deslizaJogo (Jogo (Jogador (2,4))  (Mapa 5 [(Rio 2 , [Nenhum , Tronco , Tronco , Tronco, Nenhum]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Relva,[Arvore, Nenhum, Nenhum, Arvore, Arvore])]))]

testsT10 :: Test
testsT10 = TestLabel "Teste coordenadas" $ test ["Diferente mapa. Soma das coordenadas igual. Geram a mesma linha de (Terreno,[Obstaculo])" ~: Jogo (Jogador (3,3)) (Mapa 5 [(Relva,[Nenhum,Nenhum,Nenhum,Arvore,Nenhum]),(Rio 2,[Tronco,Tronco,Tronco,Nenhum,Nenhum]),(Estrada 0,[Carro,Carro,Nenhum,Nenhum,Carro]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum,Carro])]) ~=? deslizaJogo (Jogo (Jogador (3,3))  (Mapa 5 [(Rio 2 , [Tronco, Tronco , Tronco , Nenhum, Nenhum]),(Estrada 0, [Carro,Carro,Nenhum,Nenhum,Carro]),(Relva, [Arvore , Arvore, Arvore, Nenhum, Nenhum]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Estrada (-1),[Carro, Carro,Nenhum,Nenhum,Carro]),(Estrada 1 , [Carro, Carro,Carro,Nenhum,Nenhum])]))]

{-testsT11 :: Test
testsT11 = TestLabel "" $ test [""   ~: Jogo (Jogador (1,0)) (Mapa 4 [(Rio 2,[Tronco,Tronco,Tronco,Nenhum]),(Rio 2,[Tronco,Tronco,Nenhum,Nenhum]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco]),(Rio 3,[Tronco,Nenhum,Nenhum,Nenhum])]) ~=? deslizaJogo (Jogo (Jogador (1,0)) (Mapa 4 [(Rio 2, [Tronco,Tronco,Nenhum,Nenhum]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco]) ,(Rio 3, [Tronco,Nenhum,Nenhum,Nenhum]), (Rio (-1), [Nenhum,Tronco,Tronco,Nenhum])])    

testsT12 :: Test
testsT12 = TestLabel "" $ test ["" ~:  ~=? deslizaJogo 

testsT13 :: Test
testsT13 = TestLabel "" $ test ["" ~:  ~=? -}


testar = runTestTT (TestList [testsT1, testsT2, testsT3, testsT4 , testsT5, testsT6 , testsT7 , testsT8 , testsT9 , testsT10 ]) -- testsT11 , testsT12 , testsT13