module Tarefa1_2022li1g062_Spec where

import LI12223
import Tarefa1_2022li1g062
import Test.HUnit


testsT1 :: Test
testsT1 = TestLabel "Teste dos obstaculos" $ test ["Teste Tronco em Estrada" ~: False ~=? mapaValido1 (Mapa 3 [(Estrada 2, [Tronco, Carro, Nenhum])])]

testsT2 :: Test 
testsT2 = TestLabel "Teste dos obstaculos" $ test ["Teste Arvore em Estrada" ~: False ~=?  mapaValido1 (Mapa 3 [(Estrada 2, [Arvore, Carro, Nenhum])])]

testsT3 :: Test
testsT3 = TestLabel "Teste dos obstaculos" $ test ["Teste Carro em Rio" ~: False ~=?  mapaValido1 (Mapa 3 [(Rio 2, [Tronco, Carro, Nenhum])])]

testsT4 :: Test
testsT4 = TestLabel "Teste dos obstaculos" $ test ["Teste Arvore em Rio" ~: False ~=? mapaValido1 (Mapa 3 [(Rio 2, [Arvore, Tronco, Nenhum])])]

testsT5 :: Test
testsT5 = TestLabel "Teste dos obstaculos" $ test ["Teste Tronco em Relva" ~: False ~=? mapaValido1 (Mapa 3 [(Relva, [Arvore, Tronco, Nenhum])])]

testsT6 :: Test 
testsT6 = TestLabel "Teste dos obstaculos" $ test ["Teste Carro em Relva" ~: False ~=? mapaValido1 (Mapa 3 [(Relva , [Carro, Nenhum, Nenhum])])]

testsT7 :: Test
testsT7 = TestLabel "Teste dos rios" $ test ["Teste rios contiguos direçoes opostas" ~: False ~=? mapaValido2 (Mapa 2 [(Rio 2, [Tronco, Nenhum]), (Rio 2 , [Tronco, Nenhum])])]

testsT8 :: Test 
testsT8 = TestLabel "Teste dos obstaculos" $ test ["Teste max5 Tronco" ~: False ~=? mapaValido3 (Mapa 5 [(Rio 1, [Tronco , Tronco , Nenhum , Tronco , Nenhum]), (Rio (1), [Nenhum , Tronco , Tronco , Tronco , Tronco, Tronco, Tronco])]) ]

testsT9 :: Test
testsT9 = TestLabel "Teste dos obstaculos" $ test ["Teste max5 Tronco" ~: True ~=? mapaValido3 (Mapa 6 [(Rio 1, [Tronco , Tronco , Nenhum , Tronco , Nenhum, Tronco]), (Rio (1), [Nenhum , Tronco , Tronco , Tronco , Tronco, Tronco])]) ]

testsT10 :: Test
testsT10 = TestLabel "Teste dos obstaculos" $ test ["Teste max3 Carro" ~: True ~=? mapaValido4 (Mapa 5 [(Estrada 2 , [Carro , Carro, Carro, Nenhum, Nenhum]), (Estrada (-3)  , [Carro , Carro, Carro , Nenhum , Nenhum])]) ]

testsT11 :: Test
testsT11 = TestLabel "Teste dos obstaculos" $ test ["Teste max3 Carro" ~: False ~=? mapaValido4 (Mapa 5 [(Estrada 2 , [Carro , Carro, Carro, Carro, Nenhum]), (Estrada (-3)  , [Carro , Carro, Carro , Nenhum , Nenhum])])]

testsT12 :: Test
testsT12 = TestLabel "Teste dos obstaculos" $ test ["Teste max5 Tronco" ~: True ~=? mapaValido3 (Mapa 5 [(Rio 1, [Tronco , Tronco , Nenhum , Tronco , Nenhum]), (Rio (1), [Nenhum , Tronco , Tronco , Tronco , Nenhum])])]

testsT13 :: Test
testsT13 = TestLabel "Teste Nenhum" $ test ["Minimo um obstaculo Nenhum" ~: False ~=? mapaValido5 (Mapa 3 [(Rio 4 , [Tronco , Tronco ,Tronco])])]

testsT14 :: Test
testsT14 = TestLabel "Teste Nenhum" $ test ["Minimo um obstaculo Nenhum" ~: True ~=? mapaValido5 (Mapa 3 [(Estrada 3 , [Carro, Nenhum , Nenhum])])]

testsT15 :: Test
testsT15 = TestLabel "Teste Nenhum" $ test ["Minimo um obstaculo Nenhum" ~: False ~=? mapaValido5 (Mapa 2 [(Relva , [Arvore , Arvore])])]

testsT16 :: Test 
testsT16 = TestLabel "Teste largura e comprimento lista de obs" $ test ["Teste largura=comprimento lista de obs" ~: False ~=? mapaValido6 (Mapa 3 [(Relva , [Arvore, Nenhum])])]

testsT17 :: Test 
testsT17 = TestLabel "Teste largura e comprimento lista de obs" $ test ["Teste largura=comprimento lista de obs" ~: False ~=? mapaValido6 (Mapa 3 [(Relva , [Arvore, Nenhum])])]

testsT18 :: Test
testsT18 = TestLabel "Teste largura e comprimento lista de obs" $ test ["Teste largura=comprimento lista de obs" ~: True ~=? mapaValido6 (Mapa 2 [(Rio 1 , [Tronco , Nenhum])])]

testsT19 :: Test 
testsT19 = TestLabel "Teste rios" $ test ["Teste max4 Rio contiguos" ~: False ~=? mapaValido7 (Mapa 2 [(Rio 2 , [Tronco , Nenhum]) , (Rio 2 , [Tronco , Nenhum]) , (Rio 2 , [Tronco , Nenhum]) , (Rio 2 , [Tronco , Nenhum]) , (Rio 2 , [Tronco , Nenhum]) ])]

testsT20 :: Test 
testsT20 = TestLabel "Teste rios" $ test ["Teste max4 Rio contiguos" ~: True ~=? mapaValido7 (Mapa 2 [(Rio 2 , [Tronco , Nenhum]) , (Rio 2 , [Tronco , Nenhum]) , (Rio 2 , [Tronco , Nenhum]) , (Rio 2 , [Tronco , Nenhum])])]

testsT21 :: Test
testsT21 = TestLabel "Teste max5 Estrada contiguas" $ test ["Estrada max5" ~: False ~=? mapaValido7 (Mapa 2 [(Estrada 3 , [Carro, Nenhum]) , (Estrada 3 , [Carro, Nenhum]) , (Estrada 3 , [Carro, Nenhum]) , (Estrada 3 , [Carro, Nenhum]) , (Estrada 3 , [Carro, Nenhum]) , (Estrada 3 , [Carro, Nenhum]) ])]

testsT22 :: Test
testsT22 = TestLabel "Teste max5 Estrada contiguas" $ test ["Estrada max5" ~: True ~=? mapaValido7 (Mapa 2 [(Estrada 3 , [Carro, Nenhum]) , (Estrada 3 , [Carro, Nenhum]) , (Estrada 3 , [Carro, Nenhum]) , (Estrada 3 , [Carro, Nenhum]) , (Estrada 3 , [Carro, Nenhum]) ])]

testsT23 :: Test
testsT23 = TestLabel "Teste max5 Relva contiguas" $ test ["Relva max5" ~: False ~=? mapaValido7 (Mapa 2 [(Relva , [Arvore , Nenhum]) , (Relva , [Arvore , Nenhum]) , (Relva , [Arvore , Nenhum]) , (Relva , [Arvore , Nenhum]) , (Relva , [Arvore , Nenhum]) , (Relva , [Arvore , Nenhum]) ])]

testsT24 :: Test
testsT24 = TestLabel "Teste max5 Relva contiguas" $ test ["Relva max5" ~: True ~=? mapaValido7 (Mapa 2 [(Relva , [Arvore , Nenhum]) , (Relva , [Arvore , Nenhum]) , (Relva , [Arvore , Nenhum]) , (Relva , [Arvore , Nenhum]) , (Relva , [Arvore , Nenhum]) ])]

testsT25 :: Test
testsT25 = TestLabel "Teste max4 Rio contiguos" $ test ["Terreno Relva seguido de 5 rios contiguos" ~: False ~=? mapaValido7 (Mapa 2 [(Relva , [Arvore, Nenhum]) , (Rio 3 , [Nenhum , Tronco]), (Rio 3 , [Nenhum , Tronco]), (Rio 3 , [Nenhum , Tronco]), (Rio 3 , [Nenhum , Tronco]), (Rio 3 , [Nenhum , Tronco]) ])]

{-testsT26 :: Test
testsT26 = TestLabel

testsT27 :: Test
testsT27 = TestLabel

testsT28 :: Test
testsT28 = TestLabel -}

todos1 = runTestTT (TestList [testsT1, testsT2, testsT3, testsT4 , testsT5, testsT6 , testsT7 , testsT8 , testsT9 , testsT10 , testsT11 , testsT12 , testsT13 , testsT14 , testsT15 , testsT16 , testsT17 , testsT18,testsT19,testsT20, testsT21 , testsT22 , testsT23 , testsT24 , testsT25 ]) -- testsT26, testsT27, testsT28