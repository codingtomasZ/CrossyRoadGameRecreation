module Tarefa3_2022li1g062_Spec where

import LI12223
import Tarefa3_2022li1g062
import Test.HUnit

--testsT3 :: Test
--testsT3 = TestLabel "Testes Tarefa 3" $ test ["Teste 1" ~: 1 ~=? 1]

testsT1 :: Test
testsT1 = TestLabel "Teste direçao" $ test ["Teste Cima " ~: Jogador (3,3) ~=? moveJogador Cima (Jogador (3 ,4 ) ) ]

testsT2 :: Test 
testsT2 = TestLabel "Teste direçao" $ test ["Teste Baixo" ~: Jogador (2,5) ~=?  moveJogador Baixo (Jogador (2 ,4 ) ) ]

testsT3 :: Test
testsT3 = TestLabel "Teste direçao" $ test ["Teste depois das 5 Relva" ~: Jogador (1,9) ~=? moveJogador Esquerda (Jogador (2 ,9 ) ) ]

testsT4 :: Test
testsT4 = TestLabel "Teste direçao" $ test ["Teste de um terreno valido apos 5 Relva contiguas" ~: Jogador (3,9) ~=? moveJogador Esquerda (Jogador (2 ,9 ) ) ]

testsT5 :: Test
testsT5 = TestLabel "Teste movimentoValido" $ test ["Teste movimento" ~: True ~=? validoMovimento (Mapa 2 [(Relva , [Arvore , Nenhum]) ]) (Jogador (3,2)) Esquerda]

testsT6 :: Test 
testsT6 = TestLabel "Teste movimentoValido" $ test ["Teste movimento" ~: False ~=? validoMovimento (Mapa 2 [(Relva , [Arvore , Nenhum]) ]) (Jogador (2,2)) Direita ]

testsT7 :: Test
testsT7 = TestLabel "Teste movimentoValido" $ test ["Teste movimento" ~: False ~=? validoMovimento (Mapa 2 [(Relva , [Arvore , Nenhum]) ]) (Jogador (0,4)) Esquerda]

testsT8 :: Test 
testsT8 = TestLabel "Teste movimentoValido" $ test ["Teste movimento" ~: False ~=? validoMovimento (Mapa 2 [(Relva , [Arvore , Nenhum]) ]) (Jogador (2,4)) Direita]

testsT9 :: Test 
testsT9 = TestLabel "Teste movimentoValido" $ test ["Teste movimento" ~: True ~=? validoMovimento (Mapa 2 [(Relva , [Arvore , Nenhum]) ]) (Jogador (2,4)) Cima]

testsT10 :: Test
testsT10 = TestLabel "Teste movimentoValido" $ test ["Teste movimento" ~: False ~=? validoMovimento (Mapa 2 [(Relva , [Arvore , Nenhum]) ]) (Jogador (2,0)) Cima]

testsT11 :: Test 
testsT11 = TestLabel "Teste movimentoValido" $ test ["Teste movimento" ~: True ~=? validoMovimento (Mapa 2 [(Relva , [Arvore , Nenhum]) ]) (Jogador (2,0)) Baixo]

testsT12 :: Test 
testsT12 = TestLabel "Teste moveObs" $ test ["Teste movimento dos obstaculos" ~: [(Relva,[Arvore,Arvore])] ~=? moveObs [(Relva , [Arvore , Arvore])] ]

testsT13 :: Test
testsT13 = TestLabel "Teste moveObs" $ test ["Teste movimento" ~: [(Rio 0,[Tronco,Nenhum])] ~=? moveObs [(Rio 2 , [Tronco , Nenhum])] ]

testsT14 :: Test 
testsT14 = TestLabel "Teste moveObs" $ test ["Teste movimento" ~: [(Rio 0,[Tronco,Nenhum,Tronco,Nenhum,Tronco])] ~=? moveObs [(Rio 2 , [Tronco , Nenhum, Tronco , Tronco, Nenhum])] ]



todos3 = runTestTT (TestList [testsT1, testsT2, testsT3, testsT4 , testsT5, testsT6 , testsT7 , testsT8 , testsT9 , testsT10 , testsT11 , testsT12 , testsT13 , testsT14])