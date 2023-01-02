module Tarefa3_2022li1g062_Spec where

import LI12223
import Tarefa3_2022li1g062
import Test.HUnit


testsT1 :: Test
testsT1 = TestLabel "Teste direçao" $ test ["Teste Cima" ~: Jogador (3,5) ~=? moveJogador (Move Cima) (Jogador (3 ,4 ) ) ]

testsT2 :: Test 
testsT2 = TestLabel "Teste direçao" $ test ["Teste Baixo" ~: Jogador (2,3) ~=?  moveJogador (Move Baixo) (Jogador (2 ,4 ) ) ]

testsT3 :: Test
testsT3 = TestLabel "Teste direçao" $ test ["Teste Esquerda" ~: Jogador (1,9) ~=? moveJogador (Move Esquerda) (Jogador (2 ,9 ) ) ]

testsT4 :: Test
testsT4 = TestLabel "Teste direçao" $ test ["Teste Direita" ~: Jogador (4,9) ~=? moveJogador (Move Direita) (Jogador (3 ,9 ) ) ]

testsT5 :: Test
testsT5 = TestLabel "Teste movimentoValido" $ test ["Teste movimento Cima" ~: Jogo (Jogador (3,1)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Arvore]),(Estrada (-2),[Carro,Carro,Nenhum,Carro]),(Rio (-2),[Tronco,Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum])]) ~=? validoMovimento (Jogo (Jogador (3,0)) ( Mapa 4  [(Relva, [Nenhum,Nenhum,Arvore,Arvore]), (Estrada (-2), [Carro,Carro,Nenhum, Carro ]) , (Rio (-2) , [Tronco , Tronco,Nenhum,Nenhum]) , ( Relva, [Arvore,Arvore,Arvore,Nenhum])]))  (Move Cima ) ]

testsT6 :: Test 
testsT6 = TestLabel "Teste movimentoValido" $ test ["Teste movimento Cima, com arvore na linha acima" ~: Jogo (Jogador (3,2)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Arvore]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum]),(Rio (-2),[Tronco,Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum])]) ~=?  validoMovimento (Jogo (Jogador (3,2)) ( Mapa 4  [(Relva, [Nenhum,Nenhum,Arvore,Arvore]), (Estrada (-2), [Carro,Carro,Nenhum, Nenhum]) , (Rio (-2) , [Tronco , Tronco,Nenhum,Nenhum]) , ( Relva, [Arvore,Arvore,Arvore,Nenhum])]))  (Move Cima )   ]

testsT7 :: Test
testsT7 = TestLabel "Teste movimentoValido" $ test ["Teste movimento Baixo" ~: Jogo (Jogador (3,0)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Arvore]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum]),(Rio (-2),[Tronco,Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum])]) ~=?  validoMovimento  (Jogo (Jogador (3,1)) ( Mapa 4  [(Relva, [Nenhum,Nenhum,Arvore,Arvore]), (Estrada (-2), [Carro,Carro,Nenhum, Nenhum]) , (Rio (-2) , [Tronco , Tronco,Nenhum,Nenhum]) , ( Relva, [Arvore,Arvore,Arvore,Nenhum])]))  (Move Baixo)    ]

testsT8 :: Test 
testsT8 = TestLabel "Teste movimentoValido" $ test ["Teste movimento Baixo, com arvore na linha abaixo" ~:  Jogo (Jogador (0,1)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Arvore]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum]),(Rio (-2),[Tronco,Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum])])   ~=? validoMovimento (Jogo (Jogador (0,1)) ( Mapa 4  [(Relva, [Nenhum,Nenhum,Arvore,Arvore]), (Estrada (-2), [Carro,Carro,Nenhum, Nenhum]) , (Rio (-2) , [Tronco , Tronco,Nenhum,Nenhum]) , ( Relva, [Arvore,Arvore,Arvore,Nenhum])]))  (Move Baixo)]

testsT9 :: Test 
testsT9 = TestLabel "Teste movimentoValido" $ test ["Teste movimento Esquerda" ~: Jogo (Jogador (0,3)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Arvore]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum]),(Rio (-2),[Tronco,Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum])]) ~=? validoMovimento (Jogo (Jogador (  1,3)) ( Mapa 4  [(Relva, [Nenhum,Nenhum,Arvore,Arvore]), (Estrada (-2), [Carro,Carro,Nenhum, Nenhum]) , (Rio (-2) , [Tronco , Tronco,Nenhum,Nenhum]) , ( Relva, [Arvore,Arvore,Arvore,Nenhum])]))  (Move Esquerda) ]

testsT10 :: Test
testsT10 = TestLabel "Teste movimentoValido" $ test ["Teste movimento Esquerda, com arvore a esquerda do Jogador" ~: Jogo (Jogador (3,0)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Arvore]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum]),(Rio (-2),[Tronco,Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum])]) ~=? validoMovimento (Jogo (Jogador (3,0)) ( Mapa 4  [(Relva, [Nenhum,Nenhum,Arvore,Arvore]), (Estrada (-2), [Carro,Carro,Nenhum, Nenhum]) , (Rio (-2) , [Tronco , Tronco,Nenhum,Nenhum]) , ( Relva, [Arvore,Arvore,Arvore,Nenhum])]))  (Move Esquerda)  ]

testsT11 :: Test 
testsT11 = TestLabel "Teste movimentoValido" $ test ["Teste movimento Direita" ~: Jogo (Jogador (3,2)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Arvore]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum]),(Rio (-2),[Tronco,Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum])]) ~=? validoMovimento (Jogo (Jogador (  2,2)) ( Mapa 4  [(Relva, [Nenhum,Nenhum,Arvore,Arvore]), (Estrada (-2), [Carro,Carro,Nenhum, Nenhum]) , (Rio (-2) , [Tronco , Tronco,Nenhum,Nenhum]) , ( Relva, [Arvore,Arvore,Arvore,Nenhum])]))  (Move Direita) ]

testsT12 :: Test
testsT12 = TestLabel "Teste movimentoValido" $ test ["Teste movimento Direita, com arvore a direita do Jogador" ~:  Jogo (Jogador (1,3)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Arvore]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum]),(Rio (-2),[Tronco,Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum])]) ~=? validoMovimento (Jogo (Jogador (  1,3)) ( Mapa 4  [(Relva, [Nenhum,Nenhum,Arvore,Arvore]), (Estrada (-2), [Carro,Carro,Nenhum, Nenhum]) , (Rio (-2) , [Tronco , Tronco,Nenhum,Nenhum]) , ( Relva, [Arvore,Arvore,Arvore,Nenhum])]))  (Move Direita) ]

testsT13 :: Test 
testsT13 = TestLabel "Teste moveObs" $ test ["Teste movimento dos obstaculos" ~: [(Relva,[Nenhum,Nenhum,Arvore,Arvore]),(Estrada (-2),[Nenhum,Nenhum,Carro,Carro]),(Rio 1,[Tronco,Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum])] ~=? moveObs 1 [(Relva, [Nenhum,Nenhum,Arvore,Arvore]), (Estrada (-2), [Carro,Carro,Nenhum, Nenhum]) , (Rio (1) , [Tronco , Tronco,Nenhum,Nenhum]) , ( Relva, [Arvore,Arvore,Arvore,Nenhum])] ]

testsT14 :: Test
testsT14 = TestLabel "Teste Carro posicoes" $ test ["Teste que indica as posicoes de Carro" ~: [0,1,2,4] ~=?  posicao_carro [Carro, Carro, Carro, Nenhum , Carro, Nenhum] ]

testsT15 :: Test 
testsT15 = TestLabel "Teste moveObs" $ test ["Teste movimento dos obstaculos" ~: [(Relva,[Nenhum,Nenhum,Arvore,Arvore]),(Estrada (-2),[Nenhum,Nenhum,Carro,Carro]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco]),(Relva,[Arvore,Arvore,Arvore,Nenhum])] ~=? moveObs 2 [(Relva, [Nenhum,Nenhum,Arvore,Arvore]), (Estrada (-2), [Carro,Carro,Nenhum, Nenhum]) , (Rio (1) , [Tronco , Tronco,Nenhum,Nenhum]) , ( Relva, [Arvore,Arvore,Arvore,Nenhum])] ]

testsT16 :: Test 
testsT16 = TestLabel "MovenoTronco" $ test ["Mover no Tronco" ~:  (2,1) ~=? move_tronco (0,1) ( Mapa 4  [(Relva, [Nenhum,Nenhum,Arvore,Arvore]), (Estrada (-2), [Carro,Carro,Nenhum, Nenhum]) , (Rio (2) , [Tronco , Tronco,Nenhum,Nenhum]) , ( Relva, [Arvore,Arvore,Arvore,Nenhum])])       ] 
 
testsT17 :: Test
testsT17 = TestLabel "MovenoTronco" $ test ["Mover no Tronco" ~:  (1,1) ~=? move_tronco (0,1) ( Mapa 4  [(Relva, [Nenhum,Nenhum,Arvore,Arvore]), (Estrada (-2), [Carro,Carro,Nenhum, Nenhum]) , (Rio (1) , [Tronco , Tronco,Nenhum,Nenhum]) , ( Relva, [Arvore,Arvore,Arvore,Nenhum])]) ]

testsT18:: Test
testsT18 = TestLabel "Teste Tronco posicoes" $ test ["Teste que indica as posicoes de Tronco" ~:  [0,2] ~=? posicao_tronco [Tronco,Nenhum,Tronco] ]

testsT19 :: Test
testsT19 = TestLabel "Teste Arvore posicoes" $ test ["Teste que indica as posicoes de Arvore" ~: [0,5,6]  ~=? posicao_arvore [Arvore, Nenhum , Nenhum , Nenhum , Nenhum , Arvore , Arvore ] ]

testsT20 :: Test
testsT20 = TestLabel "animaJogoTempo" $ test ["Anima o jogo tendo o tempo em conta" ~:  Jogo (Jogador (2,0)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Estrada (-1),[Nenhum,Nenhum,Nenhum,Carro,Carro]),(Rio 1,[Tronco,Tronco,Nenhum,Nenhum,Tronco]),(Rio (-2),[Nenhum,Tronco,Tronco,Tronco,Tronco]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore])]) ~=?  animaJogoTempo (Jogo ( Jogador (2,0)) (Mapa 5 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Relva,[Arvore, Nenhum, Nenhum, Arvore, Arvore])]) )  ]

testsT21 :: Test
testsT21 = TestLabel "animaJogoTempo" $ test ["Anima o jogo tendo o tempo em conta" ~: Jogo (Jogador (1,0)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Estrada (-1),[Nenhum,Nenhum,Nenhum,Carro,Carro]),(Rio 1,[Tronco,Tronco,Nenhum,Nenhum,Tronco]),(Rio (-2),[Nenhum,Tronco,Tronco,Tronco,Tronco]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore])])  ~=?  animaJogoTempo (Jogo ( Jogador (1,0)) (Mapa 5 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Relva,[Arvore, Nenhum, Nenhum, Arvore, Arvore])]) )  ]

testsT22 :: Test
testsT22 = TestLabel "LinhaDoJogador" $ test ["Funcao que indica a linha em que o Jogador se encontra" ~: (Relva, [Nenhum,Arvore]) ~=? linha_jogador [(Relva, [Nenhum,Arvore]), (Rio 2 , [Tronco , Nenhum]) ] 1     ]

testsT23 :: Test
testsT23 = TestLabel "Atropelamento" $ test ["Atropelado pela direita" ~: [(Rio 2,[Tronco,Tronco,Nenhum,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum,Carro]),(Estrada (-1),[Nenhum,Carro,Carro,Carro])]  ~=?  atropelamento (Jogo (Jogador (1,0)) (Mapa 4 [(Rio 2 , [Tronco , Tronco,Nenhum,Nenhum]),(Estrada 1 , [Carro,Carro,Nenhum,Carro]),(Estrada (-1),[Carro, Nenhum , Carro, Carro])]) )   ]

testsT24 :: Test
testsT24 = TestLabel "Atropelamento" $ test ["Atropelado pela direita" ~: [(Rio 2,[Tronco,Tronco,Nenhum,Nenhum]),(Estrada 1,[Carro,Carro,Nenhum,Carro]),(Estrada (-2),[Nenhum,Carro,Carro,Carro])] ~=?  atropelamento (Jogo (Jogador (1,0)) (Mapa 4 [(Rio 2 , [Tronco , Tronco,Nenhum,Nenhum]),(Estrada 1 , [Carro,Carro,Nenhum,Carro]),(Estrada (-2),[Carro, Nenhum , Carro, Carro])]) )   ]

testsT25 :: Test
testsT25 = TestLabel "Atropelamento" $ test ["Atropelado pela esquerda" ~: [(Rio 2,[Tronco,Tronco,Nenhum,Nenhum]),(Estrada 1,[Carro,Carro,Carro,Nenhum]),(Estrada (-2),[Carro,Nenhum,Carro,Carro])] ~=?  atropelamento (Jogo (Jogador (2,1)) (Mapa 4 [(Rio 2 , [Tronco , Tronco,Nenhum,Nenhum]),(Estrada 1 , [Carro,Carro,Nenhum,Carro]),(Estrada (-2),[Carro, Nenhum , Carro, Carro])]) )  ]

testsT26 :: Test
testsT26 = TestLabel "Atropelamento" $ test ["Atropelamento pela esquerda" ~: [(Rio 2,[Tronco,Tronco,Nenhum,Nenhum]),(Estrada 2,[Carro,Carro,Carro,Nenhum]),(Estrada (-2),[Carro,Nenhum,Carro,Carro])] ~=? atropelamento (Jogo (Jogador (2,1)) (Mapa 4 [(Rio 2 , [Tronco , Tronco,Nenhum,Nenhum]),(Estrada 2 , [Carro,Carro,Nenhum,Carro]),(Estrada (-2),[Carro, Nenhum , Carro, Carro])]) )   ]

testsT27 :: Test
testsT27 = TestLabel "Teste movimentoValido" $ test ["Teste movimento Parado" ~: Jogo (Jogador (2,0)) (Mapa 5 [(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Tronco])]) ~=? validoMovimento (Jogo (Jogador (2,0)) (Mapa 5 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco])]) ) (Parado)]

todos3 = runTestTT (TestList [testsT1, testsT2, testsT3, testsT4 , testsT5, testsT6 , testsT7 , testsT8 , testsT9 , testsT10 , testsT11 , testsT12 , testsT13 , testsT14 , testsT15, testsT16 , testsT17, testsT18, testsT19, testsT20, testsT21, testsT22, testsT23 , testsT24, testsT25,testsT26,testsT27])