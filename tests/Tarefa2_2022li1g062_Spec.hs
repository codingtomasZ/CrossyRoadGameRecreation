module Tarefa2_2022li1g062_Spec where

import LI12223
import Tarefa2_2022li1g062
import Test.HUnit

testsT1 :: Test
testsT1 = TestLabel "Teste do proximo terreno" $ test ["Teste depois das 5 Estrada" ~: [Rio 2,Relva] ~=? proximosTerrenosValidos 2 (Mapa 2 [(Estrada 3, [Carro, Nenhum]) , (Estrada 3, [Carro, Nenhum]), (Estrada 3, [Carro, Nenhum]), (Estrada 3, [Carro, Nenhum]), (Estrada 3, [Carro, Nenhum])])]

testsT2 :: Test 
testsT2 = TestLabel "Teste do proximo terreno" $ test ["Teste depois dos 4 Rio" ~: [Estrada 1,Relva] ~=?  proximosTerrenosValidos 1 (Mapa 2 [(Rio 3, [Tronco, Nenhum]) , (Rio 3, [Tronco, Nenhum]), (Rio 3, [Tronco, Nenhum]), (Rio 3, [Tronco, Nenhum])])]

testsT3 :: Test
testsT3 = TestLabel "Teste do proximo terreno" $ test ["Teste depois das 5 Relva" ~: [Estrada 3,Rio 3] ~=?  proximosTerrenosValidos 3 (Mapa 2 [(Relva, [Arvore,Nenhum]), (Relva, [Arvore,Nenhum]), (Relva, [Arvore,Nenhum]), (Relva, [Arvore,Nenhum]), (Relva, [Arvore,Nenhum])])]

testsT4 :: Test 
testsT4 = TestLabel "Teste dos proximos obstaculos" $ test ["Teste em Relva" ~: [Nenhum,Arvore] ~=?  proximosObstaculosValidos 4 (Relva, [Arvore, Arvore ,Nenhum ,Nenhum ]) ]

testsT5 :: Test
testsT5 = TestLabel "Teste dos proximos obstaculos" $ test ["Teste em Estrada" ~: [Nenhum, Carro] ~=? proximosObstaculosValidos 2 (Estrada 2 , [Carro, Carro, Nenhum , Carro])]

testsT6 :: Test 
testsT6 = TestLabel "Teste dos proximos obstaculos" $ test ["Teste em Rio" ~: [Nenhum,Tronco] ~=? proximosObstaculosValidos 3 (Rio 2 , [Nenhum, Tronco, Tronco,Tronco])    ]

testsT7 :: Test
testsT7 = TestLabel "Teste dos proximos obstaculos" $ test ["Teste Estrada independen" ~: [Nenhum,Tronco] ~=? proximosObstaculosValidos (-2) (Rio 2 , [Nenhum, Tronco, Tronco,Tronco])]

testsT8 :: Test 
testsT8 = TestLabel "Teste da funçao estendeMapa" $ test ["Gerar um novo terreno com os respetivos obstaculos num mapa valido" ~: Mapa 5 [(Estrada 2,[Carro,Carro,Nenhum,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco]),(Rio 1,[Nenhum,Nenhum,Tronco,Tronco,Tronco]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore])] ~=?  estendeMapa (Mapa 5 [(Relva,[Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Rio (1),[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Tronco, Tronco]), (Rio (1), [Nenhum, Nenhum, Tronco, Tronco, Tronco]),(Relva,[Arvore, Nenhum, Nenhum, Arvore, Arvore])]) 2 ]

testsT9 :: Test
testsT9 = TestLabel "Teste da funçao estendeMapa- apos 4 Rio" $ test ["Gerar um novo terreno com os respetivos obstaculos num mapa valido"  ~: Mapa 2 [(Estrada (-2),[Carro,Nenhum]),(Rio 3,[Tronco,Nenhum]),(Rio 3,[Tronco,Nenhum]),(Rio 3,[Tronco,Nenhum]),(Rio 3,[Tronco,Nenhum])] ~=? estendeMapa (Mapa 2 [(Rio 3, [Tronco, Nenhum]) , (Rio 3, [Tronco, Nenhum]), (Rio 3, [Tronco, Nenhum]), (Rio 3, [Tronco, Nenhum])]) 2 ]

testsT10 :: Test 
testsT10 = TestLabel "Teste indicador do sentido" $ test ["Teste velocidade negativa Estrada" ~: False ~=? velocidade_positiva (Estrada (-2), [Carro, Nenhum , Nenhum , Carro ]) ]

testsT11 :: Test 
testsT11 = TestLabel "Teste indicador do sentido" $ test ["Teste velocidade positiva Estrada" ~: True ~=? velocidade_positiva (Estrada (2), [Carro, Nenhum , Nenhum , Carro ]) ] 

testsT12 :: Test 
testsT12 = TestLabel "Teste indicador do sentido" $ test ["Teste velocidade negativa Rio" ~: False ~=?  velocidade_positiva (Rio (-3), [Tronco, Tronco,Nenhum,Nenhum]) ]

testsT13 :: Test 
testsT13 = TestLabel "Teste indicador do sentido" $ test ["Teste velocidade postiiva Rio" ~: True ~=?  velocidade_positiva (Rio (3), [Tronco, Tronco,Nenhum,Nenhum]) ]

testsT14 :: Test
testsT14 = TestLabel "Teste da funçao estendeMapa- apos 5 Relva" $ test ["Gerar um novo terreno com os respetivos obstaculos num mapa valido" ~: Mapa 2 [(Relva,[Nenhum,Nenhum]),(Estrada 3,[Carro,Nenhum]),(Estrada 3,[Carro,Nenhum]),(Estrada 3,[Carro,Nenhum]),(Estrada 3,[Carro,Nenhum]),(Estrada 3,[Carro,Nenhum])] ~=? estendeMapa (Mapa 2 [(Estrada 3, [Carro, Nenhum]) , (Estrada 3, [Carro, Nenhum]), (Estrada 3, [Carro, Nenhum]), (Estrada 3, [Carro, Nenhum]), (Estrada 3, [Carro, Nenhum])]) 1]

testsT15 :: Test
testsT15 = TestLabel "Teste da funçao estendeMapa-apos 5 Estrada" $ test ["Gerar um novo terreno com os respetivos obstaculos num mapa valido" ~: Mapa 2 [(Estrada 2,[Nenhum,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum])] ~=? estendeMapa (Mapa 2 [(Relva, [Arvore,Nenhum]), (Relva, [Arvore,Nenhum]), (Relva, [Arvore,Nenhum]), (Relva, [Arvore,Nenhum]), (Relva, [Arvore,Nenhum])]) (-2)]

todos2 = runTestTT (TestList [testsT1, testsT2, testsT3, testsT4 , testsT5, testsT6 , testsT7 , testsT8, testsT9, testsT10, testsT11, testsT12, testsT13, testsT14, testsT15 ])