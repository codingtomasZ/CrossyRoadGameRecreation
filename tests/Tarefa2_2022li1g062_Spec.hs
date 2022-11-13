module Tarefa2_2022li1g062_Spec where

import LI12223
import Tarefa2_2022li1g062
import Test.HUnit

testsT1 :: Test
testsT1 = TestLabel "Teste do proximo terreno" $ test ["Teste depois das 5 Estrada" ~: [Rio 0,Relva] ~=? proximosTerrenosValidos (Mapa 2 [(Estrada 3, [Carro, Nenhum]) , (Estrada 3, [Carro, Nenhum]), (Estrada 3, [Carro, Nenhum]), (Estrada 3, [Carro, Nenhum]), (Estrada 3, [Carro, Nenhum]), (Estrada 3, [Carro,Nenhum])])]

testsT2 :: Test 
testsT2 = TestLabel "Teste do proximo terreno" $ test ["Teste depois dos 4 Rio" ~: [Relva,Rio 0,Estrada 0] ~=?  proximosTerrenosValidos (Mapa 2 [(Rio 3, [Tronco, Nenhum]) , (Rio 3, [Tronco, Nenhum]), (Rio 3, [Tronco, Nenhum]), (Rio 3, [Tronco, Nenhum]), (Rio 3, [Tronco, Nenhum]), (Rio 3, [Tronco,Nenhum])])]

testsT3 :: Test
testsT3 = TestLabel "Teste do proximo terreno" $ test ["Teste depois das 5 Relva" ~: [Estrada 0,Rio 0] ~=?  proximosTerrenosValidos (Mapa 2 [(Relva, [Arvore,Nenhum]), (Relva, [Arvore,Nenhum]), (Relva, [Arvore,Nenhum]), (Relva, [Arvore,Nenhum]), (Relva, [Arvore,Nenhum])])]

testsT4 :: Test
testsT4 = TestLabel "Teste do proximo terreno" $ test ["Teste de um terreno valido apos 5 Relva contiguas" ~: [Relva,Rio 0,Estrada 0] ~=? proximosTerrenosValidos (Mapa 2 [(Relva, [Arvore,Nenhum]), (Relva, [Arvore,Nenhum]), (Relva, [Arvore,Nenhum]), (Relva, [Arvore,Nenhum]), (Relva, [Arvore,Nenhum]), (Estrada 3, [Carro,Nenhum])])]

testsT5 :: Test
testsT5 = TestLabel "Teste dos proximos obstaculos" $ test ["Teste lista vazia em Obstaculo com Terreno Estrada" ~: [Nenhum, Carro] ~=? proximosObauxiliar 10 (Estrada 5, [])]

testsT6 :: Test 
testsT6 = TestLabel "Teste dos proximos obstaculos" $ test ["Teste lista vazia em Obstaculo com Terreno Rio" ~: [Nenhum,Tronco] ~=? proximosObauxiliar 10 (Rio 4, [])]

testsT7 :: Test
testsT7 = TestLabel "Teste dos proximos obstaculos" $ test ["Teste Estrada independentemente da Velocidade" ~: [] ~=? proximosObauxiliar 2 (Estrada 3, [Carro, Nenhum])]

testsT8 :: Test 
testsT8 = TestLabel "Teste da funçao estendeMapa" $ test ["Teste de gerar uma nova linha no mapa valido" ~: Mapa 3 [(Relva,[]),(Relva,[Arvore,Arvore,Nenhum])] ~=? estendeMapa (Mapa 3 [(Relva , [])]) 3 ]


todos1 = runTestTT (TestList [testsT1, testsT2, testsT3, testsT4 , testsT5, testsT6 , testsT7 , testsT8 ])