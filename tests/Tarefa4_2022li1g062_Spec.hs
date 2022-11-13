module Tarefa4_2022li1g062_Spec where

import LI12223
import Tarefa4_2022li1g062
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test ["Teste 1" ~: 1 ~=? 1]

testAll = runTestTT (TestList [test1_1, test1_2, test1_3, test2_1, test2_2, test2_3, test2_4, test2_5, test2_6, test2_7, test2_8, test2_9, test2_10, test3_1, test3_2, test3_3, test3_4, test3_5, test3_6, test3_7, test3_8, test3_9, test3_10])

--testex = TestCase (assertEqual "descrição" resultado (f input))

--teste_ = TestCase (assertEqual "O jogo termina porque" r (jogoTerminou ))

--Testes primeira função  

teste1_1 = TestCase (assertEqual "O jogo termina porque o jogador sai do mapa pelo lado esquerdo" True (jogoTerminou1 (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore]), (Estrada 1, [Carro, Nenhum, Carro, Nenhum, Carro])]) (Jogador (-2, 2))))

teste1_2 = TestCase (assertEqual "O jogo termina porque o jogador sai do mapa por baixo" True (jogoTerminou1 (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore]), (Estrada 1, [Carro, Nenhum, Carro, Nenhum, Carro])]) (Jogador (2,-2))))

teste1_3 = TestCase (assertEqual "O jogo termina porque o jogador sai do mapa por estar fora da largura do mapa" True (jogoTerminou1 (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore]), (Estrada 1, [Carro, Nenhum, Carro, Nenhum, Carro])]) (Jogador (7,2))))

--Teste segunda função 

teste2_1 = TestCase (assertEqual "O jogo termina porque jogador cai no rio" True (jogoTerminou2 (Mapa 5 [(Rio (-2), [Nenhum, Tronco, Tronco, Tronco, Nenhum]),(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore])])(Jogador (0,0))))

teste2_2 = TestCase (assertEqual "O jogo termina porque jogador cai no rio" True (jogoTerminou2 (Mapa 5 [(Rio 1, [Nenhum, Nenhum, Tronco, Tronco, Nenhum]),(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore])])(Jogador (1,0))))

teste2_3 = TestCase (assertEqual "O jogo termina porque jogador cai no rio" True (jogoTerminou2 (Mapa 5 [(Rio 3, [Tronco, Tronco, Nenhum, Tronco, Nenhum]),(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore])])(Jogador (2,0))))

teste2_4 = TestCase (assertEqual "O jogo termina porque jogador cai no rio" True (jogoTerminou2 (Mapa 5 [(Rio (-2), [Nenhum, Tronco, Tronco, Nenhum, Nenhum]),(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore])])(Jogador (3,0))))

teste2_5 = TestCase (assertEqual "O jogo termina porque jogador cai no rio" True (jogoTerminou2 (Mapa 5 [(Rio 1, [Nenhum, Tronco, Tronco, Nenhum, Nenhum]),(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore])])(Jogador (4,0))))

teste2_6 = TestCase (assertEqual "O jogo termina porque jogador cai no rio" True (jogoTerminou2 (Mapa 5 [(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore]),(Rio 1, [Nenhum, Tronco, Tronco, Nenhum, Nenhum])])(Jogador (0,1))))

teste2_7 = TestCase (assertEqual "O jogo termina porque jogador cai no rio" True (jogoTerminou2 (Mapa 5 [(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore]),(Rio (-1), [Nenhum, Nenhum, Tronco, Tronco, Tronco])])(Jogador (1,1))))

teste2_8 = TestCase (assertEqual "O jogo termina porque jogador cai no rio" True (jogoTerminou2 (Mapa 5 [(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore]),(Rio (-3), [Tronco, Nenhum, Nenhum, Tronco, Tronco])])(Jogador (2,1))))

teste2_9 = TestCase (assertEqual "O jogo termina porque jogador cai no rio" True (jogoTerminou2 (Mapa 5 [(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore]),(Rio 1, [Nenhum, Tronco, Tronco, Nenhum, Nenhum])])(Jogador (3,1))))

teste2_10 = TestCase (assertEqual "O jogo termina porque jogador cai no rio" True (jogoTerminou2 (Mapa 5 [(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore]),(Rio 2, [Tronco, Tronco, Tronco, Nenhum, Nenhum])])(Jogador (4,1))))

--Teste terceira função

teste3_1 = TestCase (assertEqual "O jogo termina porque o jogador é atropelado" True (jogoTerminou3 (Mapa 5 [(Estrada (-2), [Carro, Nenhum, Nenhum, Nenhum, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore])])(Jogador (0,0))))

teste3_2 = TestCase (assertEqual "O jogo termina porque o jogador é atropelado" True (jogoTerminou3 (Mapa 5 [(Estrada 1, [Carro, Carro, Nenhum, Nenhum, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore])])(Jogador (1,0))))

teste3_3 = TestCase (assertEqual "O jogo termina porque o jogador é atropelado" True (jogoTerminou3 (Mapa 5 [(Estrada 2, [Nenhum, Nenhum, Carro, Nenhum, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore])])(Jogador (2,0))))

teste3_4 = TestCase (assertEqual "O jogo termina porque o jogador é atropelado" True (jogoTerminou3 (Mapa 5 [(Estrada 3, [Nenhum, Nenhum, Carro, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore])])(Jogador (3,0))))

teste3_5 = TestCase (assertEqual "O jogo termina porque o jogador é atropelado" True (jogoTerminou3 (Mapa 5 [(Estrada 1, [Carro, Nenhum, Carro, Nenhum, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore])])(Jogador (4,0))))

teste3_6 = TestCase (assertEqual "O jogo termina porque o jogador é atropelado" True (jogoTerminou3 (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore])(Estrada 1, [Carro, Nenhum, Carro, Nenhum, Carro])])(Jogador (0,1))))

teste3_7 = TestCase (assertEqual "O jogo termina porque o jogador é atropelado" True (jogoTerminou3 (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore]),(Estrada 1, [Carro, Carro, Nenhum, Nenhum, Carro])])(Jogador (1,1))))

teste3_8 = TestCase (assertEqual "O jogo termina porque o jogador é atropelado" True (jogoTerminou3 (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore]),(Estrada 2, [Nenhum, Nenhum, Carro, Nenhum, Carro])])(Jogador (2,1))))

teste3_9 = TestCase (assertEqual "O jogo termina porque o jogador é atropelado" True (jogoTerminou3 (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore]),(Estrada 3, [Nenhum, Nenhum, Carro, Carro, Carro])])(Jogador (3,1))))

teste3_10 = TestCase (assertEqual "O jogo termina porque o jogador é atropelado" True (jogoTerminou3 (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore]),(Estrada 1, [Carro, Nenhum, Carro, Nenhum, Carro])])(Jogador (4,1))))