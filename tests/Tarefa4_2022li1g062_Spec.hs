module Tarefa4_2022li1g062_Spec where

import LI12223
import Tarefa4_2022li1g062
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test ["Teste 1" ~: 1 ~=? 1]

testAll = runTestTT (TestList [test1_1, test1_2, test1_3, test2, test3])

--testex = TestCase (assertEqual "descrição" resultado (f input))

--teste_ = TestCase (assertEqual "O jogo termina porque" r (jogoTerminou ))

--Testes primeira função  

teste1_1 = TestCase (assertEqual "O jogo termina porque o jogador sai do mapa pelo lado esquerdo" True (jogoTerminou1 (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore]), (Estrada 1, [Carro, Nenhum, Carro, Nenhum, Carro])]) (Jogador (-2, 2))))

teste1_2 = TestCase (assertEqual "O jogo termina porque o jogador sai do mapa por baixo" True (jogoTerminou1 (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore]), (Estrada 1, [Carro, Nenhum, Carro, Nenhum, Carro])]) (Jogador (2,-2))))

teste1_3 = TestCase (assertEqual "O jogo termina porque o jogador sai do mapa por estar fora da largura do mapa" True (jogoTerminou1 (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore]),(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore]), (Estrada 1, [Carro, Nenhum, Carro, Nenhum, Carro])]) (Jogador (7,2))))

--Teste segunda função 

teste2_1 = TestCase (assertEqual "O jogo termina porque jogador cai no rio" True (jogoTerminou2 (Mapa 5 [(Rio 2, [Nenhum, Tronco, Tronco, Tronco, Nenhum])(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Arvore])])(Jogador (0,0))))

teste2_2 = TestCase (assertEqual "O jogo termina porque jogador cai no rio" True (jogoTerminou2 (Mapa 5 [(Rio 1, [Nenhum, Nenhum, Tronco, Tronco, Nenhum])(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore])])(Jogador (0,1))))

teste2_3 = TestCase (assertEqual "O jogo termina porque jogador cai no rio" True (jogoTerminou2 (Mapa 5 [(Rio 3, [Tronco, Tronco, Nenhum, Tronco, Nenhum])(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore])])(Jogador (0,2))))

teste2_4 = TestCase (assertEqual "O jogo termina porque jogador cai no rio" True (jogoTerminou2 (Mapa 5 [(Rio 2, [Nenhum, Tronco, Tronco, Nenhum, Nenhum])(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore])])(Jogador (0,4))))

teste2_5 = TestCase (assertEqual "O jogo termina porque jogador cai no rio" True (jogoTerminou2 (Mapa 5 [(Rio 1, [Nenhum, Tronco, Tronco, Nenhum, Nenhum])(Relva, [Arvore, Arvore, Nenhum, Nenhum, Arvore])])(Jogador (0,5))))


teste2_6 = TestCase (assertEqual "O jogo termina porque jogador cai no rio" True (jogoTerminou2 ))

