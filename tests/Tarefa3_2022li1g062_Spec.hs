module Tarefa3_2022li1g062_Spec where

import LI12223
import Tarefa3_2022li1g062
import Test.HUnit

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test ["Teste 1" ~: 1 ~=? 1]
