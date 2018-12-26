module NameConversionData where

import qualified DeBruijnLambdas as Unnamed
import           LambdaInfo
import qualified NamedLambdas    as Named

namedLoop :: Named.Lambda LambdaInfo
namedLoop = Named.Application loopBase loopBase where
  loopBase = Named.Abstraction "x" $ Named.Application var var where
    var = Named.Variable "x"

unnamedLoop :: Unnamed.Lambda LambdaInfo
unnamedLoop = Unnamed.Application loopBase loopBase where
  loopBase = Unnamed.Abstraction $ Unnamed.Application var var where
    var = Unnamed.Variable 0

namedTrueLambda :: Named.Lambda LambdaInfo
namedTrueLambda = Named.Abstraction "x" $ Named.Abstraction "y" $ Named.Variable "x"

unnamedTrueLambda :: Unnamed.Lambda LambdaInfo
unnamedTrueLambda = Unnamed.Abstraction $ Unnamed.Abstraction $ Unnamed.Variable 1

namedFalseLambda :: Named.Lambda LambdaInfo
namedFalseLambda = Named.Abstraction "x" $ Named.Abstraction "y" $ Named.Variable "y"

unnamedFalseLambda :: Unnamed.Lambda LambdaInfo
unnamedFalseLambda = Unnamed.Abstraction $ Unnamed.Abstraction $ Unnamed.Variable 0

-- if :: boolean -> thenvalue -> elsevalue -> result
-- if = λa.λb.λc.a b c
--    = λa.(λb.(λc.(a b) c))
namedIfLambda :: Named.Lambda LambdaInfo
namedIfLambda =
  Named.Abstraction "a" $
  Named.Abstraction "b" $
  Named.Abstraction "c" $
  Named.Application
  (Named.Application
    (Named.Variable "a")
    (Named.Variable "b")
  )
  (Named.Variable "c")

unnamedIfLambda :: Unnamed.Lambda LambdaInfo
unnamedIfLambda =
  Unnamed.Abstraction $
  Unnamed.Abstraction $
  Unnamed.Abstraction $
  Unnamed.Application
  (Unnamed.Application
    (Unnamed.Variable 2)
    (Unnamed.Variable 1)
  )
  (Unnamed.Variable 0)
