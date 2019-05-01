module Interpretation where

import SemanticDomain

newtype Interpretation v = Interpretation { runInterpretation :: [(v, SemanticDomain)] }
