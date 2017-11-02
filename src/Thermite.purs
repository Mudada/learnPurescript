module Main where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import React as R
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T
import Unsafe.Coerce
import Data.List
import Data.Tuple
import Data.Either
import Data.Maybe
import Data.Lens

data InputAction = ChangeState String
data Action = Action Int InputAction

type InputState = String
type State = List InputState

_InputAction :: Prism' Action (Tuple Int InputAction)
_InputAction = prism' (uncurry Action) unwrap
    where unwrap (Action i a) = Just (Tuple i a)
          unwrap _ = Nothing

initialInputState :: InputState
initialInputState = ""

initialState :: List InputState
initialState = flip snoc initialInputState $ singleton initialInputState

performInputAction :: T.PerformAction _ InputState _ InputAction
performInputAction (ChangeState s) _ state = void $ T.modifyState $ \state -> s

renderInput :: T.Render InputState _ InputAction
renderInput dispatch _ state _ = 
    [ R.text state,
      R.br' [],
      R.input [ RP.onChange \e -> dispatch $ ChangeState (unsafeCoerce e).target.value ]
              []
    ]

inputSpec :: T.Spec _ InputState _ InputAction
inputSpec = T.simpleSpec performInputAction renderInput

spec :: T.Spec _ State _ Action
spec = T.focus id _InputAction (T.foreach \_ -> inputSpec)

main :: Eff (dom :: DOM) Unit
main = T.defaultMain spec initialState unit
