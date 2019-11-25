{-# LANGUAGE
    DeriveGeneric
#-}

module Machine (
    Letter,
    Action,
    Transition,
    Machine,
) where

import GHC.Generics (Generic)
import Data.Map.Strict (Map, toList)
import Data.Either (isRight)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson (encode, eitherDecode)

type Letter = String
type State = String
type Error = String

data Action = LEFT | RIGHT
    deriving (Show, Generic)

instance FromJSON Action
instance ToJSON Action

data Transition = Transition {
    read        :: Letter,
    to_state    :: State,
    write       :: Letter,
    action      :: Action
} deriving (Show, Generic)

instance FromJSON Transition
instance ToJSON Transition

validTransition :: Machine -> Transition -> Either Error Transition
validTransition machine transition
    | notElem (to_state transition) (states machine) =
        Left "Error: to_state of transition is not in States"
    | notElem (write transition) (alphabet machine) =
        Left "Error: write of transition is not in Alphabet"
    | notElem (Machine.read transition) (alphabet machine) =
        Left "Error: read of transition is not in Alphabet"
validTransition _ transition =
    return transition

validTransitionList :: Machine -> (State, [Transition]) -> Either Error (State, [Transition])
validTransitionList machine (name, transitions)
    | notElem name $ states machine =
        Left "Error: Transition list Name must be in States"
    | not $ all (\x -> isRight x) $ map (validTransition machine) $ transitions =
        Left "Error: Every transition must be valid"
validTransitionList _ (name, transitions) =
    return (name, transitions)

data Machine = Machine {
    name        :: String,
    alphabet    :: [Letter],
    blank       :: Letter,
    states      :: [State],
    initial     :: State,
    finals      :: [State],
    transitions :: Map State [Transition]
} deriving (Show, Generic)

instance FromJSON Machine
instance ToJSON Machine

validMachine :: Machine -> Either Error Machine
validMachine machine
    | not $ all (\s -> length s == 1) $ alphabet machine =
        Left "Error: Every element of Alphabet must be a string of size 1"
    | notElem (blank machine) (alphabet machine) =
        Left "Error: Blank must be a part of Alphabet"
    | notElem (initial machine) (states machine) =
        Left "Error: Initial must be a part of States"
    | not $ all (\x -> elem x $ states machine) $ finals machine =
        Left "Error: Every Final's elements must be a part of States"
validMachine machine
    | not $ all isRight $ map (validTransitionList machine) $ toList (transitions machine) = -- Save the result of `map valid $ transitions machine` to return it ?
        Left "Error: Every transition must be valid"
validMachine machine =
    return machine
