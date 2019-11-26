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
import Data.ByteString.Lazy (ByteString)
import Data.Either (isRight)
import Data.List (intercalate)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson

type Letter = String
type State = String
type Error = String

stack_either :: ([a], [b]) -> Either a b -> ([a], [b])
stack_either (l, r) (Left a) =
    (a : l, r)
stack_either (l, r) (Right b) =
    (l, b : r)

reduce' :: [Either a b] -> Either [a] [b]
reduce' le =
    let (l, r) = foldl stack_either ([], []) le in
    case (l, r) of
        ([], r) -> return r
        (l, _) -> Left l


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
validTransitionList machine (name, transitions) =
    case (reduce' $ map (validTransition machine) $ transitions) of
        Left errors -> Left $ intercalate "\n" errors
        Right _     -> return (name, transitions)

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
validMachine machine =
    case (reduce' $ map (validTransitionList machine) $ toList (transitions machine)) of
        Left errors -> Left $ intercalate "\n" errors
        Right _     -> return machine

encode :: Machine -> ByteString
encode = Aeson.encode

eitherDecode :: ByteString -> Either Error Machine
eitherDecode bs = Aeson.eitherDecode bs >>= validMachine
