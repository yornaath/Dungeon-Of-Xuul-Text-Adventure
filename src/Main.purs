module Main where

import Prelude

import Control.Monad.RWS (RWSResult(..), runRWS)
import Control.Monad.Reader (runReaderT)
import Data.Argonaut.Core as AC
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (split)
import Effect (Effect)
import Effect.AVar as AV
import Effect.Aff (Aff, launchAff, launchAff_, makeAff, nonCanceler, runAff, runAff_)
import Effect.Aff.AVar (read)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (try)
import Game.Game (runGame)
import Game.GameEnvironment (GameEnvironment(..), GameEnvironment)
import Game.GameState (GameState(..), gameStateFromJson)
import Game.Loop.Root (game)
import Lib.AffReadline (question)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Globals (__dirname)
import Node.Path as Path
import Node.ReadLine (Interface)
import Node.ReadLine as RL




-- completer :: String -> Effect { completions ∷ Array String , matched ∷ String }
-- completer input = do
--   pure { completions : ["wooot"], matched : "woo" }

loadGame :: String -> Aff GameState
loadGame save = makeAff \cb -> do
  readSavedStateString <- try $ FS.readTextFile UTF8 (Path.concat [__dirname, "../../.saves", save <> ".json"])
  case readSavedStateString of
    Left error -> do logShow error
    Right savedStateString -> do
      case jsonParser $ savedStateString of 
        Left error -> do
          log ("error :" <> error)
          pure unit
        Right json -> do
          case gameStateFromJson json of
            Left error -> do logShow error
            Right gameState -> do
              log ("loaded game: " <> save)
              cb (Right gameState)
  mempty

saveGame :: String -> GameState -> Aff GameState
saveGame save currentState = makeAff \cb -> do
  let 
    gameStateJson = encodeJson currentState
    gameStateString = AC.stringifyWithIndent 2 gameStateJson
  saved <- try $ FS.writeTextFile UTF8 (Path.concat [__dirname, "../../.saves", save <> ".json"]) gameStateString
  cb (Right currentState)
  mempty


main :: Effect Unit
main = do

  interface <- liftEffect $ RL.createConsoleInterface RL.noCompletion

  let 

    initialState :: GameState
    initialState = (CreatingCharacter { name: Nothing, role: Nothing })

    env :: GameEnvironment
    env = { interface }

    gameLoopRunner :: GameState -> Aff Unit
    gameLoopRunner currentState = do

      line <- interface # question "> "

      let command = (split (wrap " ")) line :: Array String

      case command of
        ["load", save] -> do 
          loadedSaveState <- loadGame save
          gameLoopRunner loadedSaveState
        ["save", save] -> do
          saveState <- saveGame save currentState
          gameLoopRunner saveState
        _ -> do
          newState <- runGame env (game currentState command)
          gameLoopRunner newState

  launchAff_ $ (gameLoopRunner initialState)