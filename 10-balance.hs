import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.List
import Text.Regex.Posix

data Chip = Chip { chipId :: Int } deriving (Eq, Show, Ord)
data Bot = Bot
  { botId :: Int
  , chips :: [Chip]
  , outputs :: [Out]
  } deriving Show
data Output = Output
  { outputId :: Int
  , chip :: (Maybe Chip)
  } deriving Show

data Instruction = Goes
  { chipValue :: Int
  , to :: Out
  }
  | Gives
  { fromBotId  :: Int
  , loTo :: Out
  , hiTo :: Out
  } deriving Show
data Out = ToOutput { toOutputId :: Int } | ToBot { toBotId :: Int } deriving Show

type Bots = Map.Map Int Bot
type Outputs = Map.Map Int Output

addBots :: Bots -> Instruction -> Bots
addBots bots (Goes _ (ToBot botId)) = addBot bots botId
addBots bots (Gives fromBotId outLo outHi) = addOut outLo $ addOut outHi $ addBot bots fromBotId
  where addOut (ToBot botId) bots = addBot bots botId
        addOut _             bots = bots

addBot :: Bots -> Int -> Bots
addBot bots botId
  | Map.member botId bots = bots
  | otherwise             = Map.insert botId (Bot botId [] []) bots

instruct bots (Goes chipId (ToBot botId)) = Map.adjust giveChip botId bots
  where giveChip bot = bot { chips = (Chip chipId) : (chips bot) }
instruct bots (Gives fromBot outLo outHi) = Map.adjust instructBot fromBot bots
  where instructBot bot = bot { outputs = [outLo, outHi] }

addOutputs outputs (Gives _ outLo outHi) = addOut outLo $ addOut outHi outputs
  where addOut (ToOutput outputId) outputs = addOutput outputs outputId
        addOut _                   outputs = outputs
addOutputs outputs _ = outputs
addOutput outputs outputId
  | Map.member outputId outputs = outputs
  | otherwise                   = Map.insert outputId (Output outputId Nothing) outputs

simulate :: Bots -> Outputs -> (Bots, Outputs)
simulate bots outputs
  | ready     = simulate botsAfter outputsAfter
  | otherwise = (bots, outputs)
  where
    ready = Map.foldl orBotRead False bots
    orBotRead b bot = b || botReady bot
    botReady = ((<) 1 . length . chips)
    (botsAfter, outputsAfter) = foldl give (bots, outputs) $ Map.filter botReady bots

give :: (Bots, Outputs) -> Bot -> (Bots, Outputs)
-- give _ (Bot botId chips _) | elem (Chip 61) chips && elem (Chip 17) chips = error $ show botId
give (bots, outputs) (Bot botId toOut outs) = foldl giveOut (Map.adjust without botId bots, outputs) outputsWithChips
  where
    outputsWithChips = zip outs $ sort toOut
    without bot = bot { chips = [] }
    giveOut (bots, beforeOuts) ((ToOutput outputId), chip) = (bots, Map.adjust (giveOutput chip) outputId beforeOuts)
    giveOut (beforeBots, outs) ((ToBot botId), chip) = (Map.adjust (giveBot chip) botId beforeBots, outs)
    giveBot chip bot = bot { chips = chip : (chips bot)}
    giveOutput chip out = out { chip = Just chip }

type Match = (String,String,String,[String])

parseInstruction :: String -> Instruction
parseInstruction line
  | isJust goes = let
    (_, _, _, [chip, bot]) = fromJust goes
    in Goes (read chip) (ToBot (read bot))
  | isJust gives = let
    (_, _, _, [fromBot, loTo, loToId, hiTo, hiToId]) = fromJust gives
    in Gives (read fromBot) (to loTo loToId) (to hiTo hiToId)
  | otherwise = error line
  where
    goes = line =~~ "value ([0-9]+) goes to bot ([0-9]+)" :: Maybe Match
    gives = line =~~ "bot ([0-9]+) gives low to (bot|output) ([0-9]+) and high to (bot|output) ([0-9]+)" :: Maybe Match
    to "bot" id = ToBot (read id)
    to "output" id = ToOutput (read id)

solve = do
  input <- readFile "10-input.txt"
  let
    instructions = map parseInstruction $ lines input
    initBots = foldl addBots Map.empty instructions
    instructed = foldl instruct initBots instructions
    outputs = foldl addOutputs Map.empty instructions
    (bots, outs) = simulate instructed outputs
  return $ product $ map (chipId . fromJust . chip . snd) $ take 3 $ Map.toList outs
