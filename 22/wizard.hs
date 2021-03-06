import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Debug.Trace
import Control.Monad.Writer

data Chr = Chr { hp::Int, mana::Int } deriving Show

data Spell = MagicMissile | Drain | Shield | Poison | Recharge deriving (Show, Eq)

spells = [(MagicMissile,53),(Drain,73)]

poisonAndCost = (Poison,173)
shieldAndCost = (Shield,113)
rechargeAndCost = (Recharge,229)

bossDmg = 8
shieldValue = 7

data Game = Game { player::Chr, bossHp::Int, poisonCount::Int, shieldCount::Int, rechargeCount::Int, totalSpent::Int, spellTrace::[Spell] } deriving Show


cast (Game (Chr h m) bhp pc sc rc ts tr) (spell, cost) | spell == MagicMissile = Game (Chr h newMana) (bhp - 4) pc sc rc newSpent newTr
                                                       | spell == Drain =        Game (Chr (h + 2) newMana) (bhp - 2) pc sc rc newSpent newTr
                                                       | spell == Poison =       Game (Chr h newMana) bhp (pc + 6) sc rc newSpent newTr
                                                       | spell == Shield =       Game (Chr h newMana) bhp pc (sc + 6) rc newSpent newTr
                                                       | spell == Recharge =     Game (Chr h newMana) bhp pc sc (rc + 5) newSpent newTr
  where newMana = m - cost
        newSpent = ts + cost
        newTr = spell:tr

applyPc g@(Game (Chr h m) bhp pc sc rc ts tr) | pc > 0 = Game (Chr h m) (bhp - 3) (pc - 1) sc rc ts tr
                                              | otherwise = g

applySc g@(Game (Chr h m) bhp pc sc rc ts tr) | sc > 0 = Game (Chr h m) bhp pc (sc - 1) rc ts tr
                                              | otherwise = g

applyRc g@(Game (Chr h m) bhp pc sc rc ts tr) | rc > 0 = Game (Chr h (m + 101)) bhp pc sc (rc - 1) ts tr
                                              | otherwise = g

applyEffects = applyPc . applySc . applyRc


availableSpells g@(Game (Chr _ m) _ pc sc rc _ _) = filter ((<= m) . snd) spells ++ catMaybes [poison, shield, recharge]
  where poison = if pc == 0 then Just poisonAndCost else Nothing
        shield = if sc == 0 then Just shieldAndCost else Nothing
        recharge = if rc == 0 then Just rechargeAndCost else Nothing

startingGame = Game (Chr 50 500) 55 0 0 0 0 []

--player turn
-- choose lowest mana game
--  advance effects
-- if boss dead, win
-- else choose spell
-- if no available spells, lose
-- cast all available spells with it
-- do boss turns on them
-- add back to queue
playerTurn curGames = 
  if (bossHp game) <= 0 then spellTrace game
  else 
    let newGames = map (cast game) $ availableSpells game
        winningMaybe = fmap spellTrace $ find ((<=0) . bossHp) newGames 
        bossGames = filter ((>0) . hp . player) $ map bossTurn newGames
    in fromMaybe (playerTurn (tail sortedCurGames ++ bossGames)) winningMaybe
  where game = applyEffects $ head $ sortedCurGames
        sortedCurGames = sortOn totalSpent curGames


--boss turn
-- advance effects
-- if alive, attack. if dead, win

bossTurn g = if bossHp afterEffects > 0 then bossAttack afterEffects else afterEffects
  where afterEffects = applyEffects g
 
bossAttack (Game (Chr h m) bhp pc sc rc ts tr) = Game (Chr (h - dmg) m) bhp pc sc rc ts tr
  where dmg = if sc > 0 then max (bossDmg - shieldValue) 1 else bossDmg


-- do all of same mana cost at once

playerTurn2 :: [Game] -> Game
playerTurn2 curGames = 
  if (isJust $ winMaybe curGames) then (fromJust $ winMaybe curGames)
  else 
    let newGames = trace (show $ totalSpent headGame) $ concatMap (\g -> map (cast g) (availableSpells g)) lowestManaGames
        winningMaybe = winMaybe newGames 
        bossGames = filter ((>0) . hp . player) $ map bossTurn newGames
    in fromMaybe (playerTurn2 (rest ++ bossGames)) winningMaybe
  where headGame = head sortedCurGames
        (lowestManaGames, rest) = span (\x -> (totalSpent x) == (totalSpent headGame)) sortedCurGames
        sortedCurGames = sortOn totalSpent curGames
        winMaybe = find ((<=0) . bossHp)

