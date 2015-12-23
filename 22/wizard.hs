import qualified Data.Map as Map

data Chr = Chr { hp::Int, mana::Int }

data Effect = Effect { hpUp::Int, armUp::Int, armDown::Int, dmg::Int, manaUp::Int }

emptyEffect = Effect 0 0 0 0 0

--data Spell = Spell { cost::Int, instant::Effect, effects::[Effect] }
--data Game = Game { player::Chr, bossHp::Int, activeEffects::[Effect] }

data Spell = MagicMissile | Drain | Shield | Poison | Recharge deriving (Show, Eq)

spells = [(MagicMissile,53),(Drain,73),(Shield,113),(Poison,173),(Recharge,229)]

bossDmg = 8

data Game = Game { player::Chr, bossHp::Int, poisonCount::Int, shieldCount::Int, rechargeCount::Int }


cast (spell, cost) (Game (Chr h m) bhp pc sc rc) | spell == MagicMissile = Game (Chr h newMana) (bhp - 4) pc sc rc
                                                 | spell == Drain =        Game (Chr (h + 2) newMana) (bhp - 2) pc sc rc
                                                 | spell == Shield =       Game (Chr h newMana) bhp (pc + 6) sc rc
                                                 | spell == Poison =       Game (Chr h newMana) bhp pc (sc + 6) rc
                                                 | spell == Recharge =     Game (Chr h newMana) bhp pc sc (rc + 6)
  where newMana = m - cost

applyPc g@(Game (Chr h m) bhp pc sc rc) | pc > 0 = Game (Chr h m) (bhp - 3) (pc - 1) sc rc
                                        | otherwise = g

applySc g@(Game (Chr h m) bhp pc sc rc) | sc > 0 = Game (Chr h m) bhp pc (sc - 1) rc
                                        | otherwise = g

applyRc g@(Game (Chr h m) bhp pc sc rc) | rc > 0 = Game (Chr h (m + 101)) bhp pc sc (rc - 1)
                                        | otherwise = g

applyEffects = applyPc . applySc . applyRc

availableSpells g@(Game (Chr _ m) _ _ _ _) = filter ((<= m) . snd) spells

playerTurn g = effected 
  where effected = applyEffects g
