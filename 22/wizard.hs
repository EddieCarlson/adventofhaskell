data Chr = Chr { hp::Int, mana::Int }

data Effect = Effect { hpUp::Int, armUp::Int, armDown::Int, dmg::Int, manaUp::Int }

emptyEffect = Effect 0 0 0 0 0

data Spell = Spell { cost::Int, instant::Effect, effects::[Effect] }

data Game = Game { player::Chr, bossHp::Int, activeEffects::[Effect] }

bossDmg = 8

magicMissile = Spell 53 (Effect 0 0 0 4 0) []
drain = Spell 73 (Effect 2 0 0 2 0) []
shield = Spell 113 emptyEffect [Effect 0 7 0 0 0]
