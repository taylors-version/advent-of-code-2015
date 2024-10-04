import Day22._
import org.scalatest.funsuite.AnyFunSuite


class Day22Test extends AnyFunSuite{
    val emptyEffects: Map[Spell, Int] = Map()
    val baseState: State = State(58, 9, 50, 500, 0, emptyEffects, 0, List.empty)
    val baseMagicMissile: State = baseState.copy(bossHp = 54, hp = 41, mana = 447, manaSpent = 53, path = List[Spell](MagicMissile))
    val baseDrain: State = baseState.copy(bossHp = 56, hp = 43, mana = 427, manaSpent = 73, path = List[Spell](Drain))
    val baseShield: State = baseState.copy(hp = 48, mana = 387, manaSpent = 113, effects = Map[Spell, Int]((Shield, 5)), path = List[Spell](Shield))
    val basePoison: State = baseState.copy(bossHp = 55, hp = 41, mana = 327, manaSpent = 173, effects = Map[Spell, Int]((Poison, 5)), path = List[Spell](Poison))
    val baseRecharge: State = baseState.copy(hp = 41, mana = 372, manaSpent = 229, effects = Map[Spell, Int]((Recharge, 4)), path = List[Spell](Recharge))

    test("Magic Missile with no effects") {
        val expectedPlayerState = baseState.copy(bossHp = 54, mana = 447, manaSpent = 53, path = List[Spell](MagicMissile))
        assert(baseState.personTurn(MagicMissile) == expectedPlayerState)
        assert(expectedPlayerState.bossTurn() == baseMagicMissile)
    }

    test("Drain with no effects") {
        val expectedPlayerState = baseState.copy(bossHp = 56, hp = 52, mana = 427, manaSpent = 73, path = List[Spell](Drain))
        assert(baseState.personTurn(Drain) == expectedPlayerState)
        assert(expectedPlayerState.bossTurn() == baseDrain)
    }

    test("Shield with no effects") {
        val expectedPlayerState = baseState.copy(bossHp = 58, mana = 387, manaSpent = 113, effects = Map[Spell, Int]((Shield, 6)) ,path = List[Spell](Shield))
        assert(baseState.personTurn(Shield) == expectedPlayerState)
        assert(expectedPlayerState.bossTurn() == baseShield)
    }

    test("Poison with no effects") {
        val expectedPlayerState = baseState.copy(bossHp = 58, mana = 327, manaSpent = 173, effects = Map[Spell, Int]((Poison, 6)), path = List[Spell](Poison))
        assert(baseState.personTurn(Poison) == expectedPlayerState)
        assert(expectedPlayerState.bossTurn() == basePoison)
    }

    test("Recharge with no effects") {
        val expectedPlayerState = baseState.copy(bossHp = 58, mana = 271, manaSpent = 229, effects = Map[Spell, Int]((Recharge, 5)), path = List[Spell](Recharge))
        assert(baseState.personTurn(Recharge) == expectedPlayerState)
        assert(expectedPlayerState.bossTurn() == baseRecharge)
    }

    test("Magic Missile with Shield") {
        val expectedPlayerState = baseShield.copy(bossHp = 54, mana = 334, manaSpent = 166, armor = 7, effects = Map[Spell, Int]((Shield, 4)), path = List[Spell](Shield, MagicMissile))
        val expectedBossState = expectedPlayerState.copy(hp = 46, armor = 0, effects = Map[Spell, Int]((Shield, 3)))
        assert(baseShield.personTurn(MagicMissile) == expectedPlayerState, "player turn error")
        assert(expectedPlayerState.bossTurn() == expectedBossState, "boss turn error")
    }

    test("Drain with Shield") {
        val expectedPlayerState = baseShield.copy(bossHp = 56, hp = 50, mana = 314, manaSpent = 186, armor = 7, effects = Map[Spell, Int]((Shield, 4)), path = List[Spell](Shield, Drain))
        val expectedBossState = expectedPlayerState.copy(hp = 48, armor = 0, effects = Map[Spell, Int]((Shield, 3)))
        assert(baseShield.personTurn(Drain) == expectedPlayerState, "player turn error")
        assert(expectedPlayerState.bossTurn() == expectedBossState, "boss turn error")
    }

    test("Poison with Shield") {
        val expectedPlayerState = baseShield.copy(mana = baseShield.mana - 173, manaSpent = baseShield.manaSpent + 173, armor = 7, effects = Map[Spell, Int]((Shield, 4), (Poison, 6)), path = List[Spell](Shield, Poison))
        val expectedBossState = expectedPlayerState.copy(bossHp = expectedPlayerState.bossHp - 3, hp = 46, armor = 0, effects = Map[Spell, Int]((Shield, 3), (Poison, 5)))
        assert(baseShield.personTurn(Poison) == expectedPlayerState, "player turn error")
        assert(expectedPlayerState.bossTurn() == expectedBossState, "boss turn error")
    }

    test("Recharge with Shield") {
        val expectedPlayerState = baseShield.copy(mana = baseShield.mana - 229, manaSpent = baseShield.manaSpent + 229, armor = 7, effects = Map[Spell, Int]((Shield, 4), (Recharge, 5)), path = List[Spell](Shield, Recharge))
        val expectedBossState = expectedPlayerState.copy(hp = 46, mana = expectedPlayerState.mana + 101, armor = 0, effects = Map[Spell, Int]((Shield, 3), (Recharge, 4)))
        assert(baseShield.personTurn(Recharge) == expectedPlayerState, "player turn error")
        assert(expectedPlayerState.bossTurn() == expectedBossState, "boss turn error")
    }

    test("Magic Missile with Poison") {
        val expectedPlayerState = basePoison.copy(bossHp = basePoison.bossHp - 7, mana = basePoison.mana - 53, manaSpent = basePoison.manaSpent + 53, effects = Map[Spell, Int]((Poison, 4)), path = List[Spell](Poison, MagicMissile))
        val expectedBossState = expectedPlayerState.copy(bossHp = expectedPlayerState.bossHp - 3, hp = expectedPlayerState.hp - 9, effects = Map[Spell, Int]((Poison, 3)))
        assert(basePoison.personTurn(MagicMissile) == expectedPlayerState, "player turn error")
        assert(expectedPlayerState.bossTurn() == expectedBossState, "boss turn error")
    }

    test("Drain with Poison") {
        val expectedPlayerState = basePoison.copy(bossHp = basePoison.bossHp - 5, hp = basePoison.hp + 2, mana = basePoison.mana - 73, manaSpent = basePoison.manaSpent + 73, effects = Map[Spell, Int]((Poison, 4)), path = List[Spell](Poison, Drain))
        val expectedBossState = expectedPlayerState.copy(bossHp = expectedPlayerState.bossHp - 3, hp = expectedPlayerState.hp - 9, effects = Map[Spell, Int]((Poison, 3)))
        assert(basePoison.personTurn(Drain) == expectedPlayerState, "player turn error")
        assert(expectedPlayerState.bossTurn() == expectedBossState, "boss turn error")
    }

    test("Shield with Poison") {
        val expectedPlayerState = basePoison.copy(bossHp = 52, mana = 214, manaSpent = 286, effects = Map[Spell, Int]((Poison, 4), (Shield, 6)), path = List[Spell](Poison, Shield))
        val expectedBossState = expectedPlayerState.copy(bossHp = 49, hp = 39, effects = Map[Spell, Int]((Poison, 3), (Shield, 5)))
        assert(basePoison.personTurn(Shield) == expectedPlayerState, "player turn error")
        assert(expectedPlayerState.bossTurn() == expectedBossState, "boss turn error")
    }

    test("Recharge with Poison") {
        val expectedPlayerState = basePoison.copy(bossHp = basePoison.bossHp - 3, mana = basePoison.mana - 229, manaSpent = basePoison.manaSpent + 229, effects = Map[Spell, Int]((Poison, 4), (Recharge, 5)), path = List[Spell](Poison, Recharge))
        val expectedBossState = expectedPlayerState.copy(bossHp = expectedPlayerState.bossHp - 3, hp = expectedPlayerState.hp - 9, mana = expectedPlayerState.mana + 101, effects = Map[Spell, Int]((Poison, 3), (Recharge, 4)))
        assert(basePoison.personTurn(Recharge) == expectedPlayerState, "player turn error")
        assert(expectedPlayerState.bossTurn() == expectedBossState, "boss turn error")
    }

    test("Magic Missile with Recharge") {
        val expectedPlayerState = baseRecharge.copy(bossHp = baseRecharge.bossHp - 4, mana = baseRecharge.mana + 48, manaSpent = baseRecharge.manaSpent + 53, effects = Map[Spell, Int]((Recharge, 3)), path = List[Spell](Recharge, MagicMissile))
        val expectedBossState = expectedPlayerState.copy(hp = expectedPlayerState.hp - 9, mana = expectedPlayerState.mana + 101, effects = Map[Spell, Int]((Recharge, 2)))
        assert(baseRecharge.personTurn(MagicMissile) == expectedPlayerState, "player turn error")
        assert(expectedPlayerState.bossTurn() == expectedBossState, "boss turn error")
    }

    test("Drain with Recharge") {
        val expectedPlayerState = baseRecharge.copy(bossHp = baseRecharge.bossHp - 2, hp = baseRecharge.hp + 2, mana = baseRecharge.mana + 28, manaSpent = baseRecharge.manaSpent + 73, effects = Map[Spell, Int]((Recharge, 3)), path = List[Spell](Recharge, Drain))
        val expectedBossState = expectedPlayerState.copy(hp = expectedPlayerState.hp - 9, mana = expectedPlayerState.mana + 101, effects = Map[Spell, Int]((Recharge, 2)))
        assert(baseRecharge.personTurn(Drain) == expectedPlayerState, "player turn error")
        assert(expectedPlayerState.bossTurn() == expectedBossState, "boss turn error")
    }

    test("Shield with Recharge") {
        val expectedPlayerState = baseRecharge.copy(mana = baseRecharge.mana - 12, manaSpent = baseRecharge.manaSpent + 113, effects = Map[Spell, Int]((Recharge, 3), (Shield, 6)), path = List[Spell](Recharge, Shield))
        val expectedBossState = expectedPlayerState.copy(hp = expectedPlayerState.hp - 2, mana = expectedPlayerState.mana + 101, effects = Map[Spell, Int]((Recharge, 2), (Shield, 5)))
        assert(baseRecharge.personTurn(Shield) == expectedPlayerState, "player turn error")
        assert(expectedPlayerState.bossTurn() == expectedBossState, "boss turn error")
    }

    test("Poison with Recharge") {
        val expectedPlayerState = baseRecharge.copy(mana = baseRecharge.mana - 72, manaSpent = baseRecharge.manaSpent + 173, effects = Map[Spell, Int]((Recharge, 3), (Poison, 6)), path = List[Spell](Recharge, Poison))
        val expectedBossState = expectedPlayerState.copy(bossHp = expectedPlayerState.bossHp - 3, hp = expectedPlayerState.hp - 9, mana = expectedPlayerState.mana + 101, effects = Map[Spell, Int]((Recharge, 2), (Poison, 5)))
        assert(baseRecharge.personTurn(Poison) == expectedPlayerState, "player turn error")
        assert(expectedPlayerState.bossTurn() == expectedBossState, "boss turn error")
    }

    test("Next from empty") {
        val startState: State = State(58, 9, 50, 500, 0, Map.empty, 0, List.empty)
        val expectedNeighbours: Seq[State] = Seq(baseMagicMissile, baseDrain, baseShield, basePoison, baseRecharge)
        val next = startState.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from MagicMissile") {
        val afterMagicMissile = baseMagicMissile.personTurn(MagicMissile).bossTurn()
        val afterDrain = baseMagicMissile.personTurn(Drain).bossTurn()
        val afterShield = baseMagicMissile.personTurn(Shield).bossTurn()
        val afterPoison = baseMagicMissile.personTurn(Poison).bossTurn()
        val afterRecharge = baseMagicMissile.personTurn(Recharge).bossTurn()
        val expectedNeighbours: Seq[State] = Seq(afterMagicMissile, afterDrain, afterShield, afterPoison, afterRecharge)
        val next = baseMagicMissile.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from Drain") {
        val afterMagicMissile = baseDrain.personTurn(MagicMissile).bossTurn()
        val afterDrain = baseDrain.personTurn(Drain).bossTurn()
        val afterShield = baseDrain.personTurn(Shield).bossTurn()
        val afterPoison = baseDrain.personTurn(Poison).bossTurn()
        val afterRecharge = baseDrain.personTurn(Recharge).bossTurn()
        val expectedNeighbours: Seq[State] = Seq(afterMagicMissile, afterDrain, afterShield, afterPoison, afterRecharge)
        val next = baseDrain.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from Shield") {
        val afterMagicMissile = baseShield.personTurn(MagicMissile).bossTurn()
        val afterDrain = baseShield.personTurn(Drain).bossTurn()
        val afterPoison = baseShield.personTurn(Poison).bossTurn()
        val afterRecharge = baseShield.personTurn(Recharge).bossTurn()
        val expectedNeighbours: Seq[State] = Seq(afterMagicMissile, afterDrain, afterPoison, afterRecharge)
        val next = baseShield.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from Poison") {
        val afterMagicMissile = basePoison.personTurn(MagicMissile).bossTurn()
        val afterDrain = basePoison.personTurn(Drain).bossTurn()
        val afterShield = basePoison.personTurn(Shield).bossTurn()
        val afterRecharge = basePoison.personTurn(Recharge).bossTurn()
        val expectedNeighbours: Seq[State] = Seq(afterMagicMissile, afterDrain, afterShield, afterRecharge)
        val next = basePoison.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from Recharge") {
        val afterMagicMissile = baseRecharge.personTurn(MagicMissile).bossTurn()
        val afterDrain = baseRecharge.personTurn(Drain).bossTurn()
        val afterShield = baseRecharge.personTurn(Shield).bossTurn()
        val afterPoison = baseRecharge.personTurn(Poison).bossTurn()
        val expectedNeighbours: Seq[State] = Seq(afterMagicMissile, afterDrain, afterShield, afterPoison)
        val next = baseRecharge.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from MagicMissile, MagicMissile") {
        val magicMissileMagicMissile = baseMagicMissile.personTurn(MagicMissile).bossTurn()
        val afterMagicMissile = magicMissileMagicMissile.personTurn(MagicMissile).bossTurn()
        val afterDrain = magicMissileMagicMissile.personTurn(Drain).bossTurn()
        val afterShield = magicMissileMagicMissile.personTurn(Shield).bossTurn()
        val afterPoison = magicMissileMagicMissile.personTurn(Poison).bossTurn()
        val afterRecharge = magicMissileMagicMissile.personTurn(Recharge).bossTurn()
        val expectedNeighbours: Seq[State] = Seq(afterMagicMissile, afterDrain, afterShield, afterPoison, afterRecharge)
        val next = magicMissileMagicMissile.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from MagicMissile, Drain") {
        val magicMissileDrain = baseMagicMissile.personTurn(Drain).bossTurn()
        val afterMagicMissile = magicMissileDrain.personTurn(MagicMissile).bossTurn()
        val afterDrain = magicMissileDrain.personTurn(Drain).bossTurn()
        val afterShield = magicMissileDrain.personTurn(Shield).bossTurn()
        val afterPoison = magicMissileDrain.personTurn(Poison).bossTurn()
        val afterRecharge = magicMissileDrain.personTurn(Recharge).bossTurn()
        val expectedNeighbours: Seq[State] = Seq(afterMagicMissile, afterDrain, afterShield, afterPoison, afterRecharge)
        val next = magicMissileDrain.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from MagicMissile, Shield") {
        val magicMissileShield = baseMagicMissile.personTurn(Shield).bossTurn()
        val afterMagicMissile = magicMissileShield.personTurn(MagicMissile).bossTurn()
        val afterDrain = magicMissileShield.personTurn(Drain).bossTurn()
        val afterPoison = magicMissileShield.personTurn(Poison).bossTurn()
        val afterRecharge = magicMissileShield.personTurn(Recharge).bossTurn()
        val expectedNeighbours: Seq[State] = Seq(afterMagicMissile, afterDrain, afterPoison, afterRecharge)
        val next = magicMissileShield.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from MagicMissile, Poison") {
        val magicMissilePoison = baseMagicMissile.personTurn(Poison).bossTurn()
        val afterMagicMissile = magicMissilePoison.personTurn(MagicMissile).bossTurn()
        val afterDrain = magicMissilePoison.personTurn(Drain).bossTurn()
        val afterShield = magicMissilePoison.personTurn(Shield).bossTurn()
        val afterRecharge = magicMissilePoison.personTurn(Recharge).bossTurn()
        val expectedNeighbours: Seq[State] = Seq(afterMagicMissile, afterDrain, afterShield, afterRecharge)
        val next = magicMissilePoison.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from MagicMissile, Recharge") {
        val magicMissileRecharge = baseMagicMissile.personTurn(Recharge).bossTurn()
        val afterMagicMissile = magicMissileRecharge.personTurn(MagicMissile).bossTurn()
        val afterDrain = magicMissileRecharge.personTurn(Drain).bossTurn()
        val afterShield = magicMissileRecharge.personTurn(Shield).bossTurn()
        val afterPoison = magicMissileRecharge.personTurn(Poison).bossTurn()
        val expectedNeighbours: Seq[State] = Seq(afterMagicMissile, afterDrain, afterShield, afterPoison)
        val next = magicMissileRecharge.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from Recharge, Poison") {
        val rechargePoison = baseRecharge.personTurn(Poison).bossTurn()
        val afterMagicMissile = rechargePoison.personTurn(MagicMissile).bossTurn()
        val afterDrain = rechargePoison.personTurn(Drain).bossTurn()
        val afterShield = rechargePoison.personTurn(Shield).bossTurn()
        val expectedNeighbours: Seq[State] = Seq(afterMagicMissile, afterDrain, afterShield)
        val next = rechargePoison.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from Recharge, MagicMissile, MagicMissile") {
        val rechargeMagicMissileMagicMissile = baseRecharge.personTurn(MagicMissile).bossTurn().personTurn(MagicMissile).bossTurn()
        val afterMagicMissile = rechargeMagicMissileMagicMissile.personTurn(MagicMissile).bossTurn()
        val afterDrain = rechargeMagicMissileMagicMissile.personTurn(Drain).bossTurn()
        val afterShield = rechargeMagicMissileMagicMissile.personTurn(Shield).bossTurn()
        val afterPoison = rechargeMagicMissileMagicMissile.personTurn(Poison).bossTurn()
        val afterRecharge = rechargeMagicMissileMagicMissile.personTurn(Recharge).bossTurn()
        val expectedNeighbours: Seq[State] = Seq(afterMagicMissile, afterDrain, afterShield, afterPoison, afterRecharge)
        val next = rechargeMagicMissileMagicMissile.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from P, R") {
        val pr = basePoison.copy(bossHp = basePoison.bossHp - 6, hp = basePoison.hp - 9, mana = basePoison.mana - 128, manaSpent = basePoison.manaSpent + 229, effects = Map((Poison, 3), (Recharge, 4)), path = List(Poison, Recharge))
        val afterMM = pr.copy(bossHp = pr.bossHp - 10, hp = pr.hp - 9, mana = pr.mana + 149, manaSpent = pr.manaSpent + 53, effects = Map((Poison, 1), (Recharge, 2)), path = List(Poison, Recharge, MagicMissile))
        val afterD = pr.copy(bossHp = pr.bossHp - 8, hp = pr.hp - 7, mana = pr.mana + 129, manaSpent = pr.manaSpent + 73, effects = Map((Poison, 1), (Recharge, 2)), path = List(Poison, Recharge, Drain))
        val afterS = pr.copy(bossHp = pr.bossHp - 6, hp = pr.hp - 2, mana = pr.mana + 89, manaSpent = pr.manaSpent + 113, effects = Map((Poison, 1), (Recharge, 2), (Shield, 5)), path = List(Poison, Recharge, Shield))

        val expectedNeighbours: Seq[State] = Seq(afterMM, afterD, afterS)
        val next = pr.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from P, R, M") {
        val prm = State(39,9,23,348,0,Map((Poison, 1), (Recharge, 2)),455,List(Poison, Recharge, MagicMissile))
        val afterMM = prm.copy(bossHp = prm.bossHp - 7, hp = prm.hp - 9, mana = prm.mana + 149, manaSpent = prm.manaSpent + 53, effects = Map(), path = List(Poison, Recharge, MagicMissile, MagicMissile))
        val afterD = prm.copy(bossHp = prm.bossHp - 5, hp = prm.hp - 7, mana = prm.mana + 129, manaSpent = prm.manaSpent + 73, effects = Map(), path = List(Poison, Recharge, MagicMissile, Drain))
        val afterS = prm.copy(bossHp = prm.bossHp - 3, hp = prm.hp - 2, mana = prm.mana + 89, manaSpent = prm.manaSpent + 113, effects = Map((Shield, 5)), path = List(Poison, Recharge, MagicMissile, Shield))
        val afterP = prm.copy(bossHp = prm.bossHp - 6, hp = prm.hp - 9, mana = prm.mana + 29, manaSpent = prm.manaSpent + 173, effects = Map((Poison, 5)), path = List(Poison, Recharge, MagicMissile, Poison))

        val expectedNeighbours: Seq[State] = Seq(afterMM, afterD, afterS, afterP)
        val next = prm.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from P, R, M, P") {
        val prmp = State(33,9,14,377,0,Map((Poison, 5)),628,List(Poison, Recharge, MagicMissile, Poison))
        val afterMM = prmp.copy(bossHp = prmp.bossHp - 10, hp = prmp.hp - 9, mana = prmp.mana - 53, manaSpent = prmp.manaSpent + 53, effects = Map((Poison, 3)), path = List(Poison, Recharge, MagicMissile, Poison, MagicMissile))
        val afterD = prmp.copy(bossHp = prmp.bossHp - 8, hp = prmp.hp - 7, mana = prmp.mana - 73, manaSpent = prmp.manaSpent + 73, effects = Map((Poison, 3)), path = List(Poison, Recharge, MagicMissile, Poison, Drain))
        val afterS = prmp.copy(bossHp = prmp.bossHp - 6, hp = prmp.hp - 2, mana = prmp.mana - 113, manaSpent = prmp.manaSpent + 113, effects = Map((Poison, 3), (Shield, 5)), path = List(Poison, Recharge, MagicMissile, Poison, Shield))
        val afterR = prmp.copy(bossHp = prmp.bossHp - 6, hp = prmp.hp - 9, mana = prmp.mana - 128, manaSpent = prmp.manaSpent + 229, effects = Map((Poison, 3), (Recharge, 4)), path = List(Poison, Recharge, MagicMissile, Poison, Recharge))

        val expectedNeighbours: Seq[State] = Seq(afterMM, afterD, afterS, afterR)
        val next = prmp.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from P, R, M, P, R") {
        val prmpr = State(27,9,5,249,0,Map((Poison, 3), (Recharge, 4)),857,List(Poison, Recharge, MagicMissile, Poison, Recharge))
        val afterMM = prmpr.copy(bossHp = prmpr.bossHp - 10, hp = prmpr.hp - 9, mana = prmpr.mana + 149, manaSpent = prmpr.manaSpent + 53, effects = Map((Poison, 1), (Recharge, 2)), path = List(Poison, Recharge, MagicMissile, Poison, Recharge, MagicMissile))
        val afterD = prmpr.copy(bossHp = prmpr.bossHp - 8, hp = prmpr.hp - 7, mana = prmpr.mana +129, manaSpent = prmpr.manaSpent + 73, effects = Map((Poison, 1), (Recharge, 2)), path = List(Poison, Recharge, MagicMissile, Poison, Recharge, Drain))
        val afterS = prmpr.copy(bossHp = prmpr.bossHp - 6, hp = prmpr.hp - 2, mana = prmpr.mana +89, manaSpent = prmpr.manaSpent + 113, effects = Map((Poison, 1), (Recharge, 2), (Shield, 5)), path = List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield))

        val expectedNeighbours: Seq[State] = Seq(afterMM, afterD, afterS)
        val next = prmpr.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from P, R, M, P, R, S") {
        val prmprs = State(21,9,3,338,0,Map((Poison, 1), (Recharge, 2), (Shield, 5)),970,List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield))
        val afterMM = prmprs.copy(bossHp = prmprs.bossHp - 7, hp = prmprs.hp - 2, mana = prmprs.mana + 149, manaSpent = prmprs.manaSpent + 53, effects = Map((Shield, 3)), path = List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield, MagicMissile))
        val afterD = prmprs.copy(bossHp = prmprs.bossHp - 5, hp = prmprs.hp, mana = prmprs.mana + 129, manaSpent = prmprs.manaSpent + 73, effects = Map((Shield, 3)), path = List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield, Drain))
        val afterP = prmprs.copy(bossHp = prmprs.bossHp - 6, hp = prmprs.hp - 2, mana = prmprs.mana + 29, manaSpent = prmprs.manaSpent + 173, effects = Map((Shield, 3), (Poison, 5)), path = List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield, Poison))

        val expectedNeighbours: Seq[State] = Seq(afterMM, afterD, afterP)
        val next = prmprs.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from P, R, M, P, R, S, P") {
        val prmprsp = State(15,9,1,367,0,Map((Shield, 3), (Poison, 5)),1143,List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield, Poison))
        val afterMM = prmprsp.copy(bossHp = prmprsp.bossHp - 10, hp = prmprsp.hp - 2, mana = prmprsp.mana - 53, manaSpent = prmprsp.manaSpent + 53, effects = Map((Shield, 1), (Poison, 3)), path = List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield, Poison, MagicMissile))
        val afterD = prmprsp.copy(bossHp = prmprsp.bossHp - 8, hp = prmprsp.hp, mana = prmprsp.mana - 73, manaSpent = prmprsp.manaSpent + 73, effects = Map((Shield, 1), (Poison, 3)), path = List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield, Poison, Drain))
        val afterR = prmprsp.copy(bossHp = prmprsp.bossHp - 6, hp = prmprsp.hp - 2, mana = prmprsp.mana - 128, manaSpent = prmprsp.manaSpent + 229, effects = Map((Shield, 1), (Poison, 3), (Recharge, 4)), path = List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield, Poison, Recharge))

        val expectedNeighbours: Seq[State] = Seq(afterMM, afterD, afterR)
        val next = prmprsp.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

    test("Next from P, R, M, P, R, S, P, D") { //M finishes for 1269
        val prmprspd = State(7,9,1,294,0,Map((Shield, 1), (Poison, 3)),1216,List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield, Poison, Drain))
        val afterMM = prmprspd.copy(bossHp = prmprspd.bossHp - 10, hp = prmprspd.hp - 9, mana = prmprspd.mana - 53, manaSpent = prmprspd.manaSpent + 53, effects = Map((Poison, 1)), path = List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield, Poison, Drain, MagicMissile))
        val afterD = prmprspd.copy(bossHp = prmprspd.bossHp - 8, hp = prmprspd.hp - 7, mana = prmprspd.mana - 73, manaSpent = prmprspd.manaSpent + 73, effects = Map((Poison, 1)), path = List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield, Poison, Drain, Drain))
        val afterS = prmprspd.copy(bossHp = prmprspd.bossHp - 6, hp = prmprspd.hp - 2, mana = prmprspd.mana - 113, manaSpent = prmprspd.manaSpent + 113, effects = Map((Poison, 1), (Shield, 5)), path = List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield, Poison, Drain, Shield))
        val afterR = prmprspd.copy(bossHp = prmprspd.bossHp - 6, hp = prmprspd.hp - 9, mana = prmprspd.mana - 128, manaSpent = prmprspd.manaSpent + 229, effects = Map((Poison, 1), (Recharge, 4)), path = List(Poison, Recharge, MagicMissile, Poison, Recharge, Shield, Poison, Drain, Recharge))

        val expectedNeighbours: Seq[State] = Seq(afterMM, afterD, afterS, afterR)
        val next = prmprspd.next
        assert(next.size == expectedNeighbours.size, "wrong size")
        assert(next.toSet == expectedNeighbours.toSet, "wrong elements")
    }

}
