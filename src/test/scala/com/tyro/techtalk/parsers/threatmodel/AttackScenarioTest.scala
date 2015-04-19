package com.tyro.techtalk.parsers.threatmodel

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class AttackScenarioTest extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {

  "An attack scenario should" - {
    "result in a compromised target" - {
      "when the target has no defences" in {
        forAll { (name: String, threat: Threat) =>
          val target = Target(name, Set.empty[CounterMeasure])
          AttackScenario.modelAttack(target, threat) should be(Compromised)
        }
      }
    }

    "result in an uncompromised target" - {
      "when the target has a counter-defence" in {
        forAll { (name: String, threat: Threat) =>
          val target = Target(name, threat.counteredBy)

          AttackScenario.modelAttack(target, threat) should be(UnCompromised)
        }
      }
    }
  }

  lazy val genThreat: Gen[Threat] = {
    for {
      name <- Gen.alphaStr
      counterMeasures <- Gen.nonEmptyContainerOf[Set, CounterMeasure](genCounterMeasure)
    } yield Threat(name, counterMeasures)
  }

  lazy val genCounterMeasure: Gen[CounterMeasure] = {
    for {
      name <- Gen.alphaStr
    } yield CounterMeasure(name)
  }

  implicit val arbThreat: Arbitrary[Threat] = Arbitrary(genThreat)
}
