package com.tyro.techtalk.parsers.threatmodel

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class AttackScenarioTest extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {

  "An attack scenario should" - {
    "result in a compromised target when" - {
      "the target is vulnerable to the threat and has no defences" in {
        forAll { (name: String, vulnerabilities: Set[Threat]) =>
          val target = Target(name, vulnerabilities, Set.empty)
          vulnerabilities.foreach { threat =>
            AttackScenario.modelAttack(target, threat) should be(Compromised)
          }
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

  implicit val arbCounterMeasure: Arbitrary[CounterMeasure] = Arbitrary(genCounterMeasure)
}
