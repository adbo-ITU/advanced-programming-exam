/* This file is empty on purpose.   It is added, and configured if you
 * wanted to add your own tests during the exam.  It is not graded and
 * should not be submitted.
 */
package adpro

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.*

object ExamSpec
  extends org.scalacheck.Properties("exam-2023-autumn"):

  property("A test that always passes (a sanity check)") = 
    forAll { (n: Int) => n == n }

  property("Q1") = 
    import Streaming.*
    import laziness.LazyList
    forAll { (l: List[Int]) => fViaRec(LazyList(l*)) == fViaFold(LazyList(l*)) }

  property("Q2: longestLine") =
    import Parsing.{longestLine, allLinesTheSame}
    import adpro.parsing.Sliceable.*

    forAll { (l: List[List[Int]]) =>
      (l.nonEmpty && l.forall(_.length > 0)) ==> {
        val s = l.map(_.mkString(",")).mkString("\n")
        Right(l.map(_.length).max) == longestLine.run(s)
      }
    }

  property("Q3: allLinesTheSame with different length lines") =
    import Parsing.{longestLine, allLinesTheSame}
    import adpro.parsing.Sliceable.*

    forAll { (l: List[List[Int]]) =>
      (l.nonEmpty && l.forall(_.length > 0) && l.map(_.length).toSet.size > 1) ==> {
        val s = l.map(_.mkString(",")).mkString("\n")
        allLinesTheSame.run(s) == Right(false)
      }
    }

  property("Q3: allLinesTheSame with same length lines") =
    import Parsing.{longestLine, allLinesTheSame}
    import adpro.parsing.Sliceable.*

    forAll (Gen.choose(1, 100), Gen.listOfN(100, Gen.choose(1, 100))) {
      (n: Int, l: List[Int]) =>
        (l.nonEmpty) ==> {
          val s = (1 to n).map(_ => l.mkString(",")).mkString("\n")
          allLinesTheSame.run(s) == Right(true)
        }
    }

  import adpro.Game.Alice
  import adpro.Game.Bob
  import adpro.Game.game
  import adpro.Game.Player.*

  given rng: spire.random.rng.SecureJava 
    = spire.random.rng.SecureJava.apply

  val M = 10_000
  val Err = 0.05

  def acceptableErr(actual: Double, expected: Double) = (actual - expected).abs <= Err

  property("Q4/Q5: Bob against himself has 25% chance of winning") =
    // Outcomes:
    //  R P
    //  P R
    //  R R
    //  P P
    val pr = game(Bob, Bob).sample(M)
    acceptableErr(pr.prMatching { case Some(P1) => }, 0.25)
    acceptableErr(pr.prMatching { case Some(P2) => }, 0.25)
    acceptableErr(pr.prMatching { case None => }, 0.5)

  property("Q4/Q5: Alice against herself has 33% chance of winning") =
    val pr = game(Alice, Alice).sample(M)
    acceptableErr(pr.prMatching { case Some(P1) => }, 0.33)
    acceptableErr(pr.prMatching { case Some(P2) => }, 0.33)
    acceptableErr(pr.prMatching { case None => }, 0.33)

  property("Q4/Q5: Alice against Bob has 33% chance of winning") =
    val pr = game(Alice, Bob).sample(M)
    acceptableErr(pr.prMatching { case Some(P1) => }, 0.33)
    acceptableErr(pr.prMatching { case Some(P2) => }, 0.33)
    acceptableErr(pr.prMatching { case None => }, 0.33)

end ExamSpec

object NullUpdatesSpecObj
  extends RL.NullUpdatesSpec(update = RL.update, "studentrl") {}
