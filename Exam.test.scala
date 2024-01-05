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

  import Parsing.{longestLine, allLinesTheSame}
  import adpro.parsing.Sliceable.*

  property("Q2: longestLine example") =
    longestLine.run("1,2,3 , 5,4\n42,42") == Right(5)

  property("Q2: longestLine") =
    forAll { (l: List[List[Int]]) =>
      (l.nonEmpty && l.forall(_.length > 0)) ==> {
        val s = l.map(_.mkString(",")).mkString("\n")
        Right(l.map(_.length).max) == longestLine.run(s)
      }
    }

  property("Q3: allLinesTheSame example") =
    allLinesTheSame.run("1,2,3 , 5,4\n42,42") == Right(false)

  property("Q3: hardcoded allLinesTheSame true case") =
    allLinesTheSame.run("1,2,3\n420, 42,42") == Right(true)

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
    //  R P - P2
    //  P R - P1
    //  R R - Draw
    //  P P - Draw
    val pr = game(Bob, Bob).sample(M)
    acceptableErr(pr.prMatching { case Some(P1) => }, 0.25)
    acceptableErr(pr.prMatching { case Some(P2) => }, 0.25)
    acceptableErr(pr.prMatching { case None => }, 0.5)

  property("Q4/Q5: Alice against herself has 33% chance of winning") =
    // Outcomes
    // R R - Draw
    // R P - P2
    // R S - P1
    // P R - P1
    // P P - Draw
    // P S - P2
    // S R - P2
    // S P - P1
    // S S - Draw
    val pr = game(Alice, Alice).sample(M)
    acceptableErr(pr.prMatching { case Some(P1) => }, 0.33)
    acceptableErr(pr.prMatching { case Some(P2) => }, 0.33)
    acceptableErr(pr.prMatching { case None => }, 0.33)

  property("Q4/Q5: Alice and Bob against each other both have 33% chance of winning") =
    // Outcomes
    // R R - Draw
    // R P - P2
    // P R - P1
    // P P - Draw
    // S R - P2
    // S P - P1
    val pr = game(Alice, Bob).sample(M)
    acceptableErr(pr.prMatching { case Some(P1) => }, 0.33)
    acceptableErr(pr.prMatching { case Some(P2) => }, 0.33)
    acceptableErr(pr.prMatching { case None => }, 0.33)

  property("Q6: Alice's fraction against Bob is 33%") =
    acceptableErr(adpro.Game.aliceFraction, 0.33)

  property("Q9: Update with lens is identical to update without lens") =
    import adpro.RL

    forAll (RL.qGen(2, 3), Gen.choose(0, 1), Gen.choose(0, 2), Gen.double) {
      (q: RL.Q[Int, Int], state: Int, action: Int, est: Double) =>
        val noLens = RL.update(q, state, action)(q(state)(action), est)
        val withLens = RL.updateWithLens(q, state, action)(q(state)(action), est)
        noLens == withLens
    }

end ExamSpec

object NullUpdatesSpecObj
  extends RL.NullUpdatesSpec(update = RL.update, "studentrl") {}
