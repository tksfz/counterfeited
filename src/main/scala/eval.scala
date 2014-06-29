package counterfeited

import cards._

package object eval {
  trait Evaluator {
    def compare(h1: Hand, h2: Hand): Int
  }


  abstract class HandClass(classRanking: Int, abbrev: String)
  case class StraightFlush(ranks: Seq[Rank]) extends HandClass(8, "SF")
  case class Quads(rank: Rank, kicker: Rank) extends HandClass(7, "4K")
  case class FullHouse(trips: Rank, pair: Rank) extends HandClass(6, "FH")
  case class Flush(ranks: Seq[Rank]) extends HandClass(5, "F")
  case class Straight(highCard: Rank) extends HandClass(4, "S")
  case class Trips(rank: Rank, k1: Rank, k2: Rank) extends HandClass(3, "3K")
  case class TwoPair(highPair: Rank, lowPair: Rank, kicker: Rank) extends HandClass(2, "2P")
  case class Pair(rank: Rank, k1: Rank, k2: Rank, k3: Rank) extends HandClass(1, "1P")

  type Classifier = Hand => HandClass

  class Simple5Evaluator extends Evaluator {
    override def compare(h1: Hand, h2: Hand): Int = {
      ???
    }
  }

}