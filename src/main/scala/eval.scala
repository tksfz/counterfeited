package counterfeited

import cards._

package object eval {
  trait Hand7 extends Hand

  trait Evaluator {
    def compare(h1: Hand, h2: Hand): Int
  }

  /**
   * Equivalence classes of hands
   * @param classRanking the relative rankings of the hand classes themselves
   * @param abbrev a short display label for this hand class
   */
  abstract class HandClass(val classRanking: Int, val abbrev: String) {
    def ranksToCompare: Seq[Rank]
  }
  case class StraightFlush(highCard: Rank) extends HandClass(8, "SF") {
    val ranksToCompare = Seq(highCard)
  }
  case class Quads(rank: Rank, kicker: Rank) extends HandClass(7, "4K") {
    val ranksToCompare = Seq(rank, kicker)
  }
  case class FullHouse(trips: Rank, pair: Rank) extends HandClass(6, "FH") {
    val ranksToCompare = Seq(trips, pair)
  }
  case class Flush(ranks: Seq[Rank]) extends HandClass(5, "F") {
    val ranksToCompare = ranks
  }
  case class Straight(highCard: Rank) extends HandClass(4, "S") {
    val ranksToCompare = Seq(highCard)
  }
  case class Trips(rank: Rank, k1: Rank, k2: Rank) extends HandClass(3, "3K") {
    val ranksToCompare = Seq(rank, k1, k2)
  }
  case class TwoPair(highPair: Rank, lowPair: Rank, kicker: Rank) extends HandClass(2, "2P") {
    val ranksToCompare = Seq(highPair, lowPair, kicker)
  }
  case class Pair(rank: Rank, k1: Rank, k2: Rank, k3: Rank) extends HandClass(1, "1P") {
    val ranksToCompare = Seq(rank, k1, k2, k3)
  }
  case class HighCard(ranks: Seq[Rank]) extends HandClass(0, "HC") {
    val ranksToCompare = ranks
  }

  object HandClass {
    import Ordering.Implicits.seqDerivedOrdering

    implicit val ordering: Ordering[HandClass] = Ordering.by(hc => (-hc.classRanking, hc.ranksToCompare))
  }

  val handEvaluator: Ordering[Hand] = Ordering.by(hand => simpleClassifier(hand))

  // Some aliases, for fun
  val Wheel = Straight(Five)
  val Broadway = Straight(Ace)
  val RoyalFlush = StraightFlush(Ace)

  type Classifier = Hand => HandClass

  /**
   * A simple and slow but general classifier that works for 5-card through 7-card hands
   */
  val simpleClassifier: Classifier = { hand: Hand =>
    findFlush(hand) getOrElse {
      rankBasedClassifier(hand)
    }
  }

  def findFlush(hand: Hand) = {
    hand.cards.groupBy(_.suit).find(_._2.size >= 5).map { case (suit, cards) =>
      val ranks = cards.map(_.rank)
      findStraight(ranks) map { straight =>
        StraightFlush(straight.highCard)
      } getOrElse {
        Flush(ranks.sorted.take(5))
      }
    }
  }

  /**
   * @todo create a version that can return the hand itself (the Cards) or at least
   *       a helper that forms the HandClass without having to invoke ._1 on all
   *       the cards
   */
  def rankBasedClassifier(hand: Hand): HandClass = {
    val rankCounts = hand.cards.groupBy(_.rank).mapValues(_.size).toIndexedSeq
    val nDistinctRanks = rankCounts.size
    val straightOpt =
      if (nDistinctRanks >= 5) {
        findStraight(rankCounts.map(_._1))
      } else {
        None
      }

    if (!straightOpt.isDefined) {
      // Rank counts ordered by descending (count, rank)
      // Rank's Ordering is already descending, so we just need to reverse the counts
      val orderedRankCounts = rankCounts.sortBy(_.swap.map_1(- _))
      // @todo make a canonical suit ordering
      val topRankCount = orderedRankCounts.head
      val tailRankCounts = orderedRankCounts.tail
      if (topRankCount._2 == 4) {
        // 4K
        val kicker = tailRankCounts.minBy(_._1)
        Quads(topRankCount._1, kicker._1)
      } else if (topRankCount._2 == 3) {
        // FH or 3K
        // Consider the case where there is another 3K and a 1P.  Then the bottom of the FH
        // could be either the 3K (taken as a 1P) or it could be the 1P, depending on whichever has higher rank
        val pairCandidates = tailRankCounts.takeWhile(_._2 >= 2).sortBy(_._1)
        if (!pairCandidates.isEmpty) {
          val bestPair = pairCandidates.head
          FullHouse(topRankCount._1, bestPair._1)
        } else {
          val kickers = tailRankCounts.sortBy(_._1).take(2)
          Trips(topRankCount._1, kickers(0)._1, kickers(1)._1)
        }
      } else if (topRankCount._2 == 2) {
        // Two Pair or One Pair
        val secondRankCount = tailRankCounts.head
        if (secondRankCount._2 == 2) {
          val kicker = tailRankCounts.tail.sortBy(_._1).head
          TwoPair(topRankCount._1, secondRankCount._1, kicker._1)
        } else {
          val kickers = tailRankCounts.sortBy(_._1).take(3)
          Pair(topRankCount._1, kickers(0)._1, kickers(1)._1, kickers(2)._1)
        }
      } else {
        // HighCard
        HighCard(hand.cards.sortBy(_.rank).take(5).map(_.rank))
      }
    } else {
      straightOpt.get
    }
  }

  def findStraight(ranks: IndexedSeq[Rank]): Option[Straight] = {
    var orderedRanks = ranks.sorted
    if (orderedRanks(0) == Ace) {
      orderedRanks = orderedRanks :+ Ace
    }
    val n = orderedRanks.size
    var straight: Option[Straight] = None
    var i = 0
    while(i <= n - 5 && !straight.isDefined) {
      val highCard = orderedRanks(i)
      val lowCard = orderedRanks(i + 5 - 1)
      val diff = highCard.value - lowCard.value
      if (diff == 6 - 2 || diff == (5 - Ace.value)) {
        straight = Some(Straight(highCard))
      }
      i = i + 1
    }
    straight
  }

  implicit class MappablePair[A, B](pair: Tuple2[A, B]) {
    def map_1[C](fn: A => C) = {
      (fn(pair._1), pair._2)
    }
  }

}

object Main {
  def main(args: Array[String]) = {
    while(true) {
      val handStr = Console.readLine("hand> ")
      val hand = parseHand(handStr)
      val handClass = eval.simpleClassifier(hand)
      Console.println(handClass.toString)
    }
  }
}