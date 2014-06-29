package counterfeited

import cards._
import eval._
import scala.util.Random
import scala.collection.mutable

package object simulator {

  private[this] val rand = new Random

  def computeShowdownHands(hole1: Card, hole2: Card, nTrials: Int = 100000) = {
    val handClassCounts = new mutable.HashMap[String, Int]
    for(i <- 0 until nTrials) {
      val holeCards = Seq(hole1, hole2)
      val board = generateRandomCards(5, holeCards)
      val handClass = eval.simple7Classifier(Hand((holeCards ++ board).toIndexedSeq)).abbrev
      if (handClassCounts.contains(handClass)) {
        handClassCounts(handClass) += 1
      } else {
        handClassCounts(handClass) = 1
      }
    }
    handClassCounts.toMap
  }

  def generateRandomCards(n: Int, dead: Seq[Card]): Seq[Card] = {
    var dead2 = new mutable.HashSet[Card]
    dead2 ++= dead
    for(i <- 0 until n) yield {
      val card = generateRandomCard(dead2)
      dead2 += card
      card
    }
  }

  def generateRandomCard(dead: mutable.Set[Card]) = {
    var card: Card = null
    while(card == null || dead.contains(card)) {
      val rank = allRanks(rand.nextInt(allRanks.size))
      val suit = allSuits(rand.nextInt(allSuits.size))
      card = Card(rank, suit)
    }
    card
  }
}

object Simulator {
  def main(args: Array[String]) = {
    val hand = parseHand(Console.readLine("hole cards> "))
    val handClassCounts = simulator.computeShowdownHands(hand.cards(0), hand.cards(1))
    Console.println(handClassCounts)
    val total = handClassCounts.values.sum
    Console.println(handClassCounts.values.sum)
    val handClassPercents = handClassCounts.mapValues(_ / total.asInstanceOf[Double])
    Console.println(handClassPercents)
  }
}