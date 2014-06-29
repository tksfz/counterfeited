package counterfeited

import cards._
import eval._
import scala.util.Random
import scala.collection.mutable

package object simulator {

  private[this] val rand = new Random

  def computeShowdownHands(hole1: Card, hole2: Card, nTrials: Int = 100000) = {
    val handClassCounts = new mutable.HashMap[String, Int].withDefaultValue(0)
    for(i <- 0 until nTrials) {
      val holeCards = Seq(hole1, hole2)
      val board = generateRandomCards(5, holeCards)
      val handClass = eval.simpleClassifier(Hand((holeCards ++ board).toIndexedSeq)).abbrev
      handClassCounts(handClass) += 1
    }
    handClassCounts.toMap
  }

  def compareShowdownEquity(playerHands: Seq[Hand], nTrials: Int = 100000) = {
    val wins = new mutable.HashMap[Int, Double].withDefaultValue(0)
    for(i <- 0 until nTrials) {
      val board = generateRandomCards(5, playerHands.flatMap(_.cards))
      val fullPlayerHands = playerHands.map(playerHand => Hand(playerHand.cards ++ board))
      val playerHandClasses = fullPlayerHands.map(simpleClassifier(_))
      val winningHandClass = playerHandClasses.min
      val winningPlayers = playerHandClasses.zipWithIndex.filter(_._1 == winningHandClass).map(_._2)
      for(iPlayer <- winningPlayers) {
        wins(iPlayer) += 1.0 / winningPlayers.size
      }
    }
    wins
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
    Console.println(total)
    val handClassPercents = handClassCounts.mapValues(_ / total.asInstanceOf[Double])
    Console.println(handClassPercents)
  }
}

object Showdown {
  def main(args: Array[String]) = {
    val hands = Console.readLine("hole cards> ").split("\\s+").map(parseHand(_)).toSeq
    val wins = simulator.compareShowdownEquity(hands)
    Console.println(wins)
    val total = wins.values.sum
    Console.println(total)
    val winPercents = wins.mapValues(_ / total.asInstanceOf[Double])
    Console.println(winPercents)
  }
}