package counterfeited

package object cards {

  case class Suit(abbrev: Char)
  val Spade = Suit('s')
  val Heart = Suit('h')
  val Club = Suit('c')
  val Diamond = Suit('d')

  case class Rank(value: Int) extends AnyVal {
    def abbrev = this match {
      case Ten => 'T'
      case Jack => 'J'
      case Queen => 'Q'
      case King => 'K'
      case Ace => 'A'
      case _ => value.toString.charAt(0)
    }
  }
  object Rank {
    implicit val orderByDescendingRank = Ordering.by[Rank, Int](- _.value)
  }

  val Deuce = Rank(2)
  val Trey = Rank(3)
  val Four = Rank(4)
  val Five = Rank(5)
  val Six = Rank(6)
  val Seven = Rank(7)
  val Eight = Rank(8)
  val Nine = Rank(9)
  val Ten = Rank(10)
  val Jack = Rank(11)
  val Queen = Rank(12)
  val King = Rank(13)
  val Ace = Rank(14)

  val allRanks = Seq(Deuce, Trey, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)
  val allSuits = Seq(Spade, Heart, Club, Diamond)
  val rankChars = allRanks.map(r => r.abbrev -> r).toMap
  val suitChars = allSuits.map(s => s.abbrev -> s).toMap

  case class Card(rank: Rank, suit: Suit) {
    override def toString = rank.abbrev.toString + suit.abbrev
  }

  case class Hand(cards: IndexedSeq[Card])

  case class Deck(cards: Seq[Card])

  val allCards = for(suit <- allSuits; rank <- allRanks) yield Card(rank, suit)

  def parseHand(s: String) = {
    var s2 = s.filterNot(_ == ' ')
    val cards = for(cardStr <- s2.grouped(2)) yield {
      val rankChar = cardStr(0)
      val suitChar = cardStr(1)
      val rank = rankChars(rankChar)
      val suit = suitChars(suitChar)
      Card(rank, suit)
    }
    Hand(cards.toIndexedSeq)
  }

}
