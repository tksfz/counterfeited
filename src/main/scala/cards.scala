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

  case class Card(rank: Rank, suit: Suit)

  case class Hand(cards: IndexedSeq[Card])

  case class Deck(cards: Seq[Card])

}