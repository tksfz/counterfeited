package counterfeited

package object cards {

  case class Suit(abbrev: Char)
  val Spade = Suit('s')
  val Heart = Suit('h')
  val Club = Suit('c')
  val Diamond = Suit('d')

  case class Rank(value: Int, abbrev: Char)

  object Rank {
    def apply(value: Int) = new Rank(value, value.toString.charAt(0))
  }

  val Deuce = Rank(2)
  val Trey = Rank(3)
  val Four = Rank(4)
  val Five = Rank(5)
  val Six = Rank(6)
  val Seven = Rank(7)
  val Eight = Rank(8)
  val Nine = Rank(9)
  val Ten = Rank(10, 'T')
  val Jack = Rank(11, 'J')
  val Queen = Rank(12, 'Q')
  val King = Rank(13, 'K')
  val Ace = Rank(14, 'A')

  case class Card(rank: Rank, suit: Suit)

  case class Hand(cards: Seq[Card])

  case class Deck(cards: Seq[Card])

}