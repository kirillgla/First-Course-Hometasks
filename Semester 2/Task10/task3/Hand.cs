using System.Collections.Generic;

namespace Task3
{
    // I don't know whether it should be struct or not
    public class Hand
    {
        public AbstractPlayer Owner { get; }
        public List<Card> Cards { get; }
        public uint InitialBet { get; }

        public Hand(AbstractPlayer owner, List<Card> cards, uint initialBet)
        {
            Owner = owner;
            Cards = cards;
            InitialBet = initialBet;
        }
    }
}
