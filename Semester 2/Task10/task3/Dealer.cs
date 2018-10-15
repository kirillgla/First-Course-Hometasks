using System;
using System.Collections.Generic;
using System.Linq;

namespace Task3
{
    public class Dealer
    {
        List<Card> Hand { get; }

        public Card FirstCard
        {
            get
            {
                if (Hand != null && Hand.Any())
                {
                    return Hand[0];
                }
                throw new InvalidOperationException("Attempt to access first card before it has been given.");
            }
        }

        public Dealer(IList<Card> deck)
        {
            Hand = new List<Card>
            {
                deck[0]
            };
            deck.RemoveAt(0);
        }

        public void TakeEnoughCards(IList<Card> deck)
        {
            if (Score() >= 17)
            {
                return;
            }

            Hand.Add(deck[0]);
            deck.RemoveAt(0);
            TakeEnoughCards(deck);
        }

        public uint Score() => CardUtils.GetScore(Hand);

        public void WriteCards(string message = null)
        {
            if (message != null)
            {
                Console.Write(message);
            }
            for (int i = 0; i < Hand.Count; i++)
            {
                Console.Write(Hand[i]);
                if (i == Hand.Count - 2)
                {
                    Console.Write(" and ");
                }
                else if (i != Hand.Count - 1)
                {
                    Console.Write(", ");
                }
            }
        }
    }
}
