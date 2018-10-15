using System.Collections.Generic;

namespace Task3
{
    public static class CardUtils
    {
        public static uint GetScore(IEnumerable<Card> list)
        {
            uint result = 0;
            uint aces = 0;
            foreach (var card in list)
            {
                if (card.Value == Value.Ace)
                {
                    aces++;
                }
                else
                {
                    result += card.Score();
                }
            }

            if (result + aces > 21)
            {
                return result + aces;
            }

            // Now, total aces can give
            //     aces + M * 10
            // where M in [0..aces]
            // Hence, score is
            //     result + aces + M * 10
            // We want this value to be <= 21
            //     result + aces + M * 10 <= 21
            //     M <= (21 - result - aces) / 10
            
            uint m = (21 - result - aces) / 10;

            if (m > aces)
            {
                m = aces;
            }
            
            return result + aces + m * 10;
        }

        public static bool IsPair(List<Card> hand)
        {
            return hand.Count == 2 && hand[0].Score() == hand[1].Score();
        }
    }
}
