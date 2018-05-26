using System;
using System.Collections.Generic;
using Unity;

namespace Task3
{
    public static class ShuffleMachine
    {
        public static List<Card> GetShuffledDecks(uint decks)
        {
            var list = CreateDecks(decks);
            var random = new Random();
            for (int i = list.Count - 1; i > 0; --i)
            {
                int j = random.Next(i + 1);
                var tmp = list[i];
                list[i] = list[j];
                list[j] = tmp;
            }
            return list;
        }

        static List<Card> CreateDecks(uint decks)
        {
            var list = new List<Card>();
            foreach (Suit suit in Enum.GetValues(typeof(Suit)))
            {
                Program.Container.RegisterInstance("Suit", suit);
                foreach (Value value in Enum.GetValues(typeof(Value)))
                {
                    Program.Container.RegisterInstance("Value", value);
                    var card = Program.Container.Resolve<Card>();
                    for (int i = 0; i < decks; i++)
                    {
                        list.Add(card);
                    }
                }
            }
            return list;
        }
    }
}
