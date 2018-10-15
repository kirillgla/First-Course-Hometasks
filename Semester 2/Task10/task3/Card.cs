namespace Task3
{
    public enum Suit
    {
        Hearts,
        Diamonds,
        Clubs,
        Spades
    }

    public enum Value
    {
        Two = 2,
        Three,
        Four,
        Five,
        Six,
        Seven,
        Eight,
        Nine,
        Ten,
        Jack,
        Queen,
        King,
        Ace
    }

    public struct Card
    {
        internal readonly Suit Suit;
        internal readonly Value Value;

        public Card(Suit suit, Value value)
        {
            Suit = suit;
            Value = value;
        }
        
        public override string ToString()
        {
            return $"{Value} of {Suit}";
        }

        public uint Score()
        {
            if (Value == Value.Ace)
            {
                return 11;
            }
            if (Value <= Value.Ten)
            {
                return (uint)Value;
            }
            return 10;
        }
    }
}
