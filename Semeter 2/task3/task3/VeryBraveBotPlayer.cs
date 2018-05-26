using System;

namespace Task3
{
    class VeryBraveBotPlayer : AbstractPlayer
    {
        public VeryBraveBotPlayer(uint initialMoney, string name = "Very brave bot") : base(initialMoney, name)
        {
        }

        public override Action ChooseAction(Dealer dealer, Hand hand)
        {
            Action result;
            uint score = CardUtils.GetScore(hand.Cards);
            var actions = GetPossibleActions(hand);
            if (score == 15)
            {
                // That score seems the least trustworthy
                result = Action.Surrender;
            }
            else if (score >= 10 && score <= 11 && actions.Contains(Action.Double))
            {
                result = Action.Double;
            }
            else if (score <= 16 && actions.Contains(Action.Split))
            {
                result = Action.Split;
            }
            else if (score < 15)
            {
                result = Action.Hit;
            }
            else
            {
                result = Action.Stand;
            }
            Console.WriteLine($"{Name} decided to {result}!");
            return result;
        }

        public override uint MakeInitialBet()
        {
            uint bet;
            if (Money >= 10)
            {
                Money -= 10;
                bet = 10;
            }
            else
            {
                Money = 0;
                bet = Money;
            }
            Console.WriteLine($"{Name} bets {bet}$ out of {Money + bet}$");
            return bet;
        }
    }
}
