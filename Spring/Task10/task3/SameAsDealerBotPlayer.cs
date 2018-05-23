using System;

namespace Task3
{
    /**
     * Bot that behaves similarly to dealer:
     * taks cards as long as below 17.
     * Doesn't perform other actions.
     */
    class SameAsDealerBotPlayer : AbstractPlayer
    {
        public SameAsDealerBotPlayer(uint initialMoney, string name = "Same-as-dealer bot") : base(initialMoney, name)
        {
        }

        public override Action ChooseAction(Dealer dealer, Hand hand)
        {
            var result = CardUtils.GetScore(hand.Cards) < 17 ? Action.Hit : Action.Stand;
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
