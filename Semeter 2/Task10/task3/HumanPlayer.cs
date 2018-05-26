namespace Task3
{
    class HumanPlayer : AbstractPlayer
    {
        public HumanPlayer(uint initialMoney) : this(initialMoney, "You")
        {
        }

        // Not that useful though
        HumanPlayer(uint initialMoney, string name) : base(initialMoney, name)
        {
        }

        public override Action ChooseAction(Dealer dealer, Hand hand) =>
            ConsoleInteractions.ChooseAction(GetPossibleActions(hand));

        public override uint MakeInitialBet()
        {
            uint result = ConsoleInteractions.MakeInitialBet(Money);

            checked
            {
                Money -= result;
            }

            return result;
        }
    }
}
