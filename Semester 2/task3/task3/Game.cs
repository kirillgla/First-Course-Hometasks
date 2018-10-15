using System;
using System.Collections.Generic;
using System.Linq;

namespace Task3
{
    public static class Game
    {
        internal static void Round(IList<AbstractPlayer> players, IList<Card> deck, int roundNumber,
            IList<AbstractPlayer> lost)
        {
            int i = 0;
            while (i < players.Count)
            {
                if (players[i].Money == 0)
                {
                    lost.Add(players[i]);
                    players.RemoveAt(i);
                }
                else
                {
                    i++;
                }
            }

            if (!players.Any())
            {
                return;
            }

            // Following formula ensures that players
            // can pick as many cards from the deck as they want
            if (deck.Count < (players.Count + 1) * 11)
            {
                deck = ShuffleMachine.GetShuffledDecks(8);
            }

            Console.WriteLine($"==-== Round {roundNumber} ==-==");
            if (lost.Any())
            {
                ConsoleInteractions.WriteList(lost, "(");
                Console.WriteLine(" have lost all their money)");
            }

            Console.WriteLine();

            var bets = GetInitialBets(players);

            Console.WriteLine();

            var hands = GetHands(players, bets, deck);
            var dealer = new Dealer(deck);
            Console.WriteLine($"Dealer's card is {dealer.FirstCard} (score: {dealer.FirstCard.Score()})");
            Console.WriteLine();

            i = 0;
            while (i < hands.Count)
            {
                Console.WriteLine(hands[i].Owner.Name == "You" ? "Your turn!" : $"Turn of {hands[i].Owner.Name}.");
                if (!PerformActions(hands, deck, i, dealer))
                {
                    hands.RemoveAt(i);
                }
                else
                {
                    i++;
                }

                ConsoleInteractions.PressAnyKey();
            }

            Console.WriteLine();
            Console.WriteLine("Dealer collects cards:");

            dealer.TakeEnoughCards(deck);

            dealer.WriteCards();
            Console.WriteLine($" ({dealer.Score()})");
            Console.WriteLine();

            if (dealer.Score() > 21)
            {
                Console.WriteLine("Dealer lost!");
                foreach (var hand in hands)
                {
                    if (CardUtils.GetScore(hand.Cards) == 21 && hand.Cards.Count == 2)
                    {
                        Console.WriteLine(
                            $"{hand.Owner.Name} {(hand.Owner.Name == "You" ? "have" : "has")} blackjack!");
                        Console.WriteLine(
                            $"{hand.Owner.Name} {(hand.Owner.Name == "You" ? "get" : "gets")} repaid 3:2!");
                        hand.Owner.GiveMoney((int) hand.InitialBet * 5 / 2);
                        Console.WriteLine();
                    }
                    else
                    {
                        Console.WriteLine(
                            $"{hand.Owner.Name} {(hand.Owner.Name == "You" ? "get" : "gets")} repaid 1:1!");
                        hand.Owner.GiveMoney((int) hand.InitialBet * 2);
                        Console.WriteLine();
                    }
                }
            }
            else
            {
                foreach (var hand in hands)
                {
                    uint score = CardUtils.GetScore(hand.Cards);

                    if (score == dealer.Score())
                    {
                        Console.WriteLine("{0} {1} equal score with dealer. {2}$ bet is returned.", hand.Owner.Name,
                            hand.Owner.Name == "You" ? "have" : "has", hand.InitialBet);
                        hand.Owner.GiveMoney((int) hand.InitialBet);
                        Console.WriteLine();
                    }
                    else if (score > dealer.Score())
                    {
                        Console.WriteLine(
                            $"{hand.Owner.Name} {(hand.Owner.Name == "You" ? "beat" : "beats")} the dealer!");
                        Console.WriteLine(
                            $"{hand.Owner.Name} {(hand.Owner.Name == "You" ? "get" : "gets")} repaid 1:1!");
                        hand.Owner.GiveMoney((int) hand.InitialBet * 2);
                        Console.WriteLine();
                    }
                    else
                    {
                        Console.WriteLine($"{hand.Owner.Name} lost.");
                        Console.WriteLine();
                    }
                }
            }

            ConsoleInteractions.PressAnyKey();
            Round(players, deck, roundNumber + 1, lost);
        }

        /// <summary>
        /// Checks that hand is valid and should stay in game
        /// </summary>
        /// <returns>
        /// whether player can stay in game or not
        /// </returns>
        static PlayerState CheckScore(Hand hand, Dealer dealer)
        {
            if (CardUtils.GetScore(hand.Cards) == 21 && hand.Cards.Count == 2)
            {
                Console.WriteLine("{0} {1} blackjack!", hand.Owner.Name, hand.Owner.Name == "You" ? "have" : "has");
                if (dealer.FirstCard.Score() != 10 || dealer.FirstCard.Score() == 11)
                {
                    Console.WriteLine($"Dealer has {dealer.FirstCard.Score()} though");
                    Console.WriteLine();
                    return PlayerState.BlackJack;
                }

                Console.WriteLine("{0} {1} repaid 3:2!", hand.Owner.Name, hand.Owner.Name == "You" ? "get" : "gets");
                hand.Owner.GiveMoney((int) hand.InitialBet * 5 / 2);
                Console.WriteLine();
                return PlayerState.Won;
            }

            if (CardUtils.GetScore(hand.Cards) <= 21)
            {
                return PlayerState.Playing;
            }

            Console.WriteLine("{0} {1} above 21... Bet removed.", hand.Owner.Name,
                hand.Owner.Name == "You" ? "are" : "is");
            Console.WriteLine();
            return PlayerState.Lost;
        }

        /// <summary>
        /// Let a player decide what to do
        /// </summary>
        /// <returns>
        /// Whether hand should stay in hands or not
        /// </returns>
        static bool PerformActions(IList<Hand> hands, IList<Card> deck, int index, Dealer dealer)
        {
            AbstractPlayer.WriteCards(hands[index].Cards, "Current hand: ");
            Console.WriteLine($" (score: {CardUtils.GetScore(hands[index].Cards)})");
            switch (CheckScore(hands[index], dealer))
            {
                case PlayerState.Playing:
                    break;
                case PlayerState.Won:
                    return false;
                case PlayerState.Lost:
                    return false;
                case PlayerState.BlackJack:
                    return true;
                default:
                    throw new ArgumentOutOfRangeException();
            }

            var action = hands[index].Owner.ChooseAction(dealer, hands[index]);
            Console.WriteLine();
            switch (action)
            {
                case Action.Stand:
                    return true;
                case Action.Hit:
                    GiveCard(hands, deck, index);
                    Console.WriteLine();
                    return PerformActions(hands, deck, index, dealer);
                case Action.Double:
                    hands[index].Owner.GiveMoney((int) -hands[index].InitialBet);
                    Console.WriteLine();
                    GiveCard(hands, deck, index);
                    Console.WriteLine();
                    var state = CheckScore(hands[index], dealer);
                    return state == PlayerState.Playing;
                case Action.Split:
                    hands[index].Owner.GiveMoney((int) -hands[index].InitialBet);
                    Console.WriteLine();
                    Console.WriteLine($"Card added to first hand: {deck[0]}");
                    Console.WriteLine($"Card added to second hand: {deck[1]}");
                    var half = new Hand(
                        hands[index].Owner,
                        new List<Card>
                        {
                            hands[index].Cards[0],
                            deck[1]
                        },
                        hands[index].InitialBet
                    );
                    hands.Insert(index + 1, half);
                    hands[index].Cards.RemoveAt(0);
                    hands[index].Cards.Add(deck[0]);
                    deck.RemoveAt(0);
                    deck.RemoveAt(0);
                    return PerformActions(hands, deck, index, dealer);
                case Action.Surrender:
                    hands[index].Owner.GiveMoney((int) hands[index].InitialBet / 2);
                    Console.WriteLine();
                    return false;
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

        static void GiveCard(IList<Hand> hands, IList<Card> deck, int index)
        {
            Console.WriteLine($"New card is {deck[0]}.");
            hands[index].Cards.Add(deck[0]);
            deck.RemoveAt(0);
            // Console.WriteLine($"New Score is {Card.GetScore(hands[index].Cards)}.");
        }

        static List<Hand> GetHands(IList<AbstractPlayer> players, IReadOnlyList<uint> bets, IList<Card> deck)
        {
            var hands = new List<Hand>();
            for (int i = 0; i < players.Count; i++)
            {
                hands.Add(new Hand(
                    players[i],
                    new List<Card>
                    {
                        deck[0],
                        deck[1]
                    },
                    bets[i]
                ));
                Console.WriteLine(
                    $"{players[i].Name} got {deck[0]} and {deck[1]} (score: {CardUtils.GetScore(hands[i].Cards)})");
                deck.RemoveAt(0);
                deck.RemoveAt(0);
            }

            return hands;
        }

        static uint[] GetInitialBets(IList<AbstractPlayer> players)
        {
            uint[] bets = new uint[players.Count];
            for (int i = 0; i < players.Count; i++)
            {
                bets[i] = players[i].MakeInitialBet();
            }

            return bets;
        }
    }
}
