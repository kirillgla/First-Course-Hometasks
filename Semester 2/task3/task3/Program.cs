using System;
using System.Collections.Generic;
using System.Linq;

namespace Task3
{
    static class Program
    {
        const uint InitialMoney = 100;
        const uint MaxPlayers = 10;
        
        static void Main()
        {
            Console.WriteLine("==-== BlackJack ==-==");
            Console.WriteLine();
            
            uint playerCount = GetPlayerCount();
            bool humanParticipates = ConsoleInteractions.Confirm("Will you particiate in game? [y/n]: ");

            var players = new List<AbstractPlayer>();

            uint nonHumanplayers;
            if (humanParticipates)
            {
                players.Add(new HumanPlayer(InitialMoney));
                nonHumanplayers = playerCount - 1;
            }

            else
            {
                nonHumanplayers = playerCount;
            }

            var random = new Random();
            for (int i = 0; i < nonHumanplayers; i++)
            {
                // Can possibly support more bots
                switch (random.Next(1, 3))
                {
                    case 1:
                        int number = players.OfType<SameAsDealerBotPlayer>().Count();
                        players.Add(new SameAsDealerBotPlayer(InitialMoney, $"Same-as-dealer bot{(number == 0 ? "" : $" #{number + 1}")}"));
                        break;
                    case 2:
                        number = players.OfType<VeryBraveBotPlayer>().Count();
                        players.Add(new VeryBraveBotPlayer(InitialMoney, $"Very brave bot{(number == 0 ? "" : $" #{number + 1}")}"));
                        break;
                    default:
                        throw new ArgumentOutOfRangeException();
                }
            }

            var deck = ShuffleMachine.GetShuffledDecks(8);

            Console.WriteLine();

            const int initialRoundNumber = 1;

            var lostPlayers = new List<AbstractPlayer>();
            
            // Starts game
            Game.Round(players, deck, initialRoundNumber, lostPlayers);

            Console.ReadKey();
        }
        
        

        static uint GetPlayerCount()
        {
            Console.Write("Please, enter number of players: ");
            if (uint.TryParse(Console.ReadLine(), out uint result) && result > 0 && result <= MaxPlayers)
            {
                return result;
            }
            Console.Write("Error. ");
            return GetPlayerCount();
        }
    }
}
