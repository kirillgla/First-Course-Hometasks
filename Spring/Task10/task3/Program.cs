using System;
using System.Linq;
using System.Collections.Generic;
using Unity;
using Microsoft.Practices.Unity.Configuration;

namespace Task3
{
    static class Program
    {
        const uint MaxPlayers = 10;

        public static IUnityContainer Container { get; private set; }

        static void Main()
        {
            Container = new UnityContainer().LoadConfiguration();
            
            PlayGame();
        }

        static void PlayGame()
        {
            Console.WriteLine("==-== BlackJack ==-==");
            Console.WriteLine();

            uint playerCount = GetPlayerCount();
            bool humanParticipates = ConsoleInteractions.Confirm("Will you particiate in game? [y/n]: ");

            var players = new List<AbstractPlayer>();

            uint nonHumanplayers;
            if (humanParticipates)
            {
                var humanPlayer = Container.Resolve<AbstractPlayer>("HumanPlayer");
                players.Add(humanPlayer);
                nonHumanplayers = playerCount - 1;
            }

            else
            {
                nonHumanplayers = playerCount;
            }

            var random = new Random();
            for (int i = 0; i < nonHumanplayers; i++)
            {
                int count;
                string name;
                AbstractPlayer player;
                // Can possibly support more bots
                switch (random.Next(1, 3))
                {
                    case 1:
                        count = players.OfType<SameAsDealerBotPlayer>().Count();
                        name = $"Same-as-dealer bot{(count == 0 ? "" : $" #{count + 1}")}";
                        Container.RegisterInstance("BotName", name);
                        player = Container.Resolve<AbstractPlayer>("SameAsDealerBotPlayer");
                        players.Add(player);
                        break;
                    case 2:
                        count = players.OfType<VeryBraveBotPlayer>().Count();
                        name = $"Very brave bot{(count == 0 ? "" : $" #{count + 1}")}";
                        Container.RegisterInstance("BotName", name);
                        player = Container.Resolve<AbstractPlayer>("VeryBraveBotPlayer");
                        players.Add(player);
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
