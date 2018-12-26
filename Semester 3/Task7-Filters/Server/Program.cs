using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using System.Net;
using Tools;
using Tools.Extensions;

namespace Server
{
    static class Program
    {
        static IPAddress ServerAddress => IPAddress.Loopback;

        public static void Main()
        {
            var filters = ReadFilters();
            Console.WriteLine($"Loaded {filters.Count} filters");

            var endPoint = new IPEndPoint(ServerAddress, Core.Port);
            using (var server = new Server(endPoint, filters))
            {
                server.Start();
                Console.WriteLine($"Server started at http://{ServerAddress}:{Core.Port}");
                Console.ReadKey();
            }
        }

        static IReadOnlyDictionary<string, double[][]> ReadFilters() => Collections.BuildDictionary(
            from key in ConfigurationManager.AppSettings.AllKeys
            where !string.IsNullOrWhiteSpace(key)
            where key.Length <= 256
            let value = ConfigurationManager.AppSettings[key]
            let array = value.TryParseMany(double.Parse)
            where array != null
            let size = array.Length
            where size.IsPerfectSquare()
            let side = size.IntegralRoot()
            let matrix = array.BuildMatrix(side, side)
            let extendedKey = key.Extend()
            select (extendedKey, matrix)
        );
    }
}
