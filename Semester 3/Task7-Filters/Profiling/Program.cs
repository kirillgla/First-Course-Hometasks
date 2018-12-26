using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Sockets;
using System.Threading.Tasks;
using Client;
using Tools;
using Tools.Extensions;

namespace Profiling
{
    static class Program
    {
        // 1.
        // static async Task Main() => await RunInfoClientsMany(10, 1, "Profiling.small.bmp");
        // static async Task Main() => (await RunClients(20, "Profiling.small.bmp")).ForEach(span => Console.WriteLine(span.Milliseconds));
        // static async Task Main() => (await RunClients(70, "Profiling.small.bmp")).ForEach(span => Console.WriteLine(span.Milliseconds));
        //
        // 2.
        // first 10 launches are for heat-up
        // static async Task Main() => await RunClientsManyTimes("Profiling.small.bmp", 10, 1, 20, 70);
        // static async Task Main() => await RunClientsManyTimes("Profiling.average.bmp", 10, 1, 20, 70);
        static async Task Main() => await RunClientsManyTimes("Profiling.big.bmp", 10, 1, 20, 70);
        //
        // 3.
        // static Task Main() => RunInfoClients(100000000, "Profiling.small.bmp");
        // ^ runs relatively fine, although very slowly ^
        // static Task Main() => RunInfoClients(1000000000, "Profiling.small.bmp");
        // ^ causes OutOfMemoryException ^

        static async Task RunClientsManyTimes(string resourceName, params int[] counts)
        {
            foreach (int count in counts)
            {
                await RunInfoClients(count, resourceName);
            }
        }

        static async Task RunInfoClientsMany(int launches, int clientsPerLaunch, string resourceName)
        {
            for (int index = 0; index < launches; index += 1)
            {
                await RunInfoClients(clientsPerLaunch, resourceName);
            }
        }
        
        static async Task RunInfoClients(int count, string resourceName)
        {
            Console.WriteLine($@"Number of clients: {count}");
            var milliseconds = (
                from timeSpan in await RunClients(count, resourceName)
                select timeSpan.Milliseconds
            ).ToArray();
            Console.WriteLine($@"Minimal response time: {milliseconds.Min()}");
            Console.WriteLine($@"Average response time: {milliseconds.Average()}");
            Console.WriteLine($@"Maximal response time: {milliseconds.Max()}");
            Console.WriteLine($@"Median  response time: {milliseconds.Median()}");
        }

        // It takes less than a second to launch n clients,
        // so that is approximately equal to
        // having load of n client per second
        static async Task<IList<TimeSpan>> RunClients(int count, string resourceName)
        {
            IList<TimeSpan> result = new List<TimeSpan>(count);
            var generator =
                from _ in Collections.Range(1, count)
                select RunClient(resourceName);
            var second = TimeSpan.FromSeconds(1);
            var tasks = DoQuickly(() => generator.ToArray(), second);
            foreach (var task in tasks)
            {
                result.Add(await task);
            }

            return result;
        }

        static T DoQuickly<T>(Func<T> action, TimeSpan limit)
        {
            var start = DateTime.Now;
            var result = action();
            var end = DateTime.Now;
            if (end - start <= limit)
            {
                return result;
            }

            throw new InvalidOperationException("Operation took longer than expected!");
        }

        static async Task<TimeSpan> RunClient(string resourceName)
        {
            var start = DateTime.Now;
            var client = new TcpClient(Core.Ip, Core.Port);
            var stream = client.GetStream();
            string filter = (await Utils.ReadFiltersAsync(stream)).First();
            var request = new MockRequest(resourceName, filter);
            await request.Handle(stream, _ => { });
            var end = DateTime.Now;
            return end - start;
        }
    }
}
