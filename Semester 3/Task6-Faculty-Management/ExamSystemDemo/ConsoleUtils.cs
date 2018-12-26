using System;
using System.Linq;

namespace ExamSystemDemo
{
    public static class ConsoleUtils
    {
        public static void PrettyPrintAllStats(int total, ProfileResult profileResult)
        {
            Console.WriteLine($"Total: {total} ms");
            Console.WriteLine($"Execution time of batch of up to {Program.TaskSize} operations:");
            Console.WriteLine($"|{"Task",-20}|{"Minimum",-20}|{"Average",-20}|{"Maximum",-20}|");
            PrettyPrintStats("Contains", profileResult.ContainsRequests);
            PrettyPrintStats("Add", profileResult.AddRequests);
            PrettyPrintStats("Remove", profileResult.RemoveRequests);
            Console.WriteLine();
        }

        public static void PrettyPrintStats(string name, int[] data)
        {
            const int offset = 20;
            int min = data.Min();
            int max = data.Max();
            int average = (int) data.Average();
            // Yeah, right. That's interpolated strings inside interpolated string. Sorry.
            string message = $"|{name,-offset}|{$"{min} ms",-offset}|{$"{average} ms",-offset}|{$"{max} ms",-offset}|";
            Console.WriteLine(message);
        }
    }
}
