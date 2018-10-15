using System;
using System.Collections.Generic;

namespace Task4
{
    class Program
    {
        static void Main(string[] args)
        {
            
            ILinkedList<int> list = new LinkedList<int>();
            Console.Write("Created list:       ");
            list.Print();

            for (int i = 0; i < 10; i++)
                list.AddToEnd(i + 1);
            Console.Write("Filled list:        ");
            list.Print();

            list.AddToStart(11);
            Console.Write("Added to start:     ");
            list.Print();

            list.PopLast(); // just the same with PopFirst
            Console.Write("Popped last:        ");
            list.Print();

            var result = list.Remove(6);
            Console.Write($"Removed 6 ({result}):   ");
            list.Print();

            result = list.Remove(42);
            Console.Write($"Removed 42 ({result}): ");
            list.Print();

            Console.WriteLine($"Contains 6:         {list.Contains(6)}");
            Console.WriteLine($"Contains 7:         {list.Contains(7)}");

            Console.ReadKey();
        }
    }
}
