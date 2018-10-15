using System;
using System.Collections.Generic;
using Task2Lib;
using Task2LibImpl;

namespace Task2
{
    static class Program
    {
        static void Main()
        {
            var tanks = new List<AbstractTank>
            {
                new Armata(),
                new Sprut(),
                new CustomTank("Custom tank", "Mars Mining Company", 200, 12, 50, 400, 500)
            };
            foreach (var tank in tanks)
            {
                Console.WriteLine(tank.GetFullInfo());
            }
            Console.ReadKey();
        }
    }
}
