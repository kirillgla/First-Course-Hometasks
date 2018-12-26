using System;
using System.Collections.Generic;
using Fibers;
using Fibers.Collections;
using Fibers.Managing;
using JetBrains.Annotations;

namespace FiberDemo
{
    static class Program
    {
        const uint NumberOfTasks = 3;

        public static void Main()
        {
            uint mainFiber = UnmanagedFiberApi.ConvertThreadToFiber(0);
            using (var manager = GetManager(mainFiber))
            {
                manager.Info($"Main fiber: {mainFiber}");
                manager.Enqueue(CreateTasks(manager, NumberOfTasks));
                Console.WriteLine("Press any key to start...");
                Console.ReadKey();
                manager.Start();
                manager.Info("Returned from manager");
            }
        }

        // change this method to use another manager
        [NotNull]
        static IProcessManager GetManager(uint mainFiber) => new MultiqueueManager(mainFiber);

        [NotNull]
        [ItemNotNull]
        static IEnumerable<Process> CreateTasks([NotNull] IProcessManager manager, uint count)
        {
            for (uint index = 0; index < count; index += 1)
            {
                yield return new Process(manager);
            }
        }
    }
}