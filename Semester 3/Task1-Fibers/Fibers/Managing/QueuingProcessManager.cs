using System;
using System.Collections.Generic;
using System.Linq;
using Extensions;
using JetBrains.Annotations;

namespace Fibers.Managing
{
    public class QueuingProcessManager : IProcessManager
    {
        bool disposed;
        bool started;
        uint MainFiber { get; }

        [NotNull]
        [ItemNotNull]
        LinkedList<Process> AliveProcesses { get; } = new LinkedList<Process>();

        [NotNull]
        [ItemNotNull]
        IList<Process> FinishedProcesses { get; } = new List<Process>();

        [NotNull]
        IDictionary<Process, Fiber> FibersMap { get; } = new Dictionary<Process, Fiber>();

        public QueuingProcessManager(uint mainFiber) => MainFiber = mainFiber;

        public void Switch(bool fiberFinished)
        {
            var process = AliveProcesses.DropFirst();
            if (!fiberFinished)
            {
                Info("Enqueueing fiber");
                AliveProcesses.AddLast(process);
            }
            else
            {
                FinishedProcesses.Add(process);
            }

            LaunchNextFiber();
        }

        void LaunchNextFiber()
        {
            Info("Moving to next fiber...");
            var nextProcess = AliveProcesses.FirstOrDefault();
            if (nextProcess != null)
            {
                Fiber.Switch(FibersMap[nextProcess].Id, this);
                return;
            }

            Info("There's no next fiber!");
            Info("Returning");
            Fiber.Switch(MainFiber, this);
        }

        public void Start()
        {
            if (started)
            {
                throw new InvalidOperationException();
            }

            started = true;
            Switch(false);
        }

        public void Enqueue(IEnumerable<Process> processes)
        {
            if (started)
            {
                throw new InvalidOperationException();
            }

            processes.ForEach(CreateFiber);
        }

        void CreateFiber([NotNull] Process process)
        {
            var fiber = new Fiber(process.Run, this);
            Info($"Secondary fiber: {fiber.Id}");
            FibersMap.Add(process, fiber);
            AliveProcesses.AddLast(process);
        }

        public void Info(string info)
        {
            // Console.WriteLine(info);
        }

        public void Dispose()
        {
            if (disposed)
            {
                return;
            }

            AliveProcesses.ForEach(process => FibersMap[process].Delete());
            FinishedProcesses.ForEach(process => FibersMap[process].Delete());
            FibersMap.Clear();
            disposed = true;
        }
    }
}
