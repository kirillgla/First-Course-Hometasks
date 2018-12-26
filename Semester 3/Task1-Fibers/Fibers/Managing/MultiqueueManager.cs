using System;
using System.Collections.Generic;
using System.Linq;
using Extensions;
using Fibers.Collections;
using JetBrains.Annotations;

namespace Fibers.Managing
{
    public sealed class MultiqueueManager : IProcessManager
    {
        bool disposed;
        bool started;
        uint MainFiber { get; }

        [NotNull]
        [ItemNotNull]
        PriorityAwareQueue<Process>[] Queues { get; }

        [CanBeNull]
        Process CurrentlyExecutingProcess { get; set; }

        [NotNull]
        IPriorityConverter Converter { get; }

        [NotNull]
        [ItemNotNull]
        IList<Process> FinishedProcesses { get; } = new List<Process>();

        [NotNull]
        IDictionary<Process, Fiber> FibersMap { get; } = new Dictionary<Process, Fiber>();

        [NotNull] Random random = new Random();

        public MultiqueueManager(uint mainFiber, [CanBeNull] IPriorityConverter converter = null)
        {
            MainFiber = mainFiber;
            Converter = converter ?? new ExponentPriorityConverter();

            Queues = new PriorityAwareQueue<Process>[Process.PriorityLevelsNumber];
            for (int index = 0; index < Process.PriorityLevelsNumber; index += 1)
            {
                Queues[index] = new PriorityAwareQueue<Process>(index);
            }
        }

        public void Switch(bool fiberFinished)
        {
            if (fiberFinished)
            {
                if (CurrentlyExecutingProcess == null)
                {
                    throw new InvalidOperationException("Attempted to finish nonexistent process");
                }

                FinishedProcesses.Add(CurrentlyExecutingProcess);
                LaunchNextFiber();
            }
            else
            {
                if (CurrentlyExecutingProcess != null)
                {
                    Queues[CurrentlyExecutingProcess.Priority].Enqueue(CurrentlyExecutingProcess);
                    Info("Process enqueued");
                }

                LaunchNextFiber();
            }
        }

        void LaunchNextFiber()
        {
            if (!HasProcesses)
            {
                Info("There's no next fiber!");
                Fiber.Switch(MainFiber, this);
            }

            int queueIndex = SelectQueue();
            var queue = Queues[queueIndex];
            if (queue.IsEmpty)
            {
                string message =
                    "Internal error: attempted to select item from empty queue.\n" +
                    $"Index = {queueIndex},\n" +
                    $"Queues: {Queues.JoinToString()}";
                throw new InvalidOperationException(message);
            }

            var process = queue.Dequeue();
            CurrentlyExecutingProcess = process;
            var fiber = FibersMap[process] ?? throw new InvalidOperationException("There's no fiber in fiber map!");
            Fiber.Switch(fiber.Id, this);
        }

        bool HasProcesses => Queues.Any(it => it.IsNotEmpty);

        int SelectQueue()
        {
            int sum = Queues.Where(it => it.IsNotEmpty).Sum(it => Converter.Convert(it.Priority));
            int selected = random.Next(sum);
            int index = 0;
            foreach (var queue in Queues)
            {
                if (!queue.IsNotEmpty)
                {
                    index += 1;
                    continue;
                }

                selected -= Converter.Convert(queue.Priority);
                if (selected <= 0)
                {
                    return index;
                }

                index += 1;
            }

            throw new InvalidOperationException("Internal error: could not select next queue");
        }

        public void Enqueue(IEnumerable<Process> processes) => processes.ForEach(CreateFiber);

        void CreateFiber([NotNull] Process process)
        {
            var fiber = new Fiber(process.Run, this);
            Info($"Secondary fiber: {fiber.Id}, priority: {process.Priority}");
            FibersMap.Add(process, fiber);
            Queues[process.Priority].Enqueue(process);
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

            Queues.ForEach(Kill);
            FinishedProcesses.ForEach(Kill);
            FibersMap.Clear();
            disposed = true;
        }

        void Kill([NotNull] PriorityAwareQueue<Process> queue)
        {
            while (!queue.IsEmpty)
            {
                Kill(queue.Dequeue());
            }
        }

        void Kill([NotNull] Process process) => FibersMap[process]?.Delete();
    }
}
