using System;
using System.Collections.Generic;
using System.Threading;
using Fibers.Managing;
using JetBrains.Annotations;

namespace Fibers
{
    public sealed class Process
    {
        [NotNull] static readonly Random Random = new Random();
        const int LongPauseBoundary = 10000;
        const int ShortPauseBoundary = 100;
        const int WorkBoundary = 1000;
        const int IntervalsAmountBoundary = 10;
        public const int PriorityLevelsNumber = 10;

        [NotNull]
        IReadOnlyList<int> WorkIntervals { get; }

        [NotNull]
        IReadOnlyList<int> PauseIntervals { get; }

        [NotNull]
        IProcessManager Manager { get; }

        public uint Priority { get; }
        // public int TotalDuration => ActiveDuration + PauseIntervals.Sum();
        // public int ActiveDuration => WorkIntervals.Sum();

        public Process([NotNull] IProcessManager manager)
        {
            Manager = manager;
            int amount = Random.Next(IntervalsAmountBoundary);

            var workIntervals = new List<int>();
            var pauseIntervals = new List<int>();

            for (int i = 0; i < amount; i++)
            {
                workIntervals.Add(CreateWorkInterval());
                pauseIntervals.Add(CreatePauseInterval());
            }

            WorkIntervals = workIntervals;
            PauseIntervals = pauseIntervals;

            // disallow 0 priority
            Priority = (uint) Random.Next(PriorityLevelsNumber - 1) + 1;
        }

        public void Run()
        {
            for (int index = 0; index < WorkIntervals.Count; index++)
            {
                EmulateWork(index);
                EmulateIo(index);
            }

            Manager.Switch(true);
        }

        void EmulateWork(int index)
        {
            Manager.Info("Performing calculations...");
            Thread.Sleep(WorkIntervals[index]);
        }

        void EmulateIo(int index)
        {
            Manager.Info("Performing IO...");
            var pauseBeginTime = DateTime.Now;
            do
            {
                Manager.Info("Still performing IO...");
                Manager.Switch(false);
            }
            while ((DateTime.Now - pauseBeginTime).TotalMilliseconds < PauseIntervals[index]);
        }

        static int CreateWorkInterval() => Random.Next(WorkBoundary);

        static int CreatePauseInterval() =>
            Random.Next(Random.NextDouble() > 0.9 ? LongPauseBoundary : ShortPauseBoundary);
    }
}
