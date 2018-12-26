using System;
using System.Collections.Generic;
using JetBrains.Annotations;

namespace Fibers.Managing
{
    /// <summary>
    /// Handles switching between fibers.
    /// (And also logging. I decided it is not worth it
    /// to move that logic to separate class)
    /// </summary>
    public interface IProcessManager : IDisposable
    {
        void Switch(bool fiberFinished);
        void Enqueue([NotNull] [ItemNotNull] IEnumerable<Process> processes);
        void Start();

        /// <summary>
        /// Write string to log.
        /// This function exists for debugging purposes only.
        /// </summary>
        /// <param name="info">Text to be logged</param>
        void Info([NotNull] string info);
    }
}