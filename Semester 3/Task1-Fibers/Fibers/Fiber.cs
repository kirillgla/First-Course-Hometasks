using System;
using Fibers.Managing;
using JetBrains.Annotations;

namespace Fibers
{
    /// <summary>
    /// Represents non-primary fiber
    /// </summary>
    public sealed class Fiber
    {
        /// <summary>
        /// The fiber action delegate.
        /// </summary>
        [NotNull]
        Action Action { get; }

        [NotNull]
        IProcessManager Manager { get; }

        /// <summary>
        /// Gets the fiber identifier.
        /// </summary>
        public uint Id { get; }

        /// <summary>
        /// Initializes a new instance of the <see cref="Fiber"/> class.
        /// </summary>
        /// <param name='action'>Action.</param>
        /// <param name="manager"></param>
        public Fiber([NotNull] Action action, [NotNull] IProcessManager manager)
        {
            Action = action;
            Manager = manager;
            Id = UnmanagedFiberApi.CreateFiber(0x1000000, FiberRunnerProc, 0);
        }

        /// <summary>
        /// Deletes the current fiber.
        /// </summary>
        /// <remarks>This method should only be used in the fiber action that's executing.</remarks>
        public void Delete() => UnmanagedFiberApi.DeleteFiber(Id);

        /// <summary>
        /// Switches the execution context to the next fiber.
        /// </summary>
        /// <param name='fiberId'>Fiber id.</param>
        /// <param name="manager">The utility used for logging</param>
        public static void Switch(uint fiberId, [NotNull] IProcessManager manager)
        {
            if (fiberId == 0)
            {
                manager.Info("Attempted to switch to null fiber");
                return;
            }

            manager.Info($"Switching to fiber [{fiberId}]");
            UnmanagedFiberApi.SwitchToFiber(fiberId);
        }

        /// <summary>
        /// Fiber method that executes the fiber action.
        /// </summary>
        /// <param name='lpParam'>Lp parameter.</param>
        /// <returns>fiber status code.</returns>
        uint FiberRunnerProc(uint lpParam)
        {
            try
            {
                Action();
                Manager.Info("Dead fiber received control. Killing thread...");
                return 0;
            }
            catch (Exception e)
            {
                Manager.Info("Error in fiber action. Killing thread...");
                Manager.Info(e.ToString());
                return 1;
            }
        }
    }
}