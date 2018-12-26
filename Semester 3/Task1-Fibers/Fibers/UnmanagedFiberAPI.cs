using System.Runtime.InteropServices;

namespace Fibers
{
    public static class UnmanagedFiberApi
    {
        public delegate uint LpfiberStartRoutine(uint param);

        [DllImport("Kernel32.dll")]
        public static extern uint ConvertThreadToFiber(uint lpParameter);

        [DllImport("Kernel32.dll")]
        public static extern void SwitchToFiber(uint lpFiber);

        [DllImport("Kernel32.dll")]
        public static extern void DeleteFiber(uint lpFiber);

        [DllImport("Kernel32.dll")]
        public static extern uint CreateFiber(uint dwStackSize, LpfiberStartRoutine lpStartAddress, uint lpParameter);
    }
}
