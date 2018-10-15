using Task2Lib;

using static Task2Lib.Util;

namespace Task2LibImpl
{
    public class Sprut : AbstractTank
    {
        double MachineGun { get; }

        public Sprut():
            base("Sprut-SD", "Russia", 18, 3.1, 9.7, 125, 70)
        {
            MachineGun = 7.62;
        }

        public override string GetFullInfo() =>
            base.GetFullInfo() +
            $"Machine gun calber: {MachineGun} mm{N}";
    }
}
