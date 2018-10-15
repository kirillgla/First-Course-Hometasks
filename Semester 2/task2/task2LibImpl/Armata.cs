using Task2Lib;

using static Task2Lib.Util;

namespace Task2LibImpl
{
    public class Armata : AbstractTank
    {
        double MachineGun1 { get; } // mm
        double MachineGun2 { get; } // mm

        public Armata():
            base("Armata", "Russia", 48, 3.5, 10.8, 125, 80)
        {
            MachineGun1 = 12.7;
            MachineGun2 = 7.62;
        }

        public override string GetFullInfo() =>
            base.GetFullInfo() +
            $"First machine gun caliber: {MachineGun1} mm{N}" +
            $"Second machine gun caliber: {MachineGun2} mm{N}";
    }
}
