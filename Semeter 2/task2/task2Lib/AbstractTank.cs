using static Task2Lib.Util;

namespace Task2Lib
{
    public abstract class AbstractTank
    {
        string Name { get; }
        string Manufacturer { get; }
        double Mass { get; } // tons
        double Width { get; } // meters
        double Length { get; } // meters
        double MainCaliber { get; } // mm
        int MaxSpeed { get; } // km/h

        protected AbstractTank(string name, string manufacturer, double mass, double width, double length, double mainCaliber, int maxSpeed)
        {
            Name = name;
            Manufacturer = manufacturer;
            Mass = mass;
            Width = width;
            Length = length;
            MainCaliber = mainCaliber;
            MaxSpeed = maxSpeed;
        }

        public virtual string GetFullInfo() => $"Name: {Name}{N}" +
            $"Manufacturer: {Manufacturer}{N}" +
            $"Mass: {Mass} tons{N}" +
            $"Width: {Width} m{N}" +
            $"Length: {Length} m{N}" +
            $"Main gun caliber: {MainCaliber} mm{N}" +
            $"Max speed: {MaxSpeed} km/h{N}";
    }
}
