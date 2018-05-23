namespace Math
{
    public struct Point
    {
        public readonly double X;
        public readonly double Y;

        public Point(double x, double y)
        {
            X = x;
            Y = y;
        }

        public override string ToString()
        {
            return $"({X}, {Y})";
        }

        public static Point operator *(Point point, double value)
        {
            return new Point(point.X * value, point.Y * value);
        }

        public static Point operator *(double value, Point point)
        {
            return point * value;
        }

        public static Point operator +(Point first, Point second)
        {
            return new Point(first.X + second.X, first.Y + second.Y);
        }

        public static Point operator /(Point point, int denominator)
        {
            return new Point(point.X / denominator, point.Y / denominator);
        }

        public static Point operator -(Point point)
        {
            return new Point(-point.X, -point.Y);
        }
    }
}
