using System.Collections.Generic;
using System.Linq;

namespace Math
{
    public abstract class Painter
    {
        protected const int LineThickness = 3;
        protected const double MinStep = double.Epsilon * 16;

        protected abstract double ScreenWidth { get; }
        protected abstract double ScreenHeight { get; }

        public void Paint(CurveInfo curve, int pixelsInUnit)
        {
            Clear();

            PaintAxes();

            if (curve is null)
            {
                return;
            }

            var zeroPoint = new Point(ScreenWidth / 2, ScreenHeight / 2);

            var lowerRight = new Point(ScreenWidth / 2, ScreenHeight / 2) / pixelsInUnit;

            var upperLeft = -lowerRight;

            var region = new Region(upperLeft, lowerRight);

            var transformedComponents =
                from component in curve.GetConnectivityComponents(region)
                let transformedComponent =
                    from point in component.Dots
                    select point * pixelsInUnit + zeroPoint
                select new ConnectivityComponent(transformedComponent);

            foreach (var component in transformedComponents)
            {
                Point? previousPoint = null;

                foreach (var point in component.Dots)
                {
                    if (IsBorderPoint(point))
                    {
                        previousPoint = null;
                        PaintDot(point);
                        continue;
                    }

                    if (previousPoint != null)
                    {
                        PaintLine(previousPoint.Value, point);
                    }

                    previousPoint = point;
                }
            }
        }

        protected virtual bool IsBorderPoint(Point point)
        {
            double step = ScreenHeight / 256;
            if (step < MinStep)
            {
                step = MinStep;
            }

            return point.Y > ScreenHeight - step ||
                   point.Y < step ||
                   point.X > ScreenWidth - step ||
                   point.X < step;
        }

        protected abstract void Clear();

        protected abstract void PaintAxes();

        protected abstract void PaintLine(Point start, Point end);

        protected abstract void PaintDot(Point dot);
    }
}
