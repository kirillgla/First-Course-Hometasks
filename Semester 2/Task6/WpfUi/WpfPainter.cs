using System.Windows;
using Math;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Shapes;
using Point = Math.Point;

namespace WpfUi
{
    sealed class WpfPainter : Painter
    {
        Panel panel;

        SolidColorBrush brush;

        public WpfPainter(Panel panel)
        {
            this.panel = panel;
            brush = new SolidColorBrush(Colors.Green);
        }

        protected override double ScreenWidth => panel.ActualWidth;

        protected override double ScreenHeight => panel.ActualHeight;

        protected override void Clear()
        {
            panel.Children.Clear();
        }

        protected override void PaintAxes()
        {
            panel.Children.Add(new Line
            {
                Stroke = Brushes.LightSteelBlue,
                X1 = ScreenWidth / 2,
                X2 = ScreenWidth / 2,
                Y1 = 0,
                Y2 = ScreenHeight,
                StrokeThickness = 2
            });

            panel.Children.Add(new Line
            {
                Stroke = Brushes.LightSteelBlue,
                X1 = 0,
                X2 = ScreenWidth,
                Y1 = ScreenHeight / 2,
                Y2 = ScreenHeight / 2,
                StrokeThickness = 2
            });
        }

        protected override void PaintLine(Point start, Point end)
        {
            panel.Children.Add(new Line
            {
                Stroke = brush,
                X1 = start.X,
                X2 = end.X,
                Y1 = start.Y,
                Y2 = end.Y,
                StrokeThickness = LineThickness
            });
        }

        protected override void PaintDot(Point dot)
        {
            var currentDot = new Ellipse
            {
                Stroke = brush,
                StrokeThickness = LineThickness
            };
            Panel.SetZIndex(currentDot, 3);
            currentDot.Height = LineThickness;
            currentDot.Width = LineThickness;
            currentDot.Fill = new SolidColorBrush(Colors.Green);
            currentDot.Margin = new Thickness(dot.X, dot.Y, 0, 0);
            panel.Children.Add(currentDot);
        }
    }
}
