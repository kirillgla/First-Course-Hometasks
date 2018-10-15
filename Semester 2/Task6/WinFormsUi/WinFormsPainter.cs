using Math;
using System;
using System.Drawing;
using Point = Math.Point;

namespace WinFormsUi
{
    sealed class WinFormsPainter : Painter, IDisposable
    {
        Graphics graphics;

        Pen pen;

        protected override double ScreenWidth => MainForm.WindowWidth;

        protected override double ScreenHeight => MainForm.WindowHeight;

        public WinFormsPainter(Graphics graphics)
        {
            this.graphics = graphics;
            pen = new Pen(Brushes.Black, LineThickness);
        }

        protected override void Clear()
        {
            graphics.Clear(Color.White);
        }

        protected override void PaintAxes()
        {
            var localPen = new Pen(Color.Blue, 2);
            
            // X-axis
            graphics.DrawLine(localPen, 0, ScreenHeight / 2, ScreenWidth, ScreenHeight / 2);
            
            // Y-axis
            graphics.DrawLine(localPen, ScreenWidth / 2, 0, ScreenWidth / 2, ScreenHeight);
            
            localPen.Dispose();
        }

        protected override void PaintLine(Point start, Point end)
        {
            graphics.DrawLine(pen, start.X, start.Y, start.X, end.Y);
        }

        protected override void PaintDot(Point dot)
        {
            graphics.DrawEllipse(pen, (int) dot.X, (int) dot.Y, LineThickness, LineThickness);
        }

        public void Dispose()
        {
            pen?.Dispose();
        }
    }
}
