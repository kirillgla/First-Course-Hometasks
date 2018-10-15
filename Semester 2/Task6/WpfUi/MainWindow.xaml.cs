using System.Collections.Generic;
using Math;

namespace WpfUi
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow
    {
        public MainWindow()
        {
            InitializeComponent();

            int pixelsInUnit = 64;

            var painter = new WpfPainter(Canvas);

            FunctionSelectionBox.ItemsSource = new List<CurveInfo>
            {
                new CentralCircleInfo(2),
                new EllipticCurveInfo()
            };

            FunctionSelectionBox.SelectionChanged += (a, b) =>
                painter.Paint(b.AddedItems[0] as CurveInfo, pixelsInUnit);

            PlusButton.Click += (sender, args) =>
            {
                if (pixelsInUnit < 256)
                {
                    pixelsInUnit *= 2;
                    PlusButton.IsEnabled = true;
                    MinusButton.IsEnabled = true;
                }
                else
                {
                    pixelsInUnit = 512;
                    PlusButton.IsEnabled = false;
                    MinusButton.IsEnabled = true;
                }

                painter.Paint(FunctionSelectionBox.SelectionBoxItem as CurveInfo, pixelsInUnit);
            };

            MinusButton.Click += (sender, args) =>
            {
                if (pixelsInUnit > 32)
                {
                    pixelsInUnit /= 2;
                    PlusButton.IsEnabled = true;
                    MinusButton.IsEnabled = true;
                }
                else
                {
                    pixelsInUnit = 16;
                    PlusButton.IsEnabled = true;
                    MinusButton.IsEnabled = false;
                }

                painter.Paint(FunctionSelectionBox.SelectionBoxItem as CurveInfo, pixelsInUnit);
            };

            SizeChanged += (sender, args) => painter.Paint(FunctionSelectionBox.SelectionBoxItem as CurveInfo, pixelsInUnit);
        }
    }
}
