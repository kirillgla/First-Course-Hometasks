using System;
using System.Drawing;
using System.Windows.Forms;
using Math;

namespace WinFormsUi
{
    public partial class MainForm : Form
    {
        public const int WindowWidth = 400;
        public const int WindowHeight = 400;

        WinFormsPainter painter;

        int pixelsInUnit;

        CurveInfo selectedCurve;

        bool initializes;

        public MainForm()
        {
            InitializeComponent();

            pixelsInUnit = 64;

            painter = new WinFormsPainter(CreateGraphics());

            initializes = true;

            Closing += (sender, args) => painter.Dispose();
        }

        void MainForm_Paint(object sender, PaintEventArgs e)
        {
            if (initializes)
            {
                painter.Paint(selectedCurve, pixelsInUnit);
                initializes = false;
            }
        }

        void MainForm_Load(object sender, EventArgs e)
        {
            var plusButton = new Button
            {
                Text = "+",
                Width = 80,
                Height = 24
            };
            var minusButton = new Button
            {
                Left = 80,
                Width = 80,
                Height = 24,
                Text = "-"
            };
            var comboBox = new ComboBox
            {
                Left = 160,
                Width = 80,
                DisplayMember = "Name",
                Items =
                {
                    new CentralCircleInfo(2),
                    new EllipticCurveInfo()
                },
                DropDownStyle =  ComboBoxStyle.DropDownList
            };

            plusButton.Click += (o, args) => PlusButton_Click(plusButton, minusButton, (CurveInfo) comboBox.SelectedItem);
            minusButton.Click += (o, args) => MinusButton_Click(plusButton, minusButton, (CurveInfo)comboBox.SelectedItem);

            comboBox.SelectedIndexChanged += (o, args) =>
            {
                selectedCurve = comboBox.SelectedItem as CurveInfo;
                painter.Paint(selectedCurve, pixelsInUnit);
            };

            SizeChanged += (a, b) => painter.Paint(selectedCurve, pixelsInUnit);

            Controls.Add(plusButton);
            Controls.Add(minusButton);
            Controls.Add(comboBox);

            MinimumSize = new Size(WindowWidth, WindowHeight);
            MaximumSize = new Size(WindowWidth, WindowHeight);
        }

        void PlusButton_Click(Button plus, Button minus, CurveInfo curve)
        {
            if (pixelsInUnit < 256)
            {
                pixelsInUnit *= 2;
                plus.Enabled = true;
                minus.Enabled = true;
            }
            else
            {
                pixelsInUnit = 512;
                plus.Enabled = false;
                minus.Enabled = true;
            }

            painter.Paint(curve, pixelsInUnit);
        }

        void MinusButton_Click(Button plus, Button minus, CurveInfo curve)
        {
            if (pixelsInUnit > 32)
            {
                pixelsInUnit /= 2;
                plus.Enabled = true;
                minus.Enabled = true;
            }
            else
            {
                pixelsInUnit = 16;
                plus.Enabled = true;
                minus.Enabled = false;
            }

            painter.Paint(curve, pixelsInUnit);
        }
    }
}
