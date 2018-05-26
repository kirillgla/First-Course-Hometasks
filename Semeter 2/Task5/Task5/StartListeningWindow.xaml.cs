using System;
using System.Windows;
using Task5.Events;

namespace Task5
{
    public sealed partial class StartListeningWindow : Window
    {
        public event EventHandler<TextUpdatedEventArgs> PortTextUpdated;
        public event EventHandler<StartListeningRequestedEventArgs> StartListeningRequested;

        public bool IsDoneButtonEnabled
        {
            set => DoneButton.IsEnabled = value;
        }
        
        public StartListeningWindow()
        {
            InitializeComponent();

            PortInputBox.TextChanged += (obj, args) =>
                PortTextUpdated?.Invoke(this, new TextUpdatedEventArgs(PortInputBox.Text));

            CancelButton.Click += (obj, args) => Close();

            DoneButton.Click += (sender, args) =>
                StartListeningRequested?.Invoke(this,
                    new StartListeningRequestedEventArgs(int.Parse(PortInputBox.Text)));
        }
    }
}
