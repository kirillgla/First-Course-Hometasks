using System;
using System.Windows;
using Task5.Events;

namespace Task5
{
    public partial class ConnectWindow : Window
    {
        public event EventHandler<TextUpdatedEventArgs> IpTextUpdated;
        public event EventHandler<TextUpdatedEventArgs> PortTextUpdated;
        public event EventHandler<ConnectionRequestedEventArgs> ConnectionRequested;

        public ConnectWindow()
        {
            InitializeComponent();

            IpInputBox.TextChanged += (sender, args) =>
                IpTextUpdated?.Invoke(this, new TextUpdatedEventArgs(IpInputBox.Text));

            PortInputBox.TextChanged += (sender, args) =>
                PortTextUpdated?.Invoke(this, new TextUpdatedEventArgs(PortInputBox.Text));

            DoneButton.Click += (sender, args) =>
                ConnectionRequested?.Invoke(this,
                    new ConnectionRequestedEventArgs(IpInputBox.Text, int.Parse(PortInputBox.Text)));

            CancelButton.Click += (sender, args) => Close();
        }
    }
}
