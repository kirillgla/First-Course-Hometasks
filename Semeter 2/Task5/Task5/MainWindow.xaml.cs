using System;
using System.Windows;
using Task5.Events;

namespace Task5
{
    public sealed partial class MainWindow : Window
    {
        public event EventHandler<SendRequestedEventArgs> SendRequested;
        public event EventHandler<TextUpdatedEventArgs> MessageTextChanged;
        public event EventHandler SettingsWindowRequested;

        public bool IsSendButtonEnabled
        {
            // get => SendButton.IsEnabled;
            set => SendButton.IsEnabled = value;
        }

        public bool IsSettingsButtonEnabled
        {
            // get => SettingsButton.IsEnabled;
            set => SettingsButton.IsEnabled = value;
        }

        public void SetConnectionsCount(int connections)
        {
            if (connections < 0)
            {
                throw new ArgumentException(nameof(connections));
            }

            ConnectiosScreen.Text = $"Connections: {connections}";
        }

        public void AddMessage(string message)
        {
            ChatScreen.Text += $"{Environment.NewLine}{message}";
        }

        public void ClearInput()
        {
            InputBox.Text = "";
        }

        public MainWindow()
        {
            InitializeComponent();

            ExitApplicationButton.Click += (sender, args) => Close();

            SettingsButton.Click += (sender, args) => SettingsWindowRequested?.Invoke(this, EventArgs.Empty);

            SendButton.Click += (sender, args) =>
                SendRequested?.Invoke(this, new SendRequestedEventArgs(InputBox.Text));

            InputBox.TextChanged += (sender, args) =>
                MessageTextChanged?.Invoke(sender, new TextUpdatedEventArgs(InputBox.Text));
        }
    }
}
