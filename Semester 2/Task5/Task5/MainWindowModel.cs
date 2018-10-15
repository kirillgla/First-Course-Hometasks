using System;
using System.ComponentModel;
using System.Windows;
using Task5.Events;

namespace Task5
{
    public class MainWindowModel
    {
        public event EventHandler Closing;
        public event EventHandler<ConnectionsCountChangedEventArgs> ConnectionsCountChanged;

        internal AbstractClient Client { get; }

        MainWindow Window { get; }

        string Login { get; }

        public MainWindowModel(string login)
        {
            Login = login;
            Client = new Client();
            Window = new MainWindow {Title = Login};

            Window.Closing += OnWindowClosing;
            Window.SendRequested += OnSendRequested;
            Window.MessageTextChanged += (sender, args) =>
                Window.IsSendButtonEnabled = Client.HasConnections && !string.IsNullOrEmpty(args.NewText);
            Window.SettingsWindowRequested += OnSettingsWindowRequested;

            Client.MessageReceived += (sender, args) => Window.AddMessage(args.Message);
            Client.ConnectionsCountChanged += (sender, args) => ConnectionsCountChanged?.Invoke(sender, args);

            ConnectionsCountChanged += (sender, args) => Window.SetConnectionsCount(args.Count);
        }

        public void Show() => Window.Show();

        void OnWindowClosing(object sender, CancelEventArgs args)
        {
            Client.SendDisconnectNotifications();
            
            if (Client.IsListening)
            {
                Client.StopListening();
            }

            if (Client.IncomingConnectionsCount != 0)
            {
                Client.TerminateIncomingConnections();
            }

            if (Client.HasOutcomingConnection)
            {
                Client.Disconnect();
            }

            Closing?.Invoke(this, EventArgs.Empty);
        }

        async void OnSendRequested(object sender, SendRequestedEventArgs args)
        {
            string message = $"[{Login}] {args.Message}";
            Window.ClearInput();

            try
            {
                await Client.Send(message);
            }
            catch (Exception e)
            {
                MessageBox.Show(e.ToString(), "Error");
                return;
            }

            Window.AddMessage(message);
        }

        void OnSettingsWindowRequested(object sender, EventArgs args)
        {
            Window.IsSettingsButtonEnabled = false;
            var settings = new SettingsWindowModel(this);
            settings.Closing += (o, eventArgs) => Window.IsSettingsButtonEnabled = true;
            settings.Show();
        }
    }
}
