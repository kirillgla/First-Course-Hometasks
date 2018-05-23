using System;
using System.ComponentModel;
using Task5.Events;

namespace Task5
{
    public class SettingsWindowModel
    {
        public event EventHandler Closing;

        MainWindowModel Context { get; }
        SettingsWindow Window { get; }
        internal AbstractClient Client => Context.Client;

        public SettingsWindowModel(MainWindowModel context)
        {
            Context = context;
            Context.Closing += OnContextClosing;

            Window = new SettingsWindow();
            Window.InvalidateInformation(Client);

            Window.Closing += OnWindowClosing;
            Window.StartListeningWindowRequested += OnStartListeningWindowRequested;
            Window.StopListeningRequested += OnStopListeningRequested;
            Window.ConnectWindowRequested += OnConnectWindowRequested;
            Window.DisconnectRequested += OnDisconnectRequested;

            Client.ConnectionsCountChanged += OnClientConnectionsCountChanged;
            Client.ListeningStateChanged += OnClientListeningStateChanged;
        }

        void OnClientListeningStateChanged(object sender, EventArgs e) => Window.InvalidateInformation(Client);

        void OnClientConnectionsCountChanged(object sender, ConnectionsCountChangedEventArgs args) =>
            Window.InvalidateInformation(Client);

        public void Show() => Window.Show();

        void OnContextClosing(object sender, EventArgs args) => Window.Close();

        void OnWindowClosing(object sender, CancelEventArgs args)
        {
            Closing?.Invoke(this, EventArgs.Empty);
            Context.Closing -= OnContextClosing;
            Client.ConnectionsCountChanged -= OnClientConnectionsCountChanged;
        }

        void OnStartListeningWindowRequested(object sender, EventArgs args)
        {
            Window.StartListeningButton.IsEnabled = false;
            var startListening = new StartListeningWindowModel(this);
            startListening.Closing += (o, eventArgs) => Window.InvalidateInformation(Client);
            startListening.Show();
        }

        void OnStopListeningRequested(object sender, EventArgs args)
        {
            Client.StopListening();
        }

        void OnConnectWindowRequested(object sender, EventArgs args)
        {
            Window.ConnectButton.IsEnabled = false;
            var connect = new ConnectWindowModel(this);
            connect.Closing += (o, eventArgs) => Window.InvalidateInformation(Client);
            connect.Show();
        }

        void OnDisconnectRequested(object sender, EventArgs args)
        {
            Client.Disconnect();
        }
    }
}
