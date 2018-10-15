using System;
using System.Net;
using System.Net.Sockets;
using System.Windows;

namespace Task5
{
    public sealed partial class SettingsWindow : Window
    {
        public event EventHandler StartListeningWindowRequested;
        public event EventHandler StopListeningRequested;
        public event EventHandler ConnectWindowRequested;
        public event EventHandler DisconnectRequested;

        static string IpAddress
        {
            get
            {
                var addresses = Dns.GetHostAddresses(Dns.GetHostName());

                foreach (var address in addresses)
                {
                    if (address.AddressFamily == AddressFamily.InterNetwork)
                    {
                        return $"IP address: {address}";
                    }
                }

                return "Could not determine IPv4 address";
            }
        }

        public SettingsWindow()
        {
            InitializeComponent();

            ConnectButton.Click += (sender, args) => ConnectWindowRequested?.Invoke(this, EventArgs.Empty);
            DisconnectButton.Click += (sender, args) => DisconnectRequested?.Invoke(this, EventArgs.Empty);
            StartListeningButton.Click += (sender, args) => StartListeningWindowRequested?.Invoke(this, EventArgs.Empty);
            StopListeningButton.Click += (sender, args) => StopListeningRequested?.Invoke(this, EventArgs.Empty);
            DoneButton.Click += (sender, args) => Close();
        }
        
        public void InvalidateInformation(IClientInformation client)
        {
            if (client is null)
            {
                throw new ArgumentException(nameof(client));
            }
            
            IpAddressScreen.Text = IpAddress;

            ListeningPortScreen.Text =
                $"Listening Port: {(client.ListeningPort >= 0 ? client.ListeningPort.ToString() : "None")}";

            IncomingConnectionsScreen.Text = $"Incoming connections: {client.IncomingConnectionsCount}";

            OutcomingConnectionsScreen.Text = $"Outcoming connection: {client.OutcomingConnectionIp ?? "None"}";

            DisconnectButton.IsEnabled = client.HasOutcomingConnection;

            ConnectButton.IsEnabled = !client.HasOutcomingConnection;

            StartListeningButton.IsEnabled = !client.IsListening;

            StopListeningButton.IsEnabled = client.IsListening;
        }
    }
}
