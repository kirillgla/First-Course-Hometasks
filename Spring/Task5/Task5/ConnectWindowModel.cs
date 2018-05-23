using System;
using System.Net;
using System.Windows;
using Task5.Events;

namespace Task5
{
    public class ConnectWindowModel
    {
        public event EventHandler Closing;

        SettingsWindowModel Context { get; }
        ConnectWindow Window { get; }
        AbstractClient Client => Context.Client;

        bool hasValidIp = true;
        bool hasValidPort = true;

        public ConnectWindowModel(SettingsWindowModel context)
        {
            Context = context;
            Context.Closing += OnContextClosing;

            Window = new ConnectWindow();
            Window.Closing += OnWindowClosing;
            Window.IpTextUpdated += OnIpTextUpdated;
            Window.PortTextUpdated += OnPortTextUpdated;
            Window.ConnectionRequested += OnConnectionRequested;
        }

        public void Show() => Window.Show();

        void OnContextClosing(object sender, EventArgs eventArgs) => Window.Close();

        void OnWindowClosing(object sender, EventArgs eventArgs)
        {
            Closing?.Invoke(this, EventArgs.Empty);
            Context.Closing -= OnContextClosing;
        }

        void OnIpTextUpdated(object sender, TextUpdatedEventArgs args)
        {
            string text = args.NewText;
            hasValidIp = !string.IsNullOrWhiteSpace(text) && text.Split('.').Length == 4 &&
                         IPAddress.TryParse(text, out _);
            Window.DoneButton.IsEnabled = hasValidIp && hasValidPort;
        }

        void OnPortTextUpdated(object sender, TextUpdatedEventArgs args)
        {
            string text = args.NewText;
            hasValidPort = int.TryParse(text, out _);
            Window.DoneButton.IsEnabled = hasValidIp && hasValidPort;
        }

        async void OnConnectionRequested(object sender, ConnectionRequestedEventArgs args)
        {
            Window.IpInputBox.IsEnabled = false;
            Window.PortInputBox.IsEnabled = false;
            Window.CancelButton.IsEnabled = false;
            Window.DoneButton.IsEnabled = false;

            try
            {
                int port = int.Parse(Window.PortInputBox.Text);
                string ip = Window.IpInputBox.Text;
                await Client.Connect(ip, port);
            }
            catch (Exception e)
            {
                MessageBox.Show(e.Message, "Error");
                Window.DoneButton.IsEnabled = true;
                Window.CancelButton.IsEnabled = true;
                Window.IpInputBox.IsEnabled = true;
                Window.PortInputBox.IsEnabled = true;
                return;
            }
            
            Window.Close();
        }
    }
}
