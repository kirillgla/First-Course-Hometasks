using System;
using System.ComponentModel;
using System.Windows;
using Task5.Events;

namespace Task5
{
    public class StartListeningWindowModel
    {
        public event EventHandler Closing;
        
        SettingsWindowModel Context { get; }
        StartListeningWindow Window { get; }
        AbstractClient Client => Context.Client;

        public StartListeningWindowModel(SettingsWindowModel context)
        {
            Context = context;
            Context.Closing += OnContextClosing;

            Window = new StartListeningWindow();
            Window.StartListeningRequested += OnStartListeningRequested;
            Window.PortTextUpdated += (sender, args) =>
                Window.IsDoneButtonEnabled = int.TryParse(args.NewText, out _);
            Window.Closing += OnWindowClosing;
        }

        public void Show() => Window.Show();
        
        void OnContextClosing(object sender, EventArgs args) => Window.Close();

        void OnWindowClosing(object sender, CancelEventArgs args)
        {
            Closing?.Invoke(this, EventArgs.Empty);
            Context.Closing -= OnContextClosing;
        }

        void OnStartListeningRequested(object sender, StartListeningRequestedEventArgs args)
        {
            try
            {
                Client.StartListening(args.ListeningPort);
            }
            catch (Exception e)
            {
                MessageBox.Show(e.Message, "Error: could not occupy port");
                return;
            }

            Window.Close();
        }
    }
}
