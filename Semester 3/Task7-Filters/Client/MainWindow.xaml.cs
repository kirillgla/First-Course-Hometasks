using System;
using System.Collections.ObjectModel;
using System.Linq;
using System.Net.Sockets;
using System.Windows;
using System.Windows.Controls;
using Microsoft.Win32;
using Tools;
using Tools.Extensions;

namespace Client
{
    public sealed partial class MainWindow
    {
        ObservableCollection<Filter> Filters { get; }

        TcpClient Client { get; }

        public MainWindow()
        {
            Filters = new ObservableCollection<Filter>();
            try
            {
                Client = new TcpClient(Core.Ip, Core.Port);
                var stream = Client.GetStream();
                var filters = Utils.ReadFilters(stream);
                Filters.AddAll(filters.Select(name => new Filter(name)));
            }
            catch (Exception e)
            {
                MessageBox.Show($"Error: {e.Message}");
                Close();
            }

            InitializeComponent();
            FiltersList.ItemsSource = Filters;
        }

        MainWindow SoftClone() => new MainWindow
        {
            SourcePathTextBox = {Text = SourcePathTextBox.Text},
            DestinationPathTextBox = {Text = DestinationPathTextBox.Text}
        };

        void OnCancelClicked(object sender, RoutedEventArgs e)
        {
            Client.GetStream().WriteAsync(0xFF);
            Restart();
        }

        void Restart()
        {
            Client.Close();
            SoftClone().Show();
            Close();
        }

        void ListViewItemClicked(object sender, RoutedEventArgs routedEventArgs)
        {
            if (!(sender is ListViewItem item))
            {
                return;
            }

            if (!item.IsSelected)
            {
                return;
            }
            
            if (!(item.Content is Filter filter))
            {
                return;
            }

            SendRequest(filter);
        }

        async void SendRequest(Filter filter)
        {
            var request = TryBuildRequest(filter);
            if (request is null)
            {
                return;
            }

            FiltersList.IsEnabled = false;
            CancelButton.IsEnabled = true;
            try
            {
                await request.Handle(Client.GetStream(), OnProgressChanged);
                Restart();
            }
            catch (Exception)
            {
                // Ignore
            }
        }

        void OnProgressChanged(byte progress) => ProgressTextBlock.Text = $"Progress: {progress}%";

        Request TryBuildRequest(Filter filter)
        {
            try
            {
                var builder = new Request.Builder();
                builder.SetSource(SourcePathTextBox.Text);
                builder.SetDestination(DestinationPathTextBox.Text);
                builder.SetFilter(filter.Name);
                return builder.Build();
            }
            catch (ArgumentException e)
            {
                if (!string.IsNullOrEmpty(e.Message))
                {
                    MessageBox.Show(e.Message);
                }

                return null;
            }
        }

        void SelectSourceClicked(object sender, RoutedEventArgs e) => SelectImage(SourcePathTextBox);

        void SelectDestinationClicked(object sender, RoutedEventArgs e) => SelectImage(DestinationPathTextBox);

        static void SelectImage(TextBox destination)
        {
            var dialog = new OpenFileDialog
            {
                DefaultExt = ".bmp",
                Filter = "BMP files (*.bmp)|*.bmp"
            };

            bool? result = dialog.ShowDialog();

            if (result != true)
            {
                return;
            }

            string filename = dialog.FileName;
            destination.Text = filename;
        }
    }
}
