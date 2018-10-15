using System.Text.RegularExpressions;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;

namespace Task5
{
    // This class is too simple
    // to introduce complexity
    // related with ViewModel 
    public sealed partial class LoginWindow : Window
    {
        public LoginWindow()
        {
            InitializeComponent();

            NameInputTextBox.TextChanged += InputNameChanged;

            ExitButton.Click += (sender, args) => Close();

            LoginButton.Click += (sender, args) =>
            {
                new MainWindowModel(NameInputTextBox.Text).Show();
                
                // After this MainWindowModel turns into closure,
                // inaccessible for anyone, but still alive,
                // since MainWindow has references to it's methods
                
                Close();
            };
        }

        void InputNameChanged(object sender, TextChangedEventArgs textChangedEventArgs)
        {
            if (!string.IsNullOrEmpty(NameInputTextBox.Text))
            {
                NameInputTextBox.Background = (SolidColorBrush) FindResource("BaseBackground");
                LoginButton.IsEnabled = Regex.IsMatch(NameInputTextBox.Text, @"\A\S{3,}\z");
            }
            else
            {
                NameInputTextBox.Background = null;
                LoginButton.IsEnabled = false;
            }
        }
    }
}
