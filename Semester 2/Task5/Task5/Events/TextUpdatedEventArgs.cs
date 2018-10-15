using System;

namespace Task5.Events
{
    public class TextUpdatedEventArgs: EventArgs
    {
        public string NewText { get; }

        public TextUpdatedEventArgs(string newText)
        {
            NewText = newText;
        }
    }
}
