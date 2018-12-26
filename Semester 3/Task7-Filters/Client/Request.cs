using System;
using System.Threading.Tasks;
using System.Windows;
using Tools;

namespace Client
{
    public sealed class Request : RequestBase
    {
        string Source { get; }
        string Destination { get; }

        Request(string source, string destination, string filter) : base(filter)
        {
            Source = source;
            Destination = destination;
        }

        protected override Task<byte[]> GetSource() => File.ReadAllBytesAsync(Source);

        protected override Task SaveResult(byte[] result) => File.WriteAllBytesAsync(Destination, result);

        public sealed class Builder
        {
            string Source { get; set; }
            string Destination { get; set; }
            string Filter { get; set; }

            public Builder SetSource(string source)
            {
                if (!System.IO.File.Exists(source))
                {
                    throw new ArgumentException("Source file does not exist");
                }

                Source = source;
                return this;
            }

            public Builder SetDestination(string destination)
            {
                if (System.IO.File.Exists(destination))
                {
                    var result = MessageBox.Show("Destination file already exists. Override?",
                        "Confirmation",
                        MessageBoxButton.YesNo,
                        MessageBoxImage.Question
                    );
                    if (result != MessageBoxResult.Yes)
                    {
                        throw new ArgumentException("");
                    }
                }

                Destination = destination;
                return this;
            }

            public Builder SetFilter(string filter)
            {
                if (string.IsNullOrWhiteSpace(filter))
                {
                    throw new ArgumentException("Attempting to select empty filter");
                }

                Filter = filter;
                return this;
            }

            public Request Build()
            {
                if (Source is null || Destination is null || Filter is null)
                {
                    throw new InvalidOperationException("Builder was not built completely");
                }

                return new Request(Source, Destination, Filter);
            }
        }
    }
}
