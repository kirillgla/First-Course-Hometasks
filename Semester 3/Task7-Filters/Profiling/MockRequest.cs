using System;
using System.Reflection;
using System.Threading.Tasks;
using Client;
using Tools.Extensions;

namespace Profiling
{
    public sealed class MockRequest : RequestBase
    {
        string ResourceName { get; }

        public MockRequest(string resourceName, string filter) : base(filter) => ResourceName = resourceName;

        protected override async Task<byte[]> GetSource()
        {
            var assembly = Assembly.GetExecutingAssembly();
            using (var stream = assembly.GetManifestResourceStream(ResourceName))
            {
                if (stream is null)
                {
                    throw new InvalidOperationException($"There is no such resource: {ResourceName}");
                }

                byte[] result = new byte[stream.Length];
                await stream.ReadAsync(result, 0, result.Length);
                return result;
            }
        }

        protected override Task SaveResult(byte[] result) => Tasks.Noop;
    }
}
