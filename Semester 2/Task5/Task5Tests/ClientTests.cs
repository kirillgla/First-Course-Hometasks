using System;
using System.Net;
using System.Net.Sockets;
using Task5;
using Xunit;

namespace Task5Tests
{
    public class ClientTests
    {
        [Theory]
        [InlineData("I am disconnecting and politely ask you to connect to '192.168.1.142':'2222'", true)]
        [InlineData("Hello?'':'1111'", false)]
        [InlineData("Oh well '123':'123'", false)]
        public void TestIsDisconnectingMessage(string message, bool correct)
        {
            bool match = Client.IsDisconnectingMessage(message);

            Assert.Equal(correct, match);
        }

        [Fact]
        public void TestParseIpEndPointOnCorrectInput()
        {
            const string input = "I am disconnecting and politely ask you to connect to '192.168.1.142':'2222'";

            var parsed = Client.ParseIpEndPoint(input);
            var correct = new IPEndPoint(IPAddress.Parse("192.168.1.142"), 2222);

            Assert.Equal(correct, parsed);
        }
    }
}
