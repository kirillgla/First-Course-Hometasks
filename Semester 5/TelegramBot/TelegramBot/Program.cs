using System;
using System.Diagnostics;
using System.Net;
using System.Threading;
using System.Threading.Tasks;
using Telegram.Bot;
using Telegram.Bot.Types.ReplyMarkups;

namespace TelegramBot
{
	internal static class Program
	{
		// Like anyone cares about safety...
		private const string Token = "993018946:AAE8HMpxm_U64aZs9VgLykPr5gb-Z3vuHz8";

		const string artifact =
			@"C:\w\Spbu-Homework\Semester 5\TelegramBot\YoutubeRequester\out\artifacts\YoutubeRequester.jar";

		private static async Task Main()
		{
			// Telegram IP is blocked, but how about this?
			var proxy = new WebProxy("5.9.201.68", 3128);
			var botClient = new TelegramBotClient(Token, proxy);
			var me = await botClient.GetMeAsync();
			Console.WriteLine($"Started bot. User: {me.Id}. Name: {me.FirstName}.");
			botClient.OnMessage += async (sender, args) =>
			{
				if (args.Message == null) return;
				var markup = new ReplyKeyboardMarkup(new KeyboardButton("MAGIC"));
				if (args.Message.Text != "MAGIC")
				{
					await botClient.SendTextMessageAsync(
						args.Message.Chat,
						"Do you want some magic?",
						replyMarkup: markup
					);
					return;
				}

				var process = new Process
				{
					StartInfo = new ProcessStartInfo("java", $"-jar \"{artifact}\"")
					{
						RedirectStandardOutput = true
					}
				};

				process.Start();
				process.WaitForExit();
				string output = process.StandardOutput.ReadToEnd();
				await botClient.SendTextMessageAsync(args.Message.Chat, output, replyMarkup: markup);
			};
			botClient.StartReceiving();
			Thread.Sleep(int.MaxValue);
		}
	}
}
