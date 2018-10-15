using System;
using System.Collections.Generic;
using System.IO;
using JetBrains.Annotations;
using Task9.Parsing;

namespace Task9.Execution
{
    public class Wc
    {
        [Pure]
        public static object Execute(IEnumerable<object> args, Context context)
        {
            string result = null;

            foreach (var arg in args)
            {
                if (result != null)
                {
                    throw new InvalidSyntaxException("cat expected only one argument");
                }

                string fullPath = Path.Combine(context.Path, arg.ToString());
                
                if (!File.Exists(fullPath))
                {
                    return "No such file";
                }

                try
                {
                    // What a costly operation, huh?
                    // I don't care
                    result = $"{arg}:{Environment.NewLine}" +
                             $"\t{File.ReadAllLines(fullPath).Length} lines{Environment.NewLine}" +
                             $"\t{GetWordsCount(File.ReadAllText(fullPath))} words{Environment.NewLine}" +
                             $"\t{File.ReadAllBytes(fullPath).Length} bytes{Environment.NewLine}";
                }
                catch (IOException)
                {
                    Console.WriteLine("Could not access file");
                }
            }

            if (result != null)
            {
                return result;
            }

            throw new InvalidSyntaxException("wc expected at least one argument");
        }
        
        [Pure]
        static int GetWordsCount(string text)
        {
            int wordCount = 0;
            int index = 0;

            while (index < text.Length)
            {
                // check if current char is part of a word
                while (index < text.Length && !char.IsWhiteSpace(text[index]))
                    index++;

                wordCount++;

                // skip whitespace until next word
                while (index < text.Length && char.IsWhiteSpace(text[index]))
                    index++;
            }

            return wordCount;
        }
    }
}
