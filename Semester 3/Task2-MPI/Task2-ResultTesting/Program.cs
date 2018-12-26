using System;
using System.IO;
using System.Linq;

namespace Task2_ResultTesting
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args?.Length != 2)
            {
                Console.WriteLine("Invalid number of input parameters. 2 filenames expected.");
                return;
            }

            string file1 = args[0];
            string file2 = args[1];

            if (file1 == null || !File.Exists(file1))
            {
                Console.WriteLine($"File {file1} does not exist.");
                return;
            }

            if (file2 == null || !File.Exists(file2))
            {
                Console.WriteLine($"File {file2} does not exist.");
                return;
            }

            string contents1 = File.ReadAllText(file1);
            string contents2 = File.ReadAllText(file2);

            if (contents1.Length != contents2.Length)
            {
                Console.WriteLine("File sizes are different");
                return;
            }

            for (int i = 0; i < contents1.Length; i++)
            {
                if (contents1[i] == contents2[i])
                {
                    continue;
                }

                Console.WriteLine($"Files are different at position {i}");
                return;
            }

            Console.WriteLine("Files are the same");
        }
    }
}
