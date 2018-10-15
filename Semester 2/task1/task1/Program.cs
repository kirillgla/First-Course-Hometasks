// Had to change quite a lot in my C code!

using System;
using System.IO;

namespace Task1
{
    static class Program
    {
        #region constants

        const string About = "This is an utility that applies selected filter to selected .bmp image.\r\n"
                             + "Arguments:\r\n"
                             + "-i: path to file to be modified.\r\n"
                             + "-o: path to desired destination of resulting image.\r\n"
                             + "-f: filter to be applied.\npossible filters:\r\n"
                             + "\tgauss\r\n"
                             + "\tsobelx\r\n"
                             + "\tsobely\r\n"
                             + "\tgreyen\n";

        const string Gauss = "gauss";
        const string Sobelx = "sobelx";
        const string Sobely = "sobely";
        const string Greyen = "greyen";

        static readonly double[][] GaussMatrix =
        {
            new[] {1.0 / 16, 1.0 / 8, 1.0 / 16},
            new[] {1.0 / 8, 1.0 / 4, 1.0 / 8},
            new[] {1.0 / 16, 1.0 / 8, 1.0 / 16}
        };

        static readonly double[][] SobelxMatrix =
        {
            new[] {-3.0 / 32, 0, 3.0 / 32},
            new[] {-10.0 / 32, 0, 10.0 / 32},
            new[] {-3.0 / 32, 0, 3.0 / 32}
        };

        static readonly double[][] SobelyMatrix =
        {
            new[] {-3.0 / 32, -10.0 / 32, -3.0 / 32},
            new[] {0.0, 0.0, 0.0},
            new[] {3.0 / 32, 10.0 / 32, 3.0 / 32}
        };

        #endregion

        static void Main(string[] args)
        {
            if (args.Length == 0)
            {
                Console.WriteLine(About);
                return;
            }

            try
            {
                Util.HandleArguments(args, out string source, out string filter, out string destination);
                var bytes = File.ReadAllBytes(source);
                Util.HandleBitMapFileHeader(bytes, out var fileHeader);
                Util.HandleBitMapInfoHeader(bytes, out var infoHeader);

                var newBytes = new byte[bytes.Length];
                Util.CopyHeader(bytes, newBytes, fileHeader.BfOffBits);
                switch (filter)
                {
                    case Gauss:
                        Util.ApplyKernel(bytes, newBytes, GaussMatrix, fileHeader, infoHeader);
                        break;
                    case Sobelx:
                        Util.ApplyKernel(bytes, newBytes, SobelxMatrix, fileHeader, infoHeader);
                        break;
                    case Sobely:
                        Util.ApplyKernel(bytes, newBytes, SobelyMatrix, fileHeader, infoHeader);
                        break;
                    case Greyen:
                        Util.ApplyGreyen(bytes, newBytes, fileHeader, infoHeader);
                        break;
                    default:
                        throw new ArgumentException($"Error: unknown filter type: \"{filter}\"");
                }

                File.WriteAllBytes(destination, newBytes);
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
                return;
            }

            Console.WriteLine("Done.");
        }
    }
}
