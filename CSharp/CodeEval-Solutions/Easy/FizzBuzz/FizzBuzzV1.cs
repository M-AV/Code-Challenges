using System;
using System.IO;

namespace CodeEval_Solutions.Easy.FizzBuzz
{
    /// <summary>
    /// Time:           163 ms
    /// Memory:         4829184 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.684
    /// </summary>
    public sealed class FizzBuzzV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            using (StreamReader reader = File.OpenText(args[0]))
            while (!reader.EndOfStream)
            {
                string line = reader.ReadLine();
                if (null == line)
                    continue;
                string[] values = line.Split(new char[] { ' ' });
                int a = Int32.Parse(values[0]);
                int b = Int32.Parse(values[1]);
                int n = Int32.Parse(values[2]);
                PrintFizzBuzz(a, b, n);
            }
        }

        private static void PrintFizzBuzz(int a, int b, int n)
        {
            for (int i = 1; i <= n; i++)
            {
                if (i % a == 0 && i % b == 0)
                    Console.Write("FB ");
                else if (i % a == 0)
                    Console.Write("F ");
                else if (i % b == 0)
                    Console.Write("B ");
                else
                {
                    Console.Write(i + " ");
                }
            }
            Console.WriteLine();
        }
    }
}
