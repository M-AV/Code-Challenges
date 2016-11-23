using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace CodeEval_Solutions.Moderate.Reverse_And_Add
{
    /// <summary>
    /// Time:           403 ms
    /// Memory:         4927488 bytes
    /// Unique:         Yes
    /// Ranking Points: -
    /// </summary>
    public sealed class ReverseAndAddV1 : ICodeEvalChallenge
    {
        public static int ReverseNumber(int value)
        {
            IEnumerable<char> v = value.ToString().Reverse();
            return Int32.Parse(new string(v.ToArray()));
        }
        public static bool IsPalindrome(int value)
        {
            return value == ReverseNumber(value);
        }
        public void Run(string[] args)
        {
            using (StreamReader reader = File.OpenText(args[0]))
            {
                while (!reader.EndOfStream)
                {
                    string line = reader.ReadLine();
                    if (null == line)
                        continue;
                    int initialValue = Int32.Parse(line);
                    int sum = initialValue;
                    int iterations = 0;
                    do
                    {
                        sum += ReverseNumber(sum);
                        iterations++;
                    } while (!IsPalindrome(sum));

                    Console.WriteLine(iterations + " " + sum);
                }
            }
        }
    }
}
