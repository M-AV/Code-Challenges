using System;
using System.IO;

namespace CodeEval_Solutions.Moderate.Reverse_And_Add
{
    /// <summary>
    /// Time:           79 ms
    /// Memory:         4997120 bytes
    /// Unique:         Yes
    /// Ranking Points: 56.997
    /// </summary>
    public sealed class ReverseAndAddV2 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            foreach (var line in File.ReadAllLines(args[0]))
            {
                if (null == line)
                    continue;

                int initialValue = FastIntParse(line);
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
        private static int FastIntParse(string val)
        {
            int result = 0;
            int strLength = val.Length;
            int start = 0;
            bool negative = false;
            if (val[0] == '-')
            {
                start = 1;
                negative = true;
            }
            for (int l = start; l < strLength; l++)
            {
                result = 10 * result + (val[l] - 48);
            }
            if (negative)
                result = -1 * result;
            return result;
        }
        public static int ReverseNumber(int value)
        {
            int reverse = 0;
            while (value > 0)
            {
                int rem = value % 10;
                reverse = reverse * 10 + rem;
                value = value / 10;
            }
            return reverse;
        }
        public static bool IsPalindrome(int value)
        {
            return value == ReverseNumber(value);
        }
    }
}
