using System;
using System.IO;

namespace CodeEval_Solutions.Easy.Calculate_Distance
{
    /// <summary>
    /// Time:           179 ms
    /// Memory:         5046272 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.474
    /// </summary>
    public sealed class CalculateDistanceV2 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            foreach (var line in File.ReadAllLines(args[0]))
            {
                int[] vals = FastIntsParse(line);
                Console.WriteLine(Distance(vals[0], vals[1], vals[2], vals[3]));
            }
        }
        public static double Distance(int f1, int f2, int f3, int f4)
        {
            return Math.Sqrt(Math.Pow((f1 - f3), 2D) + Math.Pow((f2 - f4), 2D));
        }

        private static int[] FastIntsParse(string val)
        {
            int[] results = new int[4];
            int resultIndex = 0;
            int result = 0;
            int strLength = val.Length;
            int start = 0;
            bool negative = false;

            for (int l = start; l < strLength; l++)
            {

                if (val[l] == '-')
                {
                    negative = true;
                }
                else if (47 < val[l] && val[l] < 58)
                {
                    result = 10 * result + (val[l] - 48);
                }
                else if (val[l] != ' ' && val[l] != '(')
                {
                    if (negative)
                        result = -1 * result;
                    results[resultIndex++] = result;
                    result = 0;
                    negative = false;
                }
            }

            return results;
        }
    }
}
