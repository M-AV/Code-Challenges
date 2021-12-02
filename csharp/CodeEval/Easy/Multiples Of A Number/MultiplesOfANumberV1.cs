using System;
using System.IO;

namespace CodeEval_Solutions.Easy.Multiples_Of_A_Number
{
    /// <summary>
    /// Time:           138 ms
    /// Memory:         4988928 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.595
    /// </summary>
    public sealed class MultiplesOfANumberV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            foreach (var line in File.ReadAllLines(args[0]))
            {
                string[] splits = line.Split(new[] { ',' });
                int x = FastIntParseNoNegatives(splits[0]);
                int n = FastIntParseNoNegatives(splits[1]);

                int result = n;

                while (result < x)
                {
                    result += n;
                }

                Console.WriteLine(result);
            }
        }
        private static int FastIntParseNoNegatives(string val)
        {
            int result = 0;
            int strLength = val.Length;
            for (int l = 0; l < strLength; l++)
            {
                result = 10 * result + (val[l] - 48);
            }
            return result;
        }
    }
}
