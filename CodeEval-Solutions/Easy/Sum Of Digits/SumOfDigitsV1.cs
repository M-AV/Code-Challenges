using System;
using System.IO;

namespace CodeEval_Solutions.Easy.Sum_Of_Digits
{
    /// <summary>
    /// Time:           270 ms
    /// Memory:         4976640 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.375
    /// </summary>
    public sealed class SumOfDigitsV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            foreach (var line in File.ReadAllLines(args[0]))
            {
                int sum = 0;
                for (int i = 0; i < line.Length; ++i)
                {
                    sum += line[i] - 48;
                }

                Console.WriteLine(sum);
            }
        }
    }
}
