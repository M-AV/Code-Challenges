using System;
using System.IO;

namespace CodeEval_Solutions.Easy.Odd_Numbers
{
    /// <summary>
    /// Time:           105 ms
    /// Memory:         4784128 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.824
    /// </summary>
    public sealed class OddNumbersV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            using (StreamReader reader = File.OpenText(args[0]))
            {
                for (int i = 1; i < 100; i++)
                {
                    if (i % 2 == 1)
                        Console.WriteLine(i);
                }
            }
        }
    }
}
