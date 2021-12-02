using System;

namespace CodeEval_Solutions.Easy.Odd_Numbers
{
    /// <summary>
    /// Time:           123 ms
    /// Memory:         4714496 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.849
    /// </summary>
    public sealed class OddNumbersV2 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            for (int i = 1; i < 100; i += 2)
            {
                Console.WriteLine(i);
            }
        }
    }
}
