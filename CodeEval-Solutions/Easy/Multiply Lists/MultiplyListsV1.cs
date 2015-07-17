using System;
using System.IO;

namespace CodeEval_Solutions.Easy.Multiply_Lists
{
    /// <summary>
    /// Time:           187 ms
    /// Memory:         4890624 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.591
    /// </summary>
    public sealed class MultiplyListsV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            var lines = File.ReadAllLines(args[0]);

            foreach (var line in lines)
            {
                var splitMiddle = line.Split('|');
                var leftNumbers = splitMiddle[0].Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                var rightNumbers = splitMiddle[1].Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                int[] results = new int[leftNumbers.Length];
                for (int i = 0; i < leftNumbers.Length; i++)
                {
                    results[i] = Convert.ToInt32(leftNumbers[i]) * Convert.ToInt32(rightNumbers[i]);
                }
                Console.WriteLine(string.Join(" ", results));
            }
        }
    }
}
