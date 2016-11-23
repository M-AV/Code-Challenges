using System;
using System.IO;

namespace CodeEval_Solutions.Easy.Longest_Word
{
    /// <summary>
    /// Time:           129 ms
    /// Memory:         4878336 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.703
    /// </summary>
    public sealed class LongestWordV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            foreach (var line in File.ReadAllLines(args[0]))
            {
                string[] splits = line.Split(new[] { ' ' });

                string maxWord = splits[0];
                for (int i = 1; i < splits.Length; i++)
                {
                    if (maxWord.Length < splits[i].Length)
                        maxWord = splits[i];
                }

                Console.WriteLine(maxWord);
            }
        }
    }
}
