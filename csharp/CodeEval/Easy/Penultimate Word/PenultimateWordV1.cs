using System;
using System.IO;

namespace CodeEval_Solutions.Easy.Penultimate_Word
{
    /// <summary>
    /// Time:           155 ms
    /// Memory:         4739072 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.774
    /// </summary>
    public sealed class PenultimateWordV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            using (StreamReader reader = File.OpenText(args[0]))
            {
                while (!reader.EndOfStream)
                {
                    string line = reader.ReadLine();
                    if (String.IsNullOrEmpty(line))
                        continue;
                    string[] lines = line.Split(new[] { ' ' });
                    Console.WriteLine(lines[lines.Length - 2]);
                }
            }
        }
    }
}
