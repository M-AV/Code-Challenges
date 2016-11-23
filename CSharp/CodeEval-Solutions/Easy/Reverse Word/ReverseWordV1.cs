using System;
using System.IO;

namespace CodeEval_Solutions.Easy.Reverse_Word
{
    /// <summary>
    /// Time:           122 ms
    /// Memory:         4747264 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.823
    /// </summary>
    public sealed class ReverseWordV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            using (StreamReader reader = File.OpenText(args[0]))
            {
                while (!reader.EndOfStream)
                {
                    string line = reader.ReadLine();
                    if (null == line || String.IsNullOrWhiteSpace(line))
                        continue;

                    string[] values = line.Split(' ');
                    Array.Reverse(values);

                    string result = String.Join(" ", values);
                    Console.WriteLine(result);
                }
            }
        }
    }
}
