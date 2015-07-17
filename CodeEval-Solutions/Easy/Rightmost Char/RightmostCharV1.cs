using System;
using System.IO;

namespace CodeEval_Solutions.Easy.Rightmost_Char
{
    /// <summary>
    /// Time:           169 ms
    /// Memory:         4702208 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.779
    /// </summary>
    public sealed class RightmostCharV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            using (StreamReader reader = File.OpenText(args[0]))
            {
                while (!reader.EndOfStream)
                {
                    string line = reader.ReadLine();
                    if (string.IsNullOrWhiteSpace(line))
                    {
                        continue;
                    }

                    string[] input = line.Split(new[] { ',' });
                    int position = input[0].LastIndexOf(input[1].ToCharArray()[0]);
                    Console.WriteLine(position);
                }
            }
        }
    }
}
