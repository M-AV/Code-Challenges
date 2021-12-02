using System;
using System.IO;

namespace CodeEval_Solutions.Moderate.Trailing_String
{
    /// <summary>
    /// Time:           269 ms
    /// Memory:         5120000 bytes
    /// Unique:         Yes
    /// Ranking Points: 56.188
    /// </summary>
    public sealed class TrailingStringV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            using (StreamReader reader = File.OpenText(args[0]))
            while (!reader.EndOfStream)
            {
                string line = reader.ReadLine();
                if (null == line)
                    continue;
                string[] splits = line.Split(new[] { ',' });
                int output = splits[0].EndsWith(splits[1]) ? 1 : 0;

                Console.WriteLine(output);
            }
        }
    }
}
