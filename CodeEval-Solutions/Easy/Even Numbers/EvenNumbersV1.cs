using System;
using System.IO;

namespace CodeEval_Solutions.Easy.Even_Numbers
{
    /// <summary>
    /// Time:           191 ms
    /// Memory:         4882432 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.591
    /// </summary>
    public sealed class EvenNumbersV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            using (StreamReader reader = File.OpenText(args[0]))
            while (!reader.EndOfStream)
            {
                string line = reader.ReadLine();
                if (null == line)
                    continue;
                int value = Int32.Parse(line);
                Console.WriteLine((value % 2) == 0 ? 1 : 0);
            }
        }
    }
}
