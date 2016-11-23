using System;
using System.IO;

namespace CodeEval_Solutions.Easy.Sum_Of_Integers_From_File
{
    /// <summary>
    /// Time:           366 ms
    /// Memory:         4767744 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.381
    /// </summary>
    public sealed class SumOfIntegersFromFileV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            using (StreamReader reader = File.OpenText(args[0]))
            {
                int sum = 0;
                while (!reader.EndOfStream)
                {
                    string line = reader.ReadLine();
                    if (null == line)
                        continue;
                    sum += Int32.Parse(line);
                }
                Console.WriteLine(sum);
            }
        }
    }
}
