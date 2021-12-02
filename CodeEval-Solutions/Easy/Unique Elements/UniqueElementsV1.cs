using System;
using System.Collections.Generic;
using System.IO;

namespace CodeEval_Solutions.Easy.Unique_Elements
{
    /// <summary>
    /// Time:           211 ms
    /// Memory:         5341184 bytes
    /// Unique:         Yes
    /// Ranking Points: -
    /// </summary>
    public sealed class UniqueElementsV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            using (StreamReader reader = File.OpenText(args[0]))
            {
                while (!reader.EndOfStream)
                {
                    HashSet<int> set = new HashSet<int>();
                    string line = reader.ReadLine();
                    if (null == line)
                        continue;

                    string[] values = line.Split(',');
                    foreach (var value in values)
                    {
                        set.Add(Int32.Parse(value));
                    }

                    Console.WriteLine(String.Join(",", set));

                }
            }
        }
    }
}
