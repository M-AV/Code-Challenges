using System;
using System.IO;
using System.Linq;

namespace CodeEval_Solutions.Moderate.Longest_Lines
{
    /// <summary>
    /// Time:           229 ms
    /// Memory:         5255168 bytes
    /// Unique:         Yes
    /// Ranking Points: 56.111
    /// </summary>
    public sealed class LongestLinesV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            var lines = File.ReadAllLines(args[0]);
            int printCount = Convert.ToInt32(lines[0]);

            foreach (var line in lines.Skip(1)
                                      .OrderByDescending(l => l.Length)
                                      .Take(printCount))
            {
                Console.WriteLine(line);
            }
        }
    }
}
