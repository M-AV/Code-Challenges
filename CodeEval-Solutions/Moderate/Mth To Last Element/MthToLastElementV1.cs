using System;
using System.IO;

namespace CodeEval_Solutions.Moderate.Mth_To_Last_Element
{
    /// <summary>
    /// Time:           143 ms
    /// Memory:         4722688 bytes
    /// Unique:         Yes
    /// Ranking Points: 57.214
    /// </summary>
    public sealed class MthToLastElementV1 : ICodeEvalChallenge
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
                    string[] elements = line.Split(new[] { ' ' });
                    int index = int.Parse(elements[elements.Length - 1]);
                    if (index <= elements.Length - 1)
                    {
                        Console.WriteLine(elements[elements.Length - (index + 1)]);
                    }
                }
            }
        }
    }
}
