using System;
using System.IO;
using System.Text;

namespace CodeEval_Solutions.Easy.Unique_Elements
{
    /// <summary>
    /// Time:           83 ms
    /// Memory:         5029888 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.657
    /// </summary>
    public sealed class UniqueElementsV2 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            foreach (var line in File.ReadAllLines(args[0]))
            {
                if (null == line)
                    continue;

                string[] values = line.Split(',');

                int length = values.Length;
                int prev = 0;
                StringBuilder sb = new StringBuilder();
                for (int i = 0; i < length; i++)
                {
                    int result = 0;
                    int strLength = values[i].Length;
                    for (int l = 0; l < strLength; l++)
                    {
                        result = 10 * result + (values[i][l] - 48);
                    }
                    if (prev != result)
                    {
                        sb.Append(result + ",");
                        prev = result;
                    }
                }
                Console.WriteLine(sb.Remove(sb.Length - 1, 1).ToString());
            }
        }
    }
}
