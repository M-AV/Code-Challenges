using System;
using System.IO;
using System.Text;

namespace CodeEval_Solutions.Easy.Capitalize_Words
{
    /// <summary>
    /// Time:           155 ms
    /// Memory:         4841472 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.687
    /// </summary>
    public sealed class CapitalizeWordsV1 : ICodeEvalChallenge
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
                    line = ToggleCase(line);
                    Console.WriteLine(line);
                }
            }
        }
        private static string ToggleCase(string line)
        {
            string[] lines = line.Split(new[] { ' ' });

            for (int i = 0; i < lines.Length; i++)
            {
                StringBuilder sb = new StringBuilder(lines[i]);
                sb[0] = Char.ToUpper(sb[0]);
                lines[i] = sb.ToString();
            }
            return String.Join(" ", lines);
        }
    }
}
