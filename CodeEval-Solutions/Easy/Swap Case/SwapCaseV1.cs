using System;
using System.IO;
using System.Text;

namespace CodeEval_Solutions.Easy.Swap_Case
{
    /// <summary>
    /// Time:           152 ms
    /// Memory:         4755456 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.765
    /// </summary>
    public sealed class SwapCaseV1 : ICodeEvalChallenge
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
            StringBuilder sb = new StringBuilder(line);
            for (int i = 0; i < sb.Length; i++)
            {
                if (Char.IsUpper(sb[i]))
                {
                    sb[i] = Char.ToLower(sb[i]);
                }
                else
                {
                    sb[i] = Char.ToUpper(sb[i]);
                }
            }
            return sb.ToString();
        }
    }
}
