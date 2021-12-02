using System;
using System.IO;
using System.Text;

namespace CodeEval_Solutions.Easy.Lowercase
{
    /// <summary>
    /// Time:           185 ms
    /// Memory:         4710400 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.745
    /// </summary>
    public sealed class LowercaseV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            using (var reader = new BinaryReader(File.OpenRead(args[0])))
            {
                var sb = new StringBuilder();
                int b;
                while ((b = reader.Read()) > -1)
                {
                    if (b == 10) //10 = linefeed - Print result
                    {
                        Console.WriteLine(sb.ToString());
                        sb.Clear();
                    }
                    else if (65 <= b && b <= 90)  //65 = A, 13 = Z
                    {
                        sb.Append(((char)(b + 32)));
                    }
                    else if (b != 13)
                    {
                        sb.Append((char)b);
                    }
                }
                Console.WriteLine(sb.ToString());
            }
        }
    }
}
