using System;
using System.IO;

namespace CodeEval_Solutions.Easy.N_Mod_M
{
    /// <summary>
    /// Given two integers N and M, calculate N Mod M (without using any inbuilt modulus operator).
    /// 
    /// Time:           165 ms
    /// Memory:         4874240 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.644
    /// </summary>
    public sealed class NModMV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            using (StreamReader reader = File.OpenText(args[0]))
            {
                while (!reader.EndOfStream)
                {
                    string line = reader.ReadLine();
                    if (null == line)
                        continue;

                    string[] values = line.Split(',');
                    int n = Int32.Parse(values[0]);
                    int m = Int32.Parse(values[1]);

                    int divided = n / m;
                    Console.WriteLine(n - (divided * m));
                }
            }
        }
    }
}
