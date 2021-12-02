using System;
using System.Text;

namespace CodeEval_Solutions.Easy.Multiplication_Tables
{
    /// <summary>
    /// Time:           138 ms
    /// Memory:         4653056 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.875
    /// </summary>
    public sealed class MultiplicationTablesV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            for (int i = 1; i <= 12; i++)
            {
                StringBuilder sb = new StringBuilder();
                for (int j = 1; j <= 12; j++)
                {
                    sb.AppendFormat("{0,4}", j * i);
                }
                Console.WriteLine(sb.ToString().Trim());
            }
        }
    }
}
