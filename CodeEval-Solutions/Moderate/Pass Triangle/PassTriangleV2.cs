using System;
using System.Collections.Generic;
using System.IO;

namespace CodeEval_Solutions.Moderate.Pass_Triangle
{
    /// <summary>
    /// Time:           200 ms
    /// Memory:         5246976 bytes
    /// Unique:         Yes
    /// Ranking Points: 56.218
    /// </summary>
    public sealed class PassTriangleV2 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            using (StreamReader reader = File.OpenText(args[0]))
            {
                TriangleCollection collection = new TriangleCollection();
                while (!reader.EndOfStream)
                {
                    string line = reader.ReadLine();

                    string[] values = line.Trim().Split(' ');
                    int[] row = new int[values.Length];
                    for (int i = 0; i < values.Length; i++)
                    {
                        int result = 0;
                        int length = values[i].Length;
                        for (int l = 0; l < length; l++)
                        {
                            result = 10 * result + (values[i][l] - 48);
                        }
                        row[i] = result;
                    }
                    collection.AddRow(row);
                }

                Console.WriteLine(collection.MaximumTriangleWalk());
            }
        }

        public class TriangleCollection 
        {
            private readonly List<int[]> rows;

            public TriangleCollection()
            {
                rows = new List<int[]>();
            }

            public void AddRow(int[] row)
            {
                this.rows.Add(row);
            }

            public int MaximumTriangleWalk()
            {
                Stack<int[]> subproblemSolutions = new Stack<int[]>();
                subproblemSolutions.Push(rows[rows.Count - 1]);
                for (int i = rows.Count - 2; i >= 0; i--)
                {
                    int[] rowResults = new int[rows[i].Length];
                    for (int j = 0; j < rows[i].Length; j++)
                    {
                        rowResults[j] = rows[i][j] + Math.Max(subproblemSolutions.Peek()[j], subproblemSolutions.Peek()[j + 1]);
                    }
                    subproblemSolutions.Pop();
                    subproblemSolutions.Push(rowResults);
                }
                return subproblemSolutions.Peek()[0];
            }
        }
    }
}
