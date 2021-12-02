using System;
using System.Collections.Generic;
using System.IO;

namespace CodeEval_Solutions.Moderate.Pass_Triangle
{
    /// <summary>
    /// Time:           452 ms
    /// Memory:         5214208 bytes
    /// Unique:         Yes
    /// Ranking Points: -
    /// </summary>
    public sealed class PassTriangleV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            using (StreamReader reader = File.OpenText(args[0]))
            {
                TriangleCollection collection = new TriangleCollection();
                while (!reader.EndOfStream)
                {
                    string line = reader.ReadLine();
                    if (null == line)
                        continue;
                    string[] values = line.Trim().Split(' ');
                    int[] row = new int[values.Length];
                    for (int i = 0; i < values.Length; i++)
                    {
                        row[i] = Int32.Parse(values[i]);
                    }
                    collection.AddRow(row);
                }

                Console.WriteLine(collection.MaximumTriangleWalk());
            }
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
            if (rows.Count != 0 && row.Length != rows[rows.Count - 1].Length + 1)
            {
                throw new InvalidDataException("Size not correct");
            }
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
                subproblemSolutions.Push(rowResults);
            }
            return subproblemSolutions.Peek()[0];
        }
    }
}
