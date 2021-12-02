using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace CodeEval_Solutions.Easy.Compressed_Sequence
{
    /// <summary>
    /// Time:           199 ms
    /// Memory:         5103616 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.391
    /// </summary>
    public sealed class CompressedSequenceV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            using (StreamReader reader = File.OpenText(args[0]))
            {
                string[] lines = File.ReadAllLines(args[0]);
                foreach (string line in lines.Where(line => !String.IsNullOrEmpty(line)))
                {
                    List<int> result = new List<int>();
                    int[] numbers = line.Split(new[] { ' ' }).Select(s => Convert.ToInt32(s)).ToArray();
                    int currentNo = numbers[0], noCount = 1;

                    for (int i = 1; i < numbers.Length; i++)
                    {
                        if (numbers[i] == currentNo)
                        {
                            noCount++;
                        }
                        else
                        {
                            result.Add(noCount);
                            result.Add(currentNo);
                            currentNo = numbers[i];
                            noCount = 1;
                        }
                    }
                    result.Add(noCount);
                    result.Add(currentNo);
                    Console.WriteLine(String.Join(" ", result));
                }
            }
        }
    }
}
