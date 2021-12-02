using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace CodeEval_Solutions.Easy.Lowest_Unique_Number
{
    /// <summary>
    /// Time:           201 ms
    /// Memory:         5275648 bytes
    /// Unique:         Yes
    /// Ranking Points: -
    /// </summary>
    public sealed class LowestUniqueNumberV2 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            foreach (var line in File.ReadAllLines(args[0]))
            {
                int[] numbes = line.Split(new[] { ' ' }).Select(c => Convert.ToInt32(c)).ToArray();

                var foundNumbers = new Dictionary<int, int>();

                for (int i = 0; i < numbes.Length; ++i)
                {
                    if (foundNumbers.ContainsKey(numbes[i]))
                    {
                        foundNumbers[numbes[i]] = -1;
                    }
                    else
                    {
                        foundNumbers.Add(numbes[i], i);
                    }
                }

                int lowestVal = 10;
                int lowestIndex = -1;

                foreach (var numbe in foundNumbers)
                {
                    if (numbe.Value > -1 && lowestVal > numbe.Key)
                    {
                        lowestVal = numbe.Key;
                        lowestIndex = numbe.Value;
                    }
                }
                Console.WriteLine(lowestVal == 10 ? 0 : lowestIndex + 1);
            }
        }
    }
}
