using System;
using System.Collections.Generic;
using System.IO;

namespace CodeEval_Solutions.Easy.Lowest_Unique_Number
{
    /// <summary>
    /// Time:           197 ms
    /// Memory:         5115904 bytes
    /// Unique:         Yes
    /// Ranking Points: -
    /// </summary>
    public sealed class LowestUniqueNumberV3 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            foreach (var line in File.ReadLines(args[0]))
            {
                const int ascii_offset = 48;

                int[] numbers = new int[line.Length];
                int index = 0;
                for (int i = 0; i < line.Length; ++i)
                {
                    if (line[i] != ' ')
                    {
                        numbers[index] = line[i] - ascii_offset;
                        index++;
                    }
                }

                var foundNumbers = new Dictionary<int, int>();

                for (int i = 0; i < index; ++i)
                {
                    if (foundNumbers.ContainsKey(numbers[i]))
                    {
                        foundNumbers[numbers[i]] = -1;
                    }
                    else
                    {
                        foundNumbers.Add(numbers[i], i);
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
