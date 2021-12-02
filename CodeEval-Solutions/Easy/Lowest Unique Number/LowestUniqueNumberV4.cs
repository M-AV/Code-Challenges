using System;
using System.IO;

namespace CodeEval_Solutions.Easy.Lowest_Unique_Number
{
    /// <summary>
    /// Time:           161 ms
    /// Memory:         4792320 bytes
    /// Unique:         Yes
    /// Ranking Points: -
    /// </summary>
    public sealed class LowestUniqueNumberV4 : ICodeEvalChallenge
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

                var foundNumbers = new int[10];

                for (int i = 0; i < index; ++i)
                {
                    if (foundNumbers[numbers[i]] == 0)
                    {
                        foundNumbers[numbers[i]] = i + 1;
                    }
                    else
                    {
                        foundNumbers[numbers[i]] = -1;
                    }
                }

                int lowestVal = 10;
                int lowestIndex = -1;

                for (int i = 0; i < foundNumbers.Length; ++i)
                {
                    if (foundNumbers[i] > 0)
                    {
                        lowestIndex = foundNumbers[i];
                        lowestVal = i;
                        break;
                    }
                }

                Console.WriteLine(lowestVal == 10 ? 0 : lowestIndex);
            }
        }
    }
}
