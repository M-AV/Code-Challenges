using System;
using System.IO;

namespace CodeEval_Solutions.Easy.Lowest_Unique_Number
{
    /// <summary>
    /// Time:           151 ms
    /// Memory:         3325952 bytes
    /// Unique:         Yes
    /// Ranking Points: 31.959
    /// </summary>
    public sealed class LowestUniqueNumberV5 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            using (var reader = new BinaryReader(File.OpenRead(args[0])))
            {
                var foundNumbers = new int[10];
                int currentIndex = 0;
                int b;
                while ((b = reader.Read()) > -1)
                {
                    if (b == 10) //linefeed - Print result
                    {
                        for (int i = 0; i < foundNumbers.Length; ++i)
                        {
                            if (foundNumbers[i] > 0)
                            {
                                Console.WriteLine(i == 10 ? 0 : foundNumbers[i]);
                                break;
                            }
                        }

                        foundNumbers = new int[10];
                        currentIndex = 0;
                    }
                    else if (b != 32 && b != 13)  //32 - space, 13 - Carriage return
                    {
                        currentIndex++;
                        var number = b - 48;
                        if (foundNumbers[number] == 0)
                        {
                            foundNumbers[number] = currentIndex;
                        }
                        else
                        {
                            foundNumbers[number] = -1;
                        }
                    }
                }

                for (int i = 0; i < foundNumbers.Length; ++i)
                {
                    if (foundNumbers[i] > 0)
                    {
                        Console.WriteLine(i == 10 ? 0 : foundNumbers[i]);
                        break;
                    }
                }
            }
        }
    }
}
