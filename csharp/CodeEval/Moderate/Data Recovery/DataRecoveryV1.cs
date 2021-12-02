using System;
using System.IO;
using System.Linq;

namespace CodeEval_Solutions.Moderate.Data_Recovery
{
    class DataRecoveryV1 : ICodeEvalChallenge
    {
        /// <summary>
        /// Time:           ? ms
        /// Memory:         ? bytes
        /// Unique:         Yes
        /// Ranking Points: 
        /// </summary>
        public void Run(string[] args)
        {
            var lines = new[] {
"2000 and was not However, implemented 1998 it until; 9 8 3 4 1 5 7 2",
"programming first The language; 3 2 1",
"programs Manchester The written ran Mark 1952 1 in Autocode from; 6 2 1 7 5 3 11 4 8 9", };

            //foreach (var line in File.ReadAllLines(args[0]))
            foreach (var line in lines)
            {
                var firstSplit = line.Split(new [] { ';' }, StringSplitOptions.RemoveEmptyEntries);
                var words = firstSplit[0].Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                var indexes = firstSplit[1].Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries)
                                           .Select(i => Int32.Parse(i)).ToArray();

                for (int i = 0; i < indexes.Length; i++)
                {
                    while(i + 1 != indexes[i])
                    {
                        Swap(ref words[i], ref words[indexes[i] - 1]);
                        Swap(ref indexes[i], ref indexes[indexes[i] - 1]);
                    }
                }

                Console.WriteLine(String.Join(" ", words));
            }
        }

        private static void Swap<T>(ref T first, ref T second)
        {
            var temp = first;
            first = second;
            second = temp;
        }
    }
}
