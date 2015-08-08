using System;
using System.IO;
using System.Linq;

namespace CodeEval_Solutions.Easy.Swap_Elements
{
    /// <summary>
    /// Time:           190 ms
    /// Memory:         5120000 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.394
    /// </summary>
    public sealed class SwapElementsV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            foreach (var line in File.ReadLines(args[0]))
            {
                var splittet = line.Split(new[] { ':' }, StringSplitOptions.RemoveEmptyEntries);
                var numbers = splittet[0].Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                var swapPositions = splittet[1].Split(new[] { ',', ' ' }, StringSplitOptions.RemoveEmptyEntries);

                foreach (var swapPosition in swapPositions)
                {
                    var positions = swapPosition.Split(new[] { '-' }, StringSplitOptions.RemoveEmptyEntries)
                                                .Select(value => Convert.ToInt32(value)).ToArray();

                    var temp = numbers[positions[0]];
                    numbers[positions[0]] = numbers[positions[1]];
                    numbers[positions[1]] = temp;
                }

                Console.WriteLine(string.Join(" ", numbers));
            }
        }
    }
}
