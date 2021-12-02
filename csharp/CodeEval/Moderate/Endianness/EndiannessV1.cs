using System;

namespace CodeEval_Solutions.Moderate.Endianness
{
    /// <summary>
    /// Time:           113 ms
    /// Memory:         4567040 bytes
    /// Unique:         Yes
    /// Ranking Points: 57.553
    /// </summary>
    /// <remarks>Had to do this.. Too obvious. :P</remarks>
    public sealed class EndiannessV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            Console.WriteLine(BitConverter.IsLittleEndian ? "LittleEndian" : "BigEndian");
        }
    }
}
