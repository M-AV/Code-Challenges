using System;
using System.IO;

namespace CodeEval_Solutions.Easy.File_Size
{
    /// <summary>
    /// Time:           131 ms
    /// Memory:         4911104 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.671
    /// </summary>
    public sealed class FileSizeV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            Console.WriteLine(new FileInfo(args[0]).Length);
        }
    }
}
