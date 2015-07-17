using System;
using System.IO;
using System.Text;

namespace CodeEval_Solutions.Easy.Bit_Positions
{
    /// <summary>
    /// Time:           193 ms
    /// Memory:         4734976 bytes
    /// Unique:         Yes
    /// Ranking Points: 30.711
    /// </summary>
    public sealed class BitPositionsV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            using (StreamReader reader = File.OpenText(args[0]))
            while (!reader.EndOfStream)
            {
                string line = reader.ReadLine();
                if (null == line)
                    continue;
                string[] values = line.Split(new[] { ',' });
                int value = Int32.Parse(values[0]);
                int index1 = Int32.Parse(values[1]);
                int index2 = Int32.Parse(values[2]);

                string bits = GetBits(value);

                Console.WriteLine(bits[bits.Length - (index1)] == bits[bits.Length - (index2)] ? "true" : "false");
            }
        }
        private static string GetBits(int value)
        {
            StringBuilder sb = new StringBuilder();
            uint temp = 0x80000000;
            for (int i = 1; i <= 32; i++)
            {
                if ((temp & value) > 0)
                    sb.Append("1");
                else
                    sb.Append("0");
                temp >>= 1;
            }
            return sb.ToString();
        }
    }
}
