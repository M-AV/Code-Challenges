using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;

namespace CodeEval_Solutions.Easy.Calculate_Distance
{
    /// <summary>
    /// Time:           824 ms
    /// Memory:         5468160 bytes
    /// Unique:         Yes
    /// Ranking Points: -
    /// </summary>
    public sealed class CalculateDistanceV1 : ICodeEvalChallenge
    {
        public void Run(string[] args)
        {
            List<Tuple<Point, Point>> coordinateSet = new List<Tuple<Point, Point>>();
            using (StreamReader reader = File.OpenText(args[0]))
            {
                while (!reader.EndOfStream)
                {
                    string line = reader.ReadLine();
                    if (null == line)
                        continue;

                    Regex regex = new Regex(@"\-?[0-9]+, \-?[0-9]+");
                    MatchCollection collection = regex.Matches(line);

                    string[] pointOneValues = collection[0].Value.Split(',');
                    string[] pointTwoValues = collection[1].Value.Split(',');
                    Tuple<Point, Point> t = new Tuple<Point, Point>(new Point(Int32.Parse(pointOneValues[0].Trim()), Int32.Parse(pointOneValues[1].Trim())),
                                                                    new Point(Int32.Parse(pointTwoValues[0].Trim()), Int32.Parse(pointTwoValues[1].Trim())));
                    coordinateSet.Add(t);
                }
            }

            foreach (var set in coordinateSet)
            {
                Console.WriteLine((int)set.Item1.Distance(set.Item2));
            }
        }
        public struct Point
        {
            public int X { get; private set; }
            public int Y { get; private set; }

            public Point(int x, int y) : this()
            {
                X = x;
                Y = y;
            }

            public double Distance(Point point)
            {
                return Math.Sqrt(Math.Pow((X - point.X), 2D) + Math.Pow((Y - point.Y), 2D));
            }
        }
    }
}
