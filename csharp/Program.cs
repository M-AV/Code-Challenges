﻿using CodeEval_Solutions.Easy.Swap_Elements;
using CodeEval_Solutions.Moderate.Data_Recovery;
using csharp.AdventOfCode._2021;
using csharp.AdventOfCode._2022;
using System.Diagnostics;

static void PrepareGC()
{
    GC.Collect();
    GC.WaitForPendingFinalizers();
    GC.Collect();
}

string input = "./../../../../fsharp/Inputs/Advent of Code/2022/2022-7.txt";// Console.ReadLine();

await Day7.Execute(input);

return;


var ticksV1 = new List<long>();
var ticksV2 = new List<long>();
var ticksV3 = new List<long>();
var ticksV4 = new List<long>();
var ticksV5 = new List<long>();
var ticksV6 = new List<long>();
var ticksV7 = new List<long>();
var ticksV8 = new List<long>();
var ticksV9 = new List<long>();
var ticksV10 = new List<long>();

var results = new[] {
    ticksV1, ticksV2, ticksV3, ticksV4, ticksV5, ticksV6, ticksV7,
    ticksV8, ticksV9, ticksV10
};

PrepareGC();

int iterations = 10000;
Stopwatch watch = new Stopwatch();
for (int i = 0; i < iterations; i++)
{
    watch.Start();
    (new SwapElementsV2()).Run(new[] { "C://input.txt" });
    watch.Stop();
    if (i != 0 && (i + 1) != iterations)
        ticksV1.Add(watch.ElapsedTicks);
    watch.Reset();
}
X.PrintResult(results);

public static class X
{
    // For some reason these conflict with eachother if not in a class.
    public static void PrintResult(params List<long>[] ticks)
    {
        for (int index = 0; index < ticks.Length; index++)
        {
            if (ticks[index].Any())
            {
                Console.WriteLine((index + 1) + ".  Avg: " + ticks[index].Average() + "   Max: " + ticks[index].Max() + "  Min: " + ticks[index].Min());
            }
        }
    }

    public static void PrintResult(params List<TimeSpan>[] ticks)
    {
        for (int index = 0; index < ticks.Length; index++)
        {
            if (ticks[index].Any())
            {
                Console.WriteLine((index + 1) + ".  Avg: " + new TimeSpan(Convert.ToInt64(ticks[index].Average(x => x.Ticks))) +
                                  "   Max: " + new TimeSpan(Convert.ToInt64(ticks[index].Max(x => x.Ticks))) +
                                  "   Min: " + new TimeSpan(Convert.ToInt64(ticks[index].Min(x => x.Ticks))));
            }
        }
    }
}