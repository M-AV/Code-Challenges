namespace csharp.AdventOfCode._2021;

public record struct Point(int X, int Y);
public record Line(Point Start, Point End);

/// <summary>
/// Did a quick implementation in C# to help debug the F# solution
/// </summary>
public static class Day5
{
    public static async Task Execute(string inputPath)
    {
        var file = await File.ReadAllLinesAsync(inputPath);

        var (lines, maxValue) = ParseLines(file);

        var grid = new int[maxValue + 1, maxValue + 1];

        int overlapping = 0;
        foreach (var line in lines.Where(l => l.Start.X == l.End.X || l.Start.Y == l.End.Y))
        {
            if (line.Start.X == line.End.X)
            {
                var start = Math.Min(line.Start.Y, line.End.Y);
                var end = Math.Max(line.Start.Y, line.End.Y);
                for (int i = start; i <= end; i++)
                {
                    grid[line.Start.X, i]++;
                }
            }
            else
            {
                var start = Math.Min(line.Start.X, line.End.X);
                var end = Math.Max(line.Start.X, line.End.X);
                for (int i = start; i <= end; i++)
                {
                    grid[i, line.Start.Y]++;
                }
            }
        }

        foreach (var point in grid)
        {
            if (point > 1) { overlapping++; }
        }

        Console.WriteLine("## Part 1:");
        Console.WriteLine("Overlapping: " + overlapping);

    }

    private static (List<Line> Lines, int MaxValue) ParseLines(string[] lines)
    {
        var result = new List<Line>(lines.Length);
        var maxValueSeen = 0;
        foreach (var line in lines)
        {
            var parts = line.Split(" -> ");
            var first = parts[0].Split(',');
            var second = parts[1].Split(',');
            var start = new Point(int.Parse(first[0]), int.Parse(first[1]));
            var end = new Point(int.Parse(second[0]), int.Parse(second[1]));
            if (start.X > maxValueSeen) maxValueSeen = start.X;
            if (start.Y > maxValueSeen) maxValueSeen = start.Y;
            if (end.X > maxValueSeen) maxValueSeen = end.X;
            if (end.Y > maxValueSeen) maxValueSeen = end.Y;
            result.Add(new Line(start, end));
        }
        return (result, maxValueSeen);
    }
}
