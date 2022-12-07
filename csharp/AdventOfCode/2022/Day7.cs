using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace csharp.AdventOfCode._2022;

public record F(string Name, int Size);
public record Folder(string Name, Folder Parent, Dictionary<string, Folder> Folders, List<F> Files);

public static class Day7
{
    public static async Task Execute(string inputPath)
    {
        var lines = await File.ReadAllLinesAsync(inputPath);

        var parsed = ParseLines(lines);

        // Print(parsed, 1);

        var res = new List<(Folder, int)>();
        var totalSize = Part1(parsed, res);

        Console.WriteLine("Part1: " + res.Sum(r => r.Item2));

        var totalDiskSpace = 70000000;
        var spaceNeeded = 30000000;
        var spaceToRemove = totalDiskSpace - totalSize;

        Console.WriteLine("Part2: " + Part2(parsed));

    }

    private static int Part1(Folder parsed, List<(Folder, int)> result, int sizeWereLookingFor = 100000)
    {
        var fileSize = parsed.Files.Sum(f => f.Size);
        var folderSize = parsed.Folders.Values.Sum(f => Part1(f, result, sizeWereLookingFor));
        if (fileSize + folderSize <= sizeWereLookingFor)
        {
            result.Add((parsed, fileSize + folderSize));
        }
        return fileSize + folderSize;
    }

    private static int Part2(Folder parsed)
    {
        var folderSizes = new List<(Folder, int)>();
        var rootSize = Part1(parsed, folderSizes, int.MaxValue);

        var spaceToRemove = 30000000 - (70000000 - rootSize);

        var result = folderSizes.OrderBy(f => f.Item2).First(f => f.Item2 > spaceToRemove);
        return result.Item2;
    }

    private static void Print(Folder f, int depth)
    {
        var prefix = new String('-', depth) + " ";
        Console.WriteLine(new String('-', depth - 1) + " " + f.Name);
        foreach (var file in f.Files)
        {
            Console.WriteLine(prefix + "F: " + file.Name);
        }

        foreach (var dir in f.Folders)
        {
            Print(dir.Value, depth + 1);
        }
    }

    private static Folder ParseLines(string[] lines)
    {
        var root = new Folder("/", null, new Dictionary<string, Folder>(), new List<F>());
        var current = root;

        foreach (var line in lines)
        {
            if (line.StartsWith("$ cd /"))
            {
                current = root;
            }
            else if (line.StartsWith("$ ls"))
            {
                // ignore
            }
            else if (line.StartsWith("$ cd .."))
            {
                if (current == root) { continue; }
                current = current.Parent;
            }
            else if (line.StartsWith("$ cd "))
            {
                var folderName = line[5..];
                current = current.Folders[folderName];
            }
            else if(line.StartsWith("dir "))
            {
                var folderName = line[4..];
                current.Folders.Add(folderName, new Folder(folderName, current, new Dictionary<string, Folder>(), new List<F>()));
            }
            else
            {
                var split = line.Split(' ');
                var file = new F(split[1], int.Parse(split[0]));
                current.Files.Add(file);
            }
        }

        return root;
    }
}
