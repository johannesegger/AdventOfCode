using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;

BenchmarkRunner.Run<Day07Benchmark>();

class Dir
{
    public Dir? Parent { get; set; }
    public int Size { get; set; }
}

public class Day07Benchmark
{
    [Benchmark]
    public int Solve()
    {
        var lines = File.ReadLines("input.txt");
        var path = "";
        Dir? dir = null;
        List<Dir> dirs = new();
        foreach (var line in lines)
        {
            if (line == "$ cd /")
            {
                path = "";
                dir = new Dir();
                dirs.Add(dir);
            }
            else if (line == "$ cd ..")
            {
                path = path.Substring(0, path.LastIndexOf('/'));
                dir = dir?.Parent;
            }
            else if (line.StartsWith("$ cd "))
            {
                path = $"{path}/{line.Substring(5)}";
                var childDir = new Dir { Parent = dir };
                dir = childDir;
                dirs.Add(dir);
            }
            else if (char.IsDigit(line[0]))
            {
                var size = int.Parse(line.Substring(0, line.IndexOf(' ')));
                var parent = dir;
                while (parent != null)
                {
                    parent.Size += size;
                    parent = parent.Parent;
                }
            }
        }

        var part1 = dirs.Select(v => v.Size).Where(v => v < 100_000).Sum();
        Console.WriteLine($"Part 1: {part1}");

        var totalUsedSpace = dirs[0].Size;
        var freeSpace = 70_000_000 - totalUsedSpace;
        var spaceToFree = 30_000_000 - freeSpace;

        var part2 = dirs
            .Select(v => v.Size)
            .Where(size => size >= spaceToFree)
            .Min();
        Console.WriteLine($"Part 2: {part2}");
        return part1;
    }
}