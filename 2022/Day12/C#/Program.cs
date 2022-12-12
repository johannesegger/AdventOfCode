string[] input = File.ReadAllLines("input.txt");

int width = input[0].Length;
int height = input.Length;
int?[,] minSteps = new int?[height, width];

IEnumerable<Position> GetPositions(char c)
{
    for (int row = 0; row < height; row++)
    {
        for (int column = 0; column < width; column++)
        {
            if (input[row][column] == c)
            {
                yield return new Position(column, row);
            }
        }
    }
}

int GetHeight(Position p)
{
    return input[p.Y][p.X] switch
    {
        'S' => 'a',
        'E' => 'z',
        var c => c
    };
}

IEnumerable<Position> GetAllNeighbors(Position p)
{
    if (p.Y > 0) yield return new Position(p.X, p.Y - 1);
    if (p.Y < height - 1) yield return new Position(p.X, p.Y + 1);
    if (p.X > 0) yield return new Position(p.X - 1, p.Y);
    if (p.X < width - 1) yield return new Position(p.X + 1, p.Y);
}

bool TrySetMinSteps(Position position, int steps)
{
    int? currentSteps = minSteps[position.Y, position.X];
    if (!currentSteps.HasValue || currentSteps.Value > steps)
    {
        minSteps[position.Y, position.X] = steps;
        return true;
    }
    return false;
}

int Run(IEnumerable<Position> startPositions, Position endPosition)
{
    Stack<Position> positionsToEvaluate = new();
    foreach (Position p in startPositions)
    {
        positionsToEvaluate.Push(p);
        TrySetMinSteps(p, 0);
    }

    while (positionsToEvaluate.Count > 0)
    {
        Position position = positionsToEvaluate.Pop();

        foreach (Position neighbor in GetAllNeighbors(position))
        {
            if (GetHeight(neighbor) <= GetHeight(position) + 1)
            {
                if (TrySetMinSteps(neighbor, minSteps[position.Y, position.X]!.Value + 1))
                {
                    positionsToEvaluate.Push(neighbor);
                }
            }
        }
    }
    return minSteps[endPosition.Y, endPosition.X]!.Value;
}

Position start = GetPositions('S').Single();
Position end = GetPositions('E').Single();

Console.WriteLine($"Part 1: {Run(new[] { start }, end)}");
Console.WriteLine($"Part 2: {Run(GetPositions('a'), end)}");

public record Position(int X, int Y);
