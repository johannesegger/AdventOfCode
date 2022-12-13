let input = require("./input")

let compare = (a, b) =>
{
    if (Number.isInteger(a) && Number.isInteger(b))
    {
        if (a < b) return 1;
        if (a > b) return -1;
        return 0;
    }
    if (Number.isInteger(a))
    {
        a = [a]
    }
    if (Number.isInteger(b))
    {
        b = [b]
    }

    for (let i = 0; i < Math.max(a.length, b.length); i++)
    {
        if (i >= a.length)
        {
            return 1;
        }
        if (i >= b.length)
        {
            return -1;
        }
        let result = compare(a[i], b[i])
        if (result !== 0)
        {
            return result;
        }
    }
    return 0;
}

let part1 = 0;
for (let i = 0; i < input.length; i += 2)
{
    if (compare(input[i], input[i + 1]) > 0)
    {
        part1 += (i / 2) + 1;
    }
}
console.log("Part 1: ", part1);

let dividerPackets = [ [[2]], [[6]] ]
input.push(...dividerPackets)

input.sort((a, b) => -compare(a, b))

let part2 = dividerPackets
    .map(p => input.indexOf(p) + 1)
    .reduce((a, b) => a * b)
console.log("Part 2: ", part2)
