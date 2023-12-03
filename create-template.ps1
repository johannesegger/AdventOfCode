param (
    [Parameter(Mandatory = $true)][string]$Year
)

@(1..25) `
| Foreach-Object {
    $ProjectName = "Day{0:00}" -f $_
    $ProjectPath = ".\$Year\$ProjectName"
    if (Test-Path $ProjectPath) {
        Write-Warning "Skipping $ProjectName because folder exists"
    }
    else {
        Write-Output "Creating $ProjectName"
        dotnet new console -o $ProjectPath -lang f# | Out-Null
        $Content = @"
open System.IO

File.ReadAllLines `"input.txt`"
|> Seq.length
|> printfn `"Part 1: %d`"
"@
        $Content | Out-File $ProjectPath\Program.fs -Encoding utf8
        "" | Out-File $ProjectPath\input.txt
    }
}