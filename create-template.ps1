param (
    [Parameter(Mandatory = $true)][string]$Year
)

@(1..12) `
| Foreach-Object {
    $ProjectName = "Day{0:00}" -f $_
    $ProjectPath = "./$Year/$ProjectName"
    if (Test-Path $ProjectPath) {
        Write-Warning "Skipping $ProjectName because folder exists"
    }
    else {
        Write-Output "Creating $ProjectName"
        dotnet new console -o $ProjectPath -lang f# | Out-Null
        $Content = @"
open Checked
open System.IO

File.ReadAllLines `"input.txt`"
|> Seq.length
|> printfn `"Part 1: %d`"
"@
        $Content | Out-File $ProjectPath/Program.fs -Encoding utf8
        New-Item $ProjectPath/input.txt | Out-Null
    }
}