module InputProvider

open System.IO
open System.Net
open System.Net.Http
open System

let private cookieContainer = new CookieContainer()
let private httpClientHandler = new HttpClientHandler()
let private httpClient = new HttpClient(httpClientHandler)
let private baseUrl = new Uri("https://adventofcode.com")
let private cacheDir = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)

let private directory = "../../../Inputs/Advent of Code/"

let private getInputFilePath year day = 
    let path = Path.Combine(directory + year + "/", year + "-" + day + ".txt")
    new FileInfo(path)
let private getInputUrl year day =
    new Uri(baseUrl, Path.Combine(year, "day", day, "input"))

let private removeLastEmptyLine lines =
    if Array.last lines = "" then
        lines |> Array.take(Array.length lines - 1)
    else
        lines

let private setToken token =
    if cookieContainer.Count = 0 then
        let cookie = new Cookie("session", token)
        cookie.Domain <- baseUrl.Host
        cookieContainer.Add(cookie)
        httpClientHandler.CookieContainer <- cookieContainer

let private downloadInputAsync year day token =
    printfn "Downloading input.."
    setToken token
    async {
       let! result = httpClient.GetAsync(getInputUrl year day) |> Async.AwaitTask
       let! content = result.Content.ReadAsStringAsync() |> Async.AwaitTask
       let file = getInputFilePath year day
       let splitLines = content.Split('\n') |> removeLastEmptyLine

       if not(Directory.Exists(directory)) then
            Directory.CreateDirectory(directory) |> ignore
       if not(Directory.Exists(directory + year + "/")) then
            Directory.CreateDirectory(directory + year + "/") |> ignore

       File.WriteAllLinesAsync(file.FullName, splitLines) |> Async.AwaitTask |> ignore
       return splitLines
    }

let getPuzzleInput year day =
        let file = getInputFilePath year day
        async {
            let! tokenFileData = File.ReadAllLinesAsync("authtoken.txt") |> Async.AwaitTask
            let token = tokenFileData.[0]
            if file.Exists then
                let! input = File.ReadAllLinesAsync(file.FullName) |> Async.AwaitTask
                return input
            else 
                let! input = downloadInputAsync year day token
                return input
        }
        