module UnitMath

open NUnit.Framework

type Number = list<unit list>

let printJson<'a> = System.Text.Json.JsonSerializer.Serialize >> System.Console.WriteLine

let inline mid<'a> (act: 'a -> unit) v =
    act v
    v

let pow v n =
    let rec pow v n acc =
        match n with
        | 0 -> acc
        | n -> pow v (n - 1) (v * acc)
    pow v n 1

let rec convert : Number -> int = fun n ->
    [
        for i = 0 to (n |> List.length) - 1 do
            yield n[i] |> List.length
    ]
    |> List.rev
    |> mid printJson
    |> List.mapi (fun i v -> if i = 0 then v else v * (pow 10 i))
    |> mid printJson
    |> List.sum

type ``Unit math tests`` () =

    [<Test>]
    member this.``55 convert works properly`` () =
        let ``55`` = [ [(); (); (); (); ()]; [(); (); (); (); ()] ]
        Assert.AreEqual(55, convert ``55``)
    
    [<Test>]
    member this.``551 convert works properly`` () =
        let ``551`` = [ [(); (); (); (); ()]; [(); (); (); (); ()]; [()] ]
        Assert.AreEqual(551, convert ``551``)

    [<Test>]
    member this.``5501 convert works properly`` () =
        let ``5501`` = [ [(); (); (); (); ()]; [(); (); (); (); ()]; []; [()] ]
        Assert.AreEqual(5501, convert ``5501``)