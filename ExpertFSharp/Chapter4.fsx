open System.Collections.Generic


(* Memoize *)
// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.
let time f = 
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = f()
    let finish = sw.Stop()
    (res, sw.Elapsed.TotalMilliseconds |> sprintf "%f ms")

let rec fib n = 
    if n <= 2 then 1
    else fib (n - 1) + fib (n - 2)

let memoize f = 
    let t = new Dictionary<_, _>(HashIdentity.Structural)
    fun n -> 
        if t.ContainsKey(n) then t.[n]
        else 
            let res = f n
            t.Add(n, res)
            res

#nowarn "40" //do not warn on recursive computer objects and functions

let rec fibFast = 
    memoize (fun n -> 
        if n <= 2 then 1
        else fibFast (n - 1) + fibFast (n - 2))

type Table<'T, 'U> = 
    abstract Item : 'T -> 'U with get
    abstract Disacrd : unit -> unit

let memoizeAndPermitDiscard (f: 'T -> 'U) = 
    let lookasideTable = new Dictionary<'T, 'U>(HashIdentity.Structural)
    { new Table<'T, 'U> with
          
          member t.Item 
              with get (n) = 
                  if lookasideTable.ContainsKey(n) then lookasideTable.[n]
                  else 
                      let res = f n
                      lookasideTable.Add(n, res)
                      res
          
          member t.Disacrd() = lookasideTable.Clear() }

#nowarn "40" //do not warn on recursive computer objects and functions
let rec fibFastObject =
    memoizeAndPermitDiscard (
        fun n -> 
            printfn "computing FibFast %d" n
            if n <= 2 then 1 
            else fibFastObject.[n - 1] + fibFastObject.[n - 2])


(* Lazy *)

let sixty = lazy (30 + 30)

sixty.Force()

let sixtyWithSideEffects = lazy(printfn "Hello World"; 30 + 30)

sixtyWithSideEffects.Force()

(* Mutable Reference Cells *)
let cell1 = ref 1

cell1.Value

cell1 := 3


