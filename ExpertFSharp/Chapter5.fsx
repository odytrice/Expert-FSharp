
open System.IO
open System.Runtime.Serialization.Formatters.Binary

let serialization () = 
    let writeValue outputStream x = 
        let formatter = new BinaryFormatter()
        formatter.Serialize(outputStream, x)

    let readValue inputStream = 
        let formatter = new BinaryFormatter()
        let res = formatter.Deserialize(inputStream)
        unbox res

    let addresses = 
        Map.ofList [ "Jeff", "123 Main Street, Redmond, WA 98052"
                     "Fred","987 Pine Road, Phila., PA 19116"
                     "Mary", "PO Box 112233, Palo Alto, CA 94301" ]


    use fsOut = new FileStream("Data.dat",FileMode.Create)
    writeValue fsOut addresses
    fsOut.Close()

    use fsIn = new FileStream("Data.dat",FileMode.Open)
    let res: Map<string,string> = readValue fsIn
    fsIn.Close()
    res

type Numeric<'T> = { Zero: 'T; Subtract: ('T -> 'T -> 'T); LessThan: ('T -> 'T -> bool) }

let GenericFunctions () =
    let hcf (ops:Numeric<'T>) = 
        let rec hcf a b =
            if a = ops.Zero then b
            elif ops.LessThan a b then hcf a (ops.Subtract b a)
            else hcf (ops.Subtract a b) a    
        hcf

    let intOps = { Zero = 0; Subtract = (-); LessThan = (<) }

    hcf intOps

let inline printAsFloatingPoint number = printfn "%f" (float number)