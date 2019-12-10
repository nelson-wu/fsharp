namespace FSharp.Compiler.UnitTests

open NUnit.Framework
open FSharp.Compiler.SourceCodeServices

[<TestFixture>]
module CustomCollectionTests =
    [<Test>]
    let ``Custom collection with Item and GetReverseIndex should support reverse index mutation``() =
        CompilerAssert.CompileExeAndRunWithOptions [| "--langversion:preview" |]
            """
open System
type foo() = 
    let mutable i = ""
    member this.GetReverseIndex(_x: int, y: string) = y + " "
    member __.Item with get (_x: string) = i and set idx value = i <- idx + value

let a = foo()
a.[^"2"] <- "-1"

if a.["2"] <> "2 -1" then failwithf "expected 2 -1 but got %A" a.["2"]
             """

    [<Test>]
    let ``Custom collection with GetSlice and GetReverseIndex should support reverse index set slicing``() =
        CompilerAssert.CompileExeAndRunWithOptions [| "--langversion:preview" |]
            """
open System
type foo() = 
    let mutable i = ""
    member this.GetReverseIndex(_x: int, y: string) = y + " "
    member this.SetSlice(x1: string option, x2: string option, source: string) = i <- x1.Value + x2.Value + source
    member this.GetSlice(_: string option, _: string option) = i

let a = foo()
a.[^"2"..^"1"] <- "-1"

if a.["2".."1"] <> "2 1 -1" then failwithf "expected 2 1 -1 but got %A" a.["2".."1"]           
            """
 
    [<Test>]
    let ``Custom collection with Item and GetReverseIndex should support reverse index indexing``() =
        CompilerAssert.CompileExeAndRunWithOptions [| "--langversion:preview" |]
            """
open System

type foo() = 
    member this.GetReverseIndex(x: int, y: int) = 10 + x + y
    member this.Item(x: int) = x

let a = foo()

if a.[^2] <> 12 then failwith "expected 12"
            """

    [<Test>]
    let ``Custom collection with Item and GetReverseIndex should support n-rank reverse index mutation``() =
        CompilerAssert.CompileExeAndRunWithOptions [| "--langversion:preview" |]
            """
open System

type foo() = 
    let mutable i = ""
    member this.GetReverseIndex(x: int, y: string) = x.ToString() + " " + y
    member __.Item with get (_x: string) = i and set (idx1, idx2) value = i <- idx1 + " " + idx2 + " " + value

let a = foo()
a.[^"1",^"2"] <- "3"

if a.[""] <> "0 1 1 2 3" then failwithf "expected 0 1 1 2 3 but got %A" a.[""]
            """

    [<Test>]
    let ``Custom collection with Item and GetReverseIndex should support n-rank reverse index indexing``() =
        CompilerAssert.CompileExeAndRunWithOptions [| "--langversion:preview" |]
            """
open System

type foo() = 
    member this.GetReverseIndex(x: int, y: int) = 10 + x + y
    member this.Item(x: int, y:int) = x + y

let a = foo()

if a.[^2,^1] <> 24 then failwithf "expected 23 but got %A" a.[^2,^1]
            """

    [<Test>]
    let ``Custom collection with Item and no GetReverseIndex should not support reverse index indexing``() =
        CompilerAssert.TypeCheckSingleError
            """
open System

type foo() = 
    member this.Item(x: int) = x

let a = foo()

if a.[^2] <> 12 then failwith "expected 12"
            """
            FSharpErrorSeverity.Error
            39
            (9,7,9,9)
            "The field, constructor or member 'GetReverseIndex' is not defined."


    [<Test>]
    let ``Custom collection with GetSlice and GetReverseIndex should support reverse index slicing``() =
        CompilerAssert.CompileExeAndRunWithOptions [| "--langversion:preview" |]
            """
open System

type foo() = 
    member this.GetSlice(x: int option, y: int option) = 
        match x, y with
        | Some(a), Some(b) -> a + b
        | _ -> failwith "not expected"

    member this.GetReverseIndex(x: int, y: int) = 10 + x + y

let a = foo()

if a.[^2..1] <> 13 then failwith "expected 13"
            """
 
    [<Test>]
    let ``Custom collection without GetReverseIndex should not support reverse index slicing``() =
        CompilerAssert.TypeCheckSingleErrorWithOptions [| "--langversion:preview" |]
            """
open System

type foo() = 
    member this.GetSlice(x: int option, y: int option) = 
        match x, y with
        | Some(a), Some(b) -> a + b
        | _ -> failwith "not expected"

let a = foo()

if a.[^2..1] <> 13 then failwith "expected 13"
            """
            FSharpErrorSeverity.Error
            39
            (12,7,12,9)
            "The field, constructor or member 'GetReverseIndex' is not defined."

    [<Test>]
    let ``Custom collection with only 2D slicing should return error for 1D slice``() =
        CompilerAssert.TypeCheckSingleError
            """
open System

type Foo<'a>() =
    let mutable m_lastLB1 : 'a option = None
    let mutable m_lastUB1 : 'a option = None

    let mutable m_lastLB2 : 'a option = None
    let mutable m_lastUB2 : 'a option = None


    member this.GetSlice(lb1, ub1, lb2, ub2) = 
        m_lastLB1 <- lb1
        m_lastUB1 <- ub1
        m_lastLB2 <- lb2
        m_lastUB2 <- ub2
        ()

    member this.LastLB1 = m_lastLB1
    member this.LastUB1 = m_lastUB1
    member this.LastLB2 = m_lastLB2
    member this.LastUB2 = m_lastUB2

let f = new Foo<char>()

let _ = f.[*]
            """
            FSharpErrorSeverity.Error
            501
            (26,9,26,14)
            "The member or object constructor 'GetSlice' takes 4 argument(s) but is here given 2. The required signature is 'member Foo.GetSlice : lb1:'a option * ub1:'a option * lb2:'a option * ub2:'a option -> unit'."

    [<Test>]
    let ``Custom collection should be able to support n-rank slicing``() =
        CompilerAssert.RunScript
            """
open System

type Foo() =
    member _.GetSlice(a: int, [<ParamArray>] arr: int option array) = 
        a, arr

let x = Foo()

let a = x.[0, 1.., 2..3, ..4, *]

if a <> (0, [| Some 1; None; Some 2; Some 3; None; Some 4; None; None |]) then failwithf "got %A" a
            """
            []
