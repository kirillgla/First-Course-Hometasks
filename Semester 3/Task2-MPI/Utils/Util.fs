// I decided I was feeling bored, so let's do some F#
namespace Utils

open JetBrains.Annotations
open System
open System.Linq
open System.Runtime.CompilerServices

[<Extension>]
type Util() =
    
    [<Extension>]
    static member BitAt(number : Int32, index) =
        match index with
        | bad when bad < 0 || bad >= 32 -> 
            bad.ToString()
            |> ArgumentOutOfRangeException
            |> raise
        | 0 -> (number % 2) <> 0
        | good -> (number / 2).BitAt(good - 1)
    
    [<Extension>]
    static member IsPowerOfTwo number =
        match number with
        | bad when bad <= 0 -> false
        | 1 -> true
        | good when good % 2 = 0 -> (good / 2).IsPowerOfTwo()
        | _ -> false
    
    [<Extension>]
    static member IntegralBinaryLogarithm number =
        match number with
        | 1 -> 0
        | bad when bad <= 0 || bad % 2 <> 0 -> 
            String.Format("{0} is not a power of two", bad)
            |> ArgumentException
            |> raise
        | good -> 1 + (good / 2).IntegralBinaryLogarithm()
    
    [<Extension>]
    static member IntegralBinaryExponent number =
        match number with
        | bad when bad < 0 || bad >= 32 -> 
            bad.ToString()
            |> ArgumentOutOfRangeException
            |> raise
        | good -> 1 <<< good
    
    [<Extension>]
    static member ForEach(list, [<NotNull>] f : Action<'t>) =
        for element in list do
            f.Invoke element
    
    [<NotNull>]
    static member private LowerCombinations index =
        match index with
        | bad when bad < 0 -> [ 0 ]
        | good -> 
            [ for undercombinated in Util.LowerCombinations(good - 1) do
                  yield undercombinated
                  yield undercombinated ||| good.IntegralBinaryExponent() ]
    
    [<Extension>]
    [<NotNull>]
    static member UntruncateAfter initial index =
        let truncated = initial.TruncateAfter(index)
        (Util.LowerCombinations index).Select(fun lower -> lower ||| truncated)
    
    /// <summary>
    /// Replaces all bits of the number after [index] with zero.
    /// [index] is also replaced. 
    /// </summary>
    /// <param name="source">The number to truncate</param>
    /// <param name="index">index of first bit to be truncated.
    /// Bits are counted from left to right, starting with 0</param>
    /// <returns>Truncated number</returns>
    /// <exception cref="ArgumentException">Thrown if index is out of range</exception>
    [<Extension>]
    static member TruncateAfter(source, index) =
        match index with
        | bad when bad < 0 || bad >= 32 -> ArgumentException () |> raise
        | good -> 
            let truncationSize = good + 1
            (source >>> truncationSize) <<< truncationSize
