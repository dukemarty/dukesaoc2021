// Learn more about F# at http://fsharp.org

open System
open AoC.Utils


let part1Filtered2Times filter image =
    let extImage = ImageData.extendImageTriple image
    //printfn "  Extended image:\n  %s" (String.concat "\n  " extImage)
    let filteredOnceImage = ImageData.applyFilter filter extImage
    //printfn "  Filtered image:\n  %s" (String.concat "\n  " filteredOnceImage)
    let resImage = ImageData.applyFilter filter filteredOnceImage
    printfn "  Result image  :\n  %s" (String.concat "\n  " resImage)
    let res = ImageData.countLitPixels resImage
    printfn "Part 1, #lit pixels: %d" res

let part2Filtered50Times filter image =
    let extImage = ImageData.extendImage image 110 110
    let mutable filteredImage = extImage
    for _ in 1..50 do
        filteredImage <- ImageData.applyFilter filter filteredImage
    printfn "  Result image  :\n  %s" (String.concat "\n  " filteredImage)
    let res = ImageData.countLitPixels filteredImage
    printfn "Part 2, #lit pixels: %d" res

[<EntryPoint>]
let main argv =
    printfn "Day 20: Trench Map\n==================\n"
    let filter, image = ImageData.loadFilterAndImage DataInput.Puzzle
    //printfn "Filter: %s" filter
    //printfn "Image:\n%A" image
    part1Filtered2Times filter image
    part2Filtered50Times filter image
    0 // return an integer exit code
