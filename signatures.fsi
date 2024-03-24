

namespace FSharp

namespace ImageLibrary
    
    module Operations =
        
        val applySepia: int * int * int -> int * int * int
        
        val Sepia:
          width: int ->
            height: int ->
            depth: int ->
            image: (int * int * int) list list -> (int * int * int) list list
        
        val intensityHelper:
          double * double * double ->
            intensity: double -> color: char -> int * int * int
        
        val IncreaseIntensity:
          width: int ->
            height: int ->
            depth: int ->
            image: (int * int * int) list list ->
            intensity: double -> channel: char -> (int * int * int) list list
        
        val FlipHorizontal:
          width: int ->
            height: int ->
            depth: int ->
            image: (int * int * int) list list -> (int * int * int) list list
        
        val Rotate180:
          width: int ->
            height: int ->
            depth: int ->
            image: (int * int * int) list list -> (int * int * int) list list
        
        val isAboveThreshold:
          r: int ->
            g: int ->
            b: int ->
            otherR: int -> otherG: int -> otherB: int -> threshold: int -> bool
        
        val edgeHelper:
          topLst: (int * int * int) list ->
            bottomLst: (int * int * int) list ->
            threshold: int -> (int * int * int) list
        
        val EdgeDetect:
          width: int ->
            height: int ->
            depth: int ->
            image: (int * int * int) list list ->
            threshold: int -> (int * int * int) list list

