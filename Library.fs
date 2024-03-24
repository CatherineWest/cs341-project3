//
// F# image processing functions.
//
// More details?
//
// Name Catherine West School UIC Date Spring 2024
//

namespace ImageLibrary

open System.Collections.Generic

module Operations =
  //
  // all functions must be indented
  //

  // let (oldR,oldG,oldB) = (float r,float g,float b)
  // (min (255, int (0.393*oldR + 0.769*oldG + 0.189*oldB)),
  // min (255, int 0.349*oldR + 0.686*oldG + 0.168*oldB),
  // min (255, int (0.272*oldR + 0.534*oldG + 0.131*oldB)))

  let applySepia pixel:(int*int*int) =
    let (r,g,b) = pixel
    let newR = int (0.393*double r + 0.769*double g + 0.189*double b)
    let newG = int (0.349*double r + 0.686*double g+ 0.168*double b)
    let newB = int (0.272*double r+ 0.534*double g+ 0.131*double b)
    (min newR 255,min newG 255, min newB 255)
  //  Sepia:
  //
  // Applies a sepia filter onto the image and returns the 
  // resulting image as a list of lists. 
  // The sepia filter adjusts the RGB values of each pixel
  // according to the following formulas:
  //    newRed = 0.393*origRed + 0.769*origGreen + 0.189*origBlue
  //    newGreen = 0.349*origRed + 0.686*origGreen + 0.168*origBlue
  //    newBlue = 0.272*origRed + 0.534*origGreen + 0.131*origBlue
  // We will use truncation to cast from the floating point result 
  // to the integer value.
  // 
  // If any of these values exceed 255, then 255 should be used
  // instead for that value.
  //
  // Returns: updated image.
  //

  let rec Sepia (width:int) (height:int) (depth:int) (image:(int*int*int) list list)= 
    List.map(fun(lst) -> (List.map(fun(r:int,g:int,b:int)->(applySepia (r,g,b))) lst)) image
    
  let intensityHelper pixel intensity color =
    let (r:double,g:double,b:double) = pixel
    if color = 'r' then ((min 255 (int (r*intensity)),int g,int b))
    elif color = 'g' then (int r,min 255 (int (g*intensity)),int b)
    elif color = 'b' then (int r,int g, min 255 (int (b*intensity)))
    else (int r, int g, int b)
  
  //
  // Increase Intensity
  //
  // Increase the intensity of a particular RGB channel
  // according to the values of the parameters.
  // The intensity is the scaling factor by which the
  // channel selected should be increased (or decreased 
  // if the value is less than 1).
  // The channel is one of 'r', 'g', or 'b' which 
  // correspond to red, green, and blue respectively.
  // If the channel is not one of those three values,
  // do not modify the image.
  // Remember that the maximum value for any pixel 
  // channel is 255, so be careful of overflow!
  //
  // Returns: updated image.
  //
  let rec IncreaseIntensity (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (intensity:double)
                    (channel:char) = 
    // for now, just return the image back, i.e. do nothing:
    List.map(fun(lst) -> (List.map(fun(r:int,g:int,b:int)->(intensityHelper (float r,float g, float b) intensity channel))lst)) image
    


  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    // for now, just return the image back, i.e. do nothing:
    List.map(fun(lst) -> (List.rev lst)) image

  //
  // Rotate180:
  //
  // Rotates the image 180 degrees.
  //
  // Returns: updated image.
  //
  let rec Rotate180 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    // for now, just return the image back, i.e. do nothing:
    FlipHorizontal width height depth (List.rev image)
    


  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "significantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compare each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //

  let isAboveThreshold (r:int) (g:int) (b:int) (otherR:int) (otherG:int) (otherB:int) (threshold:int) =
    if int (sqrt ((double (r-otherR)**2) + (double (g-otherG)**2) + (double (b-otherB)**2))) > threshold then true else false

  let rec edgeHelper (topLst:(int*int*int) list) (bottomLst:(int*int*int) list) (threshold:int) =
    match (topLst,bottomLst) with
    | ((r,g,b)::(rr,rg,rb)::[],(br,bg,bb)::tail) -> if (isAboveThreshold r g b rr rb rg threshold) || (isAboveThreshold r g b br bg bb threshold) then [(0,0,0)] else [(255,255,255)]
    | ((r,g,b)::(rr,rg,rb)::primaryTl,(br,bg,bb)::bottomTl) -> if (isAboveThreshold r g b rr rb rg threshold) || (isAboveThreshold r g b br bg bb threshold) then (0,0,0)::(edgeHelper primaryTl bottomTl threshold) else (255,255,255)::(edgeHelper primaryTl bottomTl threshold)
  
      
  let rec EdgeDetect (width:int)
                     (height:int)
                     (depth:int)
                     (image:(int*int*int) list list)
                     (threshold:int) = 
    match image with
    | primary::secondary::[] -> [edgeHelper primary secondary threshold]
    | primary::secondary::tail -> edgeHelper primary secondary  threshold::EdgeDetect width height depth tail threshold