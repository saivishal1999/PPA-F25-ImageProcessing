(* CannyEdge.sml
 *
 * Sequential Canny-like edge detector for grayscale images.
 *
 * Input:  image_matrix.txt
 *   First line: H W C   (C must be 1 – grayscale)
 *   Then H lines, each W integers (0..255)
 *
 * Output: canny_edges.txt
 *   First line: H W 1
 *   Then H lines, each W integers (0 or 255)
 *)

structure CannyEdge =
struct

  (* ---------- Basic utilities ---------- *)

  fun tokens line = String.tokens Char.isSpace line

  fun toInt s =
    case Int.fromString s of
      SOME n => n
    | NONE => raise Fail ("Cannot parse int: " ^ s)

  (* Read header: "H W C" *)
  fun readHeader (filename : string) : int * int * int =
    let
      val ins = TextIO.openIn filename
      val lineOpt = TextIO.inputLine ins
      val _ = TextIO.closeIn ins
      val line =
        (case lineOpt of
           SOME s => s
         | NONE => raise Fail "Empty file: no header")
      val toks = tokens line
    in
      case toks of
        [hStr, wStr, cStr] => (toInt hStr, toInt wStr, toInt cStr)
      | _ => raise Fail "Header must be: H W C"
    end

  (* List length and helpers *)
  fun length xs = List.foldl (fn (_, acc) => acc + 1) 0 xs

  fun minList (x :: xs) = List.foldl Int.min x xs
    | minList []        = raise Fail "minList on empty list"

  fun maxList (x :: xs) = List.foldl Int.max x xs
    | maxList []        = raise Fail "maxList on empty list"

  (* ---------- Read grayscale image as int list list ---------- *)

  type grayImage = int list list

  fun readGrayImage (filename : string) : grayImage =
    let
      val ins = TextIO.openIn filename

      (* skip header *)
      val _ =
        (case TextIO.inputLine ins of
           NONE => raise Fail "Missing header line"
         | SOME _ => ())

      fun parseLine line =
        let val toks = tokens line in List.map toInt toks end

      fun loop rows =
        case TextIO.inputLine ins of
          NONE =>
            (TextIO.closeIn ins; List.rev rows)
        | SOME line =>
            let
              val row = parseLine line
            in
              if List.null row then loop rows
              else loop (row :: rows)
            end
    in
      loop []
    end

  (* ---------- Vector helpers ---------- *)

  (* polymorphic dims: works for any 'a vec2 *)
  fun dims v2 =
    let
      val h = Vector.length v2
      val w =
        if h = 0 then 0
        else Vector.length (Vector.sub (v2, 0))
    in
      (h, w)
    end

  (* int vec2 from grayImage *)
  fun toIntVec2 (img : grayImage) : int Vector.vector Vector.vector =
    let
      fun rowToVec row = Vector.fromList row
    in
      Vector.fromList (List.map rowToVec img)
    end

  (* real vec2 from grayImage *)
  fun toRealVec2 (img : grayImage) : Real.real Vector.vector Vector.vector =
    let
      fun rowToVec row =
        Vector.fromList (List.map Real.fromInt row)
    in
      Vector.fromList (List.map rowToVec img)
    end

  (* ---------- 3x3 Gaussian blur (integer arithmetic) ---------- *)

  (* Kernel:
      [1 2 1
       2 4 2
       1 2 1] / 16
   * We do all arithmetic in int and then div 16.
   *)

  fun gaussianBlur (img : grayImage) : grayImage =
    let
      val v2 = toIntVec2 img
      val (h, w) = dims v2

      fun getInt (y : int, x : int) : int =
        if y < 0 orelse y >= h orelse x < 0 orelse x >= w then 0
        else Vector.sub (Vector.sub (v2, y), x)

      fun blurPixel (y : int, x : int) : int =
        let
          val p00 = getInt (y-1, x-1)
          val p01 = getInt (y-1, x)
          val p02 = getInt (y-1, x+1)
          val p10 = getInt (y,   x-1)
          val p11 = getInt (y,   x)
          val p12 = getInt (y,   x+1)
          val p20 = getInt (y+1, x-1)
          val p21 = getInt (y+1, x)
          val p22 = getInt (y+1, x+1)

          val sum =
            1 * p00 + 2 * p01 + 1 * p02 +
            2 * p10 + 4 * p11 + 2 * p12 +
            1 * p20 + 2 * p21 + 1 * p22

          val valInt = sum div 16
        in
          if valInt < 0 then 0 else if valInt > 255 then 255 else valInt
        end

      fun rowLoop (y : int, x : int) : int list =
        if x = w then []
        else blurPixel (y, x) :: rowLoop (y, x + 1)

      fun allRows (y : int) : grayImage =
        if y = h then []
        else rowLoop (y, 0) :: allRows (y + 1)
    in
      allRows 0
    end

  (* ---------- Sobel Gx, Gy, magnitude ---------- *)

  fun sobelGradients (img : grayImage)
      : (Real.real Vector.vector Vector.vector
         * Real.real Vector.vector Vector.vector
         * Real.real Vector.vector Vector.vector) =
    let
      val v2 = toRealVec2 img
      val (h, w) = dims v2

      fun getReal (y : int, x : int) : Real.real =
        if y < 0 orelse y >= h orelse x < 0 orelse x >= w then 0.0
        else Vector.sub (Vector.sub (v2, y), x)

      fun compute (y : int, x : int) : (Real.real * Real.real * Real.real) =
        let
          val p00 = getReal (y-1, x-1)
          val p01 = getReal (y-1, x)
          val p02 = getReal (y-1, x+1)
          val p10 = getReal (y,   x-1)
          val p11 = getReal (y,   x)
          val p12 = getReal (y,   x+1)
          val p20 = getReal (y+1, x-1)
          val p21 = getReal (y+1, x)
          val p22 = getReal (y+1, x+1)

          (* Sobel Gx *)
          val gx =
            (~1.0) * p00 + 0.0 * p01 + 1.0 * p02 +
            (~2.0) * p10 + 0.0 * p11 + 2.0 * p12 +
            (~1.0) * p20 + 0.0 * p21 + 1.0 * p22

          (* Sobel Gy *)
          val gy =
             1.0 * p00 +  2.0 * p01 +  1.0 * p02 +
             0.0 * p10 +  0.0 * p11 +  0.0 * p12 +
            ~1.0 * p20 + ~2.0 * p21 + ~1.0 * p22

          val mag = Math.sqrt (gx * gx + gy * gy)
        in
          (gx, gy, mag)
        end

      fun buildRow (y : int, x : int,
                    accGx : Real.real list,
                    accGy : Real.real list,
                    accMag : Real.real list)
          : (Real.real list * Real.real list * Real.real list) =
        if x = w then (List.rev accGx, List.rev accGy, List.rev accMag)
        else
          let
            val (gx, gy, mag) = compute (y, x)
          in
            buildRow (y, x + 1,
                      gx :: accGx,
                      gy :: accGy,
                      mag :: accMag)
          end

      fun rows (y : int,
                accGxRows : Real.real list list,
                accGyRows : Real.real list list,
                accMagRows : Real.real list list)
          : (Real.real list list * Real.real list list * Real.real list list) =
        if y = h then
          (List.rev accGxRows, List.rev accGyRows, List.rev accMagRows)
        else
          let
            val (rowGx, rowGy, rowMag) = buildRow (y, 0, [], [], [])
          in
            rows (y + 1,
                  rowGx :: accGxRows,
                  rowGy :: accGyRows,
                  rowMag :: accMagRows)
          end

      val (gxRows, gyRows, magRows) = rows (0, [], [], [])

      fun v2ofRows rows =
        Vector.fromList (List.map (fn r => Vector.fromList r) rows)
    in
      (v2ofRows gxRows, v2ofRows gyRows, v2ofRows magRows)
    end

  (* ---------- Quantize gradient direction into 4 orientations ---------- *)

  (* Approximate zero for reals *)
  fun isZero (x : Real.real) : bool =
    Real.abs x < 1.0E~6

  (* atan2 implementation without real equality *)
  fun atan2 (y : Real.real, x : Real.real) : Real.real =
    if x > 0.0 then Math.atan (y / x)
    else if x < 0.0 andalso y >= 0.0 then Math.atan (y / x) + Math.pi
    else if x < 0.0 andalso y < 0.0 then Math.atan (y / x) - Math.pi
    else if isZero x andalso y > 0.0 then Math.pi / 2.0
    else if isZero x andalso y < 0.0 then ~(Math.pi / 2.0)
    else 0.0

  (* returns orientation bucket:
       0 = 0 degrees   (horizontal)
       1 = 45 degrees  (diag)
       2 = 90 degrees  (vertical)
       3 = 135 degrees (diag)
   *)
  fun quantizeDir (gx : Real.real, gy : Real.real) : int =
    let
      val angle = atan2 (gy, gx) * 180.0 / Math.pi   (* degrees *)
      val a =
        if angle < 0.0 then angle + 180.0 else angle (* in [0,180) *)
    in
      if      a < 22.5  orelse a >= 157.5 then 0  (* 0 deg   *)
      else if a < 67.5  then 1                     (* 45 deg  *)
      else if a < 112.5 then 2                     (* 90 deg  *)
      else 3                                       (* 135 deg *)
    end

  (* ---------- Non-maximum suppression ---------- *)

  fun nonMaxSuppression (gxV2, gyV2, magV2) :
      Real.real Vector.vector Vector.vector =
    let
      val (h, w) = dims magV2

      fun magAt (y,x) =
        if y < 0 orelse y >= h orelse x < 0 orelse x >= w then 0.0
        else Vector.sub (Vector.sub (magV2, y), x)

      fun dirAt (y,x) =
        let
          val gx = Vector.sub (Vector.sub (gxV2, y), x)
          val gy = Vector.sub (Vector.sub (gyV2, y), x)
        in
          quantizeDir (gx, gy)
        end

      fun nmsPixel (y : int, x : int) : Real.real =
        let
          val m = magAt (y, x)
          val d = dirAt (y, x)

          (* choose neighbors along gradient direction *)
          val (m1, m2) =
            (case d of
               0 => (magAt (y, x-1), magAt (y, x+1))          (* left/right *)
             | 1 => (magAt (y-1, x+1), magAt (y+1, x-1))      (* 45 diag *)
             | 2 => (magAt (y-1, x), magAt (y+1, x))          (* up/down *)
             | _ => (magAt (y-1, x-1), magAt (y+1, x+1)))     (* 135 diag *)
        in
          if m >= m1 andalso m >= m2 then m else 0.0
        end

      fun buildRow (y : int, x : int, acc : Real.real list) : Real.real list =
        if x = w then List.rev acc
        else buildRow (y, x + 1, nmsPixel (y, x) :: acc)

      fun buildAll (y : int, rows : Real.real list list)
          : Real.real list list =
        if y = h then List.rev rows
        else
          let
            val row = buildRow (y, 0, [])
          in
            buildAll (y + 1, row :: rows)
          end

      val rows = buildAll (0, [])

    in
      Vector.fromList (List.map (fn r => Vector.fromList r) rows)
    end

  (* ---------- Double threshold & hysteresis ---------- *)

  val highRatio = 0.2       (* strong threshold *)
  val lowRatio  = 0.1       (* weak threshold *)

  fun maxRealInVec2 v2 =
    let
      val (h, w) = dims v2

      fun loop (y : int, x : int, current : Real.real) : Real.real =
        if y = h then current
        else if x = w then loop (y + 1, 0, current)
        else
          let
            val v = Vector.sub (Vector.sub (v2, y), x)
            val newMax = if v > current then v else current
          in
            loop (y, x + 1, newMax)
          end
    in
      if h = 0 orelse w = 0 then 0.0
      else loop (0, 0, 0.0)
    end

  (* classify into: 0 = non-edge, 1 = weak, 2 = strong *)
  fun classifyEdges (magNMS : Real.real Vector.vector Vector.vector) =
    let
      val (h, w) = dims magNMS
      val maxVal = maxRealInVec2 magNMS
      val highThr = highRatio * maxVal
      val lowThr  = lowRatio  * maxVal

      fun classify (y : int, x : int) : int =
        let
          val m = Vector.sub (Vector.sub (magNMS, y), x)
        in
          if m >= highThr then 2
          else if m >= lowThr then 1
          else 0
        end

      fun rowLoop (y : int, x : int, acc : int list) : int list =
        if x = w then List.rev acc
        else rowLoop (y, x + 1, classify (y, x) :: acc)

      fun buildAll (y : int, rows : int list list) : int list list =
        if y = h then List.rev rows
        else
          let
            val row = rowLoop (y, 0, [])
          in
            buildAll (y + 1, row :: rows)
          end
    in
      Vector.fromList
        (List.map (fn r => Vector.fromList r) (buildAll (0, [])))
    end

  (* hysteresis: promote weak (1) if neighbor of strong (2), else drop *)
  fun hysteresis (labelsV2 : int Vector.vector Vector.vector)
      : grayImage =
    let
      val (h, w) = dims labelsV2

      fun getLab (y,x) =
        if y < 0 orelse y >= h orelse x < 0 orelse x >= w then 0
        else Vector.sub (Vector.sub (labelsV2, y), x)

      fun isStrongNeighbor (y,x) =
        let
          val coords =
            [(y-1,x-1),(y-1,x),(y-1,x+1),
             (y,  x-1),        (y,  x+1),
             (y+1,x-1),(y+1,x),(y+1,x+1)]

          fun existsStrong [] = false
            | existsStrong ((yy,xx)::rest) =
                if getLab (yy,xx) = 2 then true
                else existsStrong rest
        in
          existsStrong coords
        end

      fun finalVal (y : int, x : int) : int =
        let
          val lab = getLab (y,x)
        in
          if lab = 2 then 255
          else if lab = 1 andalso isStrongNeighbor (y,x) then 255
          else 0
        end

      fun rowLoop (y : int, x : int, acc : int list) : int list =
        if x = w then List.rev acc
        else rowLoop (y, x + 1, finalVal (y,x) :: acc)

      fun buildAll (y : int, rows : int list list) : int list list =
        if y = h then List.rev rows
        else
          let
            val row = rowLoop (y, 0, [])
          in
            buildAll (y + 1, row :: rows)
          end
    in
      buildAll (0, [])
    end

  (* ---------- Write final edge image ---------- *)

  fun writeGrayImage (filename : string, img : grayImage) : unit =
    let
      val out = TextIO.openOut filename
      val height = length img
      val width =
        (case img of
           [] => 0
         | row :: _ => length row)

      val _ = TextIO.output (out,
               Int.toString height ^ " " ^
               Int.toString width  ^ " 1\n")

      fun writeRow [] = TextIO.output (out, "\n")
        | writeRow [x] =
            (TextIO.output (out, Int.toString x);
             TextIO.output (out, "\n"))
        | writeRow (x :: xs) =
            (TextIO.output (out, Int.toString x ^ " ");
             writeRow xs)

      fun loop [] = ()
        | loop (r :: rs) =
            (writeRow r; loop rs)
    in
      loop img;
      TextIO.closeOut out
    end

  (* ---------- Main entry point ---------- *)

  val _ =
    let
      val inputFile  = "image_matrix.txt"
      val outputFile = "canny_edges.txt"

      val (h, w, c) = readHeader inputFile
      val _ =
        if c <> 1 then
          raise Fail "CannyEdge: expected grayscale input (C=1)"
        else ()

      val img        = readGrayImage inputFile
      val blurred    = gaussianBlur img
      val (gxV2, gyV2, magV2) = sobelGradients blurred
      val magNMS     = nonMaxSuppression (gxV2, gyV2, magV2)
      val labelsV2   = classifyEdges magNMS
      val finalEdges = hysteresis labelsV2
    in
      writeGrayImage (outputFile, finalEdges)
    end

end

