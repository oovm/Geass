(* ::Package:: *)

(* ::Subtitle:: *)
(*A package to compute sums of Kempner series*)


(* ::Subsection::Closed:: *)
(*Version information*)


(* ::Text:: *)
(*Mathematica Version: 4.0 + *)
(**)
(*Package Version: 2.2 -- 7/24/2008 *)
(**)
(*Authors: Robert Baillie and Thomas Schmelzer *)
(**)
(*Translator: GalAster - \:9171\:7d2b\:541b *)
(**)
(*Copyright: Non-Commercial *)


(* ::Subsection::Closed:: *)
(*Summary*)


(* ::Text:: *)
(*   This package computes sums of "Kempner" series.*)
(**)
(*   The harmonic series 1/1 + 1/2 + 1/3 + ... + 1/n + ... diverges.  This means*)
(*   that the sum can be made as large as desired by adding enough terms.*)
(**)
(*   Suppose we delete from this series all terms with a "9" in the denominator.*)
(*   That is, we remove 1/9, 1/19, 1/29, etc.  Then the remaining series converges*)
(*   to a sum less than 80.  This was first proved in 1914 by A. J. Kempner.*)
(**)
(*   We can also delete from the harmonic series all terms whose denominators*)
(*   contain:*)
(*     any digit 0, 1, ..., 9;*)
(*    or*)
(*     a list of one or more digits, such as the even digits {0, 2, 4, 6, 8};*)
(*    or*)
(*     any individual multi-digit string or number, such as "00", 42, or 314159;*)
(*    or*)
(*     any list of patterns of digits, such as { 3, "00", 42, 314159 }.*)
(*   In all such cases, the terms that remain form a convergent series.*)
(**)
(*   However, these series converge much too slowly to get even a rough*)
(*   estimate of the sum simply by adding up the terms.  For example,*)
(*    for the series with 9 removed, the sum of the first 10^27 terms*)
(*    still differs from the final sum (22.92067...) by about 1.*)
(**)
(*   This package computes the sums of these series to high precision.*)
(**)
(**)
(*Keywords: kempner, harmonic series*)
(**)
(*Sources:*)
(*   The original article that proves convergence of these series is:*)
(*      A. J. Kempner, "A Curious Convergent Series",*)
(*      American Mathematical Monthly, vol. 21, pages 48-50 (1914).*)
(**)
(*   The original algorithm that shows how to compute a few of these sums is in:*)
(*      Robert Baillie, "Sums of Reciprocals of Integers Missing a Given Digit",*)
(*      American Mathematical Monthly, vol. 86, pages 372-374 (May, 1979).*)
(**)
(*   The functions in this file implement the algorithm in the following article:*)
(*      Thomas Schmelzer and Robert Baillie, "Summing a Curious, Slowly Convergent*)
(*      Series", to appear in the American Mathematical Monthly, vol. 115,*)
(*      pages 525-540 (June/July 2008).*)
(*      The latest version of this Mathematica code may be downloaded from*)
(*        http://library.wolfram.com/infocenter/MathSource/7166/*)


(* ::Subsection::Closed:: *)
(*Test*)


BeginPackage[ "KempnerSums`" ];

(* :Examples:
   The main function in this package is KempnerSum.
   KempnerSum[X] computes the sum of 1/n where n has no digits in the set X.

   The output is rounded to the default number of decimal places.  This
   default is initially set to 15, but you can use kSumSetDefaultDecimals[m]
   to reset the default to m decimal places.
   Examples:
     KempnerSum[9] computes the sum of 1/n where n has no digit 9.
     This sum is about 22.920676619264150 .
   The second parameter is the number of decimal places:
     KempnerSum[9, 20] = 22.92067661926415034816 .

   KempnerSum can also take a third parameter (the base).
     KempnerSum[X, m, b] computes the sum of 1/n where n has no digits in X in base b.
     The sum is rounded and displayed to m decimal places.
     If the third parameter is omitted, the base is assumed to be 10.
     If you want to use the third parameter to specify a base other than 10,
     then you must include all three parameters:
       KempnerSum[0, 20, 2] = 1.60669515241529176378 .

   The first parameter, X, is the set of numbers that are to be omitted
   from the denominators.
   X can be:
     a single number: KempnerSum[0] or KempnerSum[314]
   or
     a list of numbers and/or strings enclosed in braces: KempnerSum[ { 1, "07", 32 } ].

   KempnerSum[{3, 1, 4}] computes the sum of 1/n where n contains no 3, no 1,
   and no 4 in base 10.  The sum is about 3.769948379731421 .

   KempnerSum[314] computes the sum of 1/n where n contains no 314 in base 10.
   The sum is about 2299.829782767518338 .

   If a missing digit pattern is an integer with more than one digit, and
   it begins with a 0, then you must enclose the number in quotes.  This
   is because Mathematica will interpret the input 09 as the integer 9,
   which gives a different result:
     KempnerSum[09]   =  22.920676619264150,
    but
     KempnerSum["09"] = 230.404757005814285 .

   KempnerSum[ { 0, 2, 4, 6, 8, 55, 13579 } ] computes the sum of 1/n where
   n has no even digit, no 55, and no 13579.  This example is in the
   Schmelzer-Baillie paper.  The sum is about 3.090849149653806 .

   KempnerSum[0, 30, 2] computes the sum of 1/n where n has no 0 in base 2.
   In order to specify the base as the third parameter, we cannot omit
   the second parameter (the number of decimal places).
   Note: this sum is equal to the sum of the rapidly-converging series
   1/1 + 1/3 + 1/7 + 1/15 + 1/31 + ... =
   Sum[1/(2^k-1), {k, 1, Infinity}] = 1.606695152415291763783301523191 ...,
   This example provides an independent check on the algorithm here.


   kSumFormatted is just like KempnerSum, except that kSumFormatted formats
   the output digits into groups of 5:
     kSumFormatted[9] = 22.92067 66192 64150 .
   (Technical notes: the output from kSumFormatted is not a number, but is
   instead an object of type "NumberForm".  Also, remember that Mathematica
   will interpret the input 22.92067 66192 as 22.92067 * 66192).


   Once you have computed a sum, you can use kPartialSum to compute
   partial sums of the same series.  kPartialSum uses the most recent
   sum computed by KempnerSum or kSumFormatted.
   For example, immediately after computing
     KempnerSum[9] = 22.920676619264150,
   you can compute the partial sum of the same series over all terms
   whose denominators have at most 30 digits (that is, all denominators
   in the series that are less than 10^30):
     kPartialSum[30] gives 21.971055078178619 .
   This shows that even if it were possible to use brute force to add all terms
   of the series up through 30 digits (more than 10^27 terms), that partial sum
   would still differ from the sum of the entire series by almost 1.0.

   Here is an example in base 2:
     KempnerSum[0, 20, 2] = 1.60669515241529176378 .
   Here is the partial sum over all denominators in the series that have
   at most 5 digits in base 2, that is, denominators at most (decimal) 31.
     kPartialSum[5] = 1.575115207373272
   This partial sum is just 1 + 1/3 + 1/7 + 1/15 + 1/31 = 1709/1085.

   You can specify a different number of decimal places in the second parameter.
   After computing KempnerSum[9]:
     kPartialSum[30, 20] = 21.97105507817861948711 .
   The next two calculations show that in order to make the partial sum exceed 22.9,
   you would need to include at least some terms with denominators of 67 digits:
     kPartialSum[66] = 22.899283165770095
     kPartialSum[67] = 22.901422511119500 .

   If we exclude denominators containing 314159, the Kempner sum is 2302582.33386... .
   At some point before this, the partial sum will first exceed 1000000.
   How far do we have to go in the series before this happens?
   kPartialSumThreshold gives us the answer.  kPartialSumThreshold gives us
   the (approximate) point at which a partial sum exceeds a given threshold, and
   shows how extraordinarily slowly these series converge to their final sums.  

   After computing KempnerSum[314159] = 2302582.333863782607892, we call
     kPartialSumThreshold[1000000].  The result is:
     {569679, 569680, 999999.793917693872470, 1.000001096506746783363*10^6}
     These four numbers tell us that the sum of the series through all
     denominators having 569679 digits is 999999.793917693872470, just under
     1000000, while the sum through denominators having 569680 digits is
     1.000001096506746783363*10^6 = 1000001.096506746783363, just over 1000000.
     In other words, to make the partial sum exceed 1000000, we would have to
     include all terms with denominators having 569679 digits, and some terms
     with denominators having 569680 digits.

     These results can be checked with kPartialSum:
     kPartialSum[569679] =  999999.793917693872470
     kPartialSum[569680] = 1000001.096506746783363

   The next calculation shows that, to make the partial sum exceed 2000000
   (that is, to approximate the final sum to just one significant digit), we
   need to include at least some terms whose denominators have 2029422 digits:
     kPartialSumThreshold[2000000] returns
     2029421, 2029422, 1.999999789743530856418*10^6, 2.000000092327587901494*10^6}

   The next calculation shows that, to make the partial sum reach the integer part
   of the sum of the entire series (that is, to reach 2302582), we must
   include at least some terms that have denominators of 15746478 digits:
   kPartialSumThreshold[2302582] returns
     {15746477, 15746478, 2.302581999999948711319*10^6, 2.302582000000282576822*10^6}

   Here is an example with an even larger sum.  If we exclude denominators
   that have the digit pattern "1234567890", the sum is
     KempnerSum[1234567890, 20] = 2.302585092202703549829506732687*10^10 .
   After including some terms with 434304 digits, the sum finally reaches 1 million:
     kPartialSumThreshold[1000000] returns
     {434303, 434304, 999998.47617794443909902633, 1.00000077866303886642136774*10^6}
   After including some terms with 444007324 digits, the sum reaches 1 billion:
     {444007323, 444007324,
      9.9999999815290203462491699527*10^8, 1.00000000035548712899465692756*10^9}

   The partial sum over all terms having at most one billion digits is
     kPartialSum[10^9] = 2.191199428160751825211987*10^9 ,
   which is still only about 1/10 the sum of the entire series.


   You can display the current default number of decimal places with
   kSumShowDefaultDecimals (no parameters).  You can change the default
   number of decimal places.  For example,
     kSumSetDefaultDecimals[20]
   will set the default number of decimal places to 20.  In any individual
   calculation, you can also specify how many decimal places you want.


   This loop computes the 10 sums that arise when terms with the digits
   0, 1, 2, ..., 9 are removed from the harmonic series:
     Do[Print[ i, " ", KempnerSum[i]], {i, 0, 9}]

   This computes sums involving all pairs of missing digits, base 10:
     For[i = 0, i <= 9, i++,
       For[j = i+1, j <= 9, j++,
         Print[ i, ",", j, ": ", KempnerSum[{ i, j }] ]
       ]
     ]

   The next loop prints out the sums for missing digit patterns 00 through 99.
   For "00", for example, we would delete 1/100, 1/200, ... from the harmonic
   series.  We need IntegerString[ ] for the input strings "00" - "09"
   because they have leading zeros.  (IntegerString also works with
   the the remaining digit patterns 10 - 99).  These sums range between
   approximately 220 and 253.
     Do[Print[IntegerString[i, 10, 2], " ",
       KempnerSum[IntegerString[i, 10, 2], 10]], {i, 0, 99}]



   The authors believe that all output digits for KempnerSum[X, m] are correct, rounded
   in the last place.  To verify that you got as many correct decimal places as
   you wanted, you can run the calculation again with, say, 5 more decimal places.
*)


(* ::Subsection:: *)
(*Main Package*)


(* usage messages for this context, and for the exported functions *)
KempnerSums::usage = "KempnerSums.m是一个用来计算 缺位调和级数(Kempner series)的程序包.\r\n
本程序包包含函数KempnerSum[X],kSumFormatted[X],kPartialSum[nDigits],kPartialSumThreshold[s],kSumGetA[X],kSumGetT[X]\r
以及选项kSumShowA,kSumTimeAndMemory,kSumSetDefaultDecimals,kSumShowDefaultDecimals.";

(* these are usage messages for individual functions *)

KempnerSum::usage = "KempnerSum[X] 对 1/n 求和但是去掉所有含有 X 的项.\r\n
 X 可以是个数值,如果X是个列表, 那么就依次去掉列表中的每一种类型的元素..\r
 (Note: 一个多位数如果含前导0必须写成字符型).\r
 第二个参数表示精度.\r\n
 Examples:
   KempnerSum[9] = 22.920676619264150 (使用默认的15位精度)\r
   KempnerSum[9, 10] = 22.9206766193 (使用10位精度)\r
   KempnerSum[09] = 22.920676619264150 (在Mathematica中09就是9)\r
   KempnerSum[\"09\"] = 230.404757005814285 (如果需要前导0,必须使用字符型)\r
   KempnerSum[{3, 1, 4}] = 3.769948379731421\r
   KempnerSum[314] = 2299.829782767518338\r
   KempnerSum[314159, 20] = 2302582.33386378260789202376\r
   KempnerSum[{0, 2, 4, 6, 8}] = 3.171765473415905 (不含偶数的所有项的和)\r
   KempnerSum[{1, 3, 5, 7, 9}] = 1.962608412994617 (不含奇数的所有项的和)\r
   KempnerSum[{\"00\", 11, 22, 33, 44, 55, 66, 77, 88, 99}] = 23.382957488301063 (去掉所有含连续数字的项后的和)\r
 KempnerSum 第三个参数表示进制,不做特别说明默认10进制.
 Warning:如果你想输入第三个参数,你必须先输入第二个参数,不得省略.
 Example: KempnerSum[0, 15, 2] = 1.606695152415292 (二进制中不含0的话剩余的项的和).\r
 Another form: KempnerSum[ T ], 以上计算本质上调用的是T矩阵,参见 Schmelzer-Baillie 的论文.";

kSumFormatted::usage = "和 KempnerSum 基本一样,但是自带5位的格式化效果.\r注意:结果是个NumberForm.";

kPartialSum::usage = "kPartialSum[ nDigits, nDecimals ]\r
 Examples:\r
   KempnerSum[9] = 22.920676619264150 .\r
   输入 kPartialSum[30] 返回 21.971055078178619 ,这对所有分母小于10^30进行了求和.\r
   另外 kPartialSum[31] 返回 22.066017232287173 ,这意味着为了使得和超过 22,我们需要对所有分母小于 10^31 的项求和.\r
   这一操作可以由函数'kPartialSumThreshold'来完成:\r
   kPartialSumThreshold[22] 直接返回了 {30, 31, 21.971055078178619, 22.066017232287173}.\r";

kPartialSumThreshold::usage = "kPartialSumThreshold 语法和 KempnerSum 大体差不多\r
 Example 1:
   KempnerSum[9] = 22.920676619264150 接着输入: \n
   kPartialSumThreshold[22] 返回结果为 {30, 31, 21.971055078178619, 22.066017232287173}.
   这意味着为了使和超过22,我们需要计算到某个十进制下31位的大数.\r\n
 Example 2:
   下面这个例子展示了对于浮点输入可能出现的错误.
   KempnerSum[9, 20] = 22.92067661926415034816.
   kPartialSumThreshold[22.920676619264149] returns
     {359, 360, 22.92067661926414950999, 22.92067661926414959380} .
   这个结果是错的: 注意到两个输出其实都大于你的输入.
   (原因在于Mathematica会微微的对你输入的浮点数进行一些舍入处理.)
   正确的写法应该是:kPartialSumThreshold[22.920676619264149``15] \r
   返回了正确的结果 {354, 355, 22.92067661926414892870, 22.92067661926414907065}.\r
   你也可以选择使用字符型 kPartialSumThreshold[\"22.920676619264149\"].";

kSumGetT::usage = "kSumGetT[X] 返回关于 X 的 T 矩阵.\r\n
 X is the collection of numbers to omit from the denominators.\
 This function takes an optional second parameter: the base.  不做特别说明默认10进制.";

kSumGetA::usage = "kSumGetA[X] 返回关于 X 的 A 矩阵.\r\n
 X is the collection of numbers to omit from the denominators.\
 This function takes an optional second parameter: the base.  不做特别说明默认10进制.";

kSumShowA::usage = "kSumShowA[ ] shows the A matrix for the previously-entered input string or list.";

kSumTimeAndMemory::usage = "kSumTimeAndMemory[nDigits] 估算 KempnerSum[9, nDigits] 需要的时间和内存,注意到 KempnerSum[99, nDigits] 需要更多的时间和内存 KempnerSum[9, nDigits].";

kSumSetDefaultDecimals::usage = "kSumSetDefaultDecimals[n] 控制默认的精度,没有设置过的话这个值是15.";
kSumShowDefaultDecimals::usage = "kSumShowDefaultDecimals 返回当前的默认精度.";


Begin["`Private`"];
(* begin the private context (implementation) *)


Off[ General::spell1 ];
Off[ General::spell ];

nDefaultDecimals = 15;

nf[y_, nDec_] :=
  If[y == 0, 0,
    NumberForm[y, Max[ 1, Floor[1+Log[10, Abs[y]]] ] + nDec, DigitBlock->5, NumberSeparator->{""," "}]
  ];

removeDups[inputList_?VectorQ] :=
Block[
  (* remove duplicates from a list without changing the order;
     (Union[ ] removes duplicates, but also sorts the list) *)
  { },
  elimReps[u_, v_] := If[MemberQ[u, v], u, Append[u, v]];
  Fold[elimReps, {First[inputList]}, Rest[inputList]]
];


(* Mathematica version 4 does not have a Norm[x, y] function.  so, to make this package
   work in version 4, we define a function that is equivalent to Norm[v, 1], which is:
     Norm[v, 1] = Abs[ v[[1]] ] + Abs[ v[[2]] ] + ... + Abs[ v[[n]] ] *)
normOne[v_?VectorQ] := Apply[Plus, Map[Abs, v] ];

(* power2 is just like Power[], but power2 returns 0^0 = 1 with no warning messages *)
power2[0, 0] := 1; power2[x_, y_] := Power[x, y] ;

(* speedMode = 1; *)    (* 0 for (slow, use less memory), 1 for (fast, lots of memory); 1 is the default *)



(* these global variables are computed in kSumX, and are used in kPartialSum *)
A = Null;
extraPol = 0;
n = 0;
Psi = Null;
iBaseSave = 0;
kSumSave = 0;
nDecimalsSave = 0;


Unprotect[KempnerSum, kSumFormatted, kPartialSum, kSumGetT, kSumGetA, kSumShowA];
Unprotect[kSumTimeAndMemory, kSumSetDefaultDecimals, kSumShowDefaultDecimals];
Unprotect[kPartialSumThreshold];

Clear[KempnerSum, kSumFormatted, kPartialSum, kSumGetT, kSumGetA, kSumShowA];
Clear[kSumTimeAndMemory, kSumSetDefaultDecimals, kSumShowDefaultDecimals];
Clear[setT, kSumX];

Clear[convertInputListToStrings];
convertInputListToStrings[inputList_?VectorQ] :=
Block[
  (* convert input list to all strings; if a list element is neither a string
     nor an integer, return an empty list. *)
  { len, i, str, outputList = { }, bSet },
  len = Length[inputList];
  For[i = 1, i <= len, i++,
    bSet = False;
    If[StringQ[ inputList[[i]] ], str = inputList[[i]]; bSet = True];
    If[IntegerQ[ inputList[[i]] ], str = ToString[ inputList[[i]] ]; bSet = True];
    If[bSet == False,
      Print["Error: Invalid input list: value #", i ," is neither a String nor an Integer."];
      Return[ {} ]
    ];
    AppendTo[outputList, str];
  ];
  outputList
];


setT[stringList_?VectorQ, iBase_:10] :=
Block[ (* enter with a list of strings {s1, s2, ...}, like { "12", "345" }; return the T matrix *)
  (* note: this function returns a 2-dimensional matrix, not a list.  even when the output
     matrix has only one row, it is a matrix of the form {{1,2,3. ...}}, not a vector of the
     form {1,2,3, ...}. *)
  (* programming note: this function was not written in mathematica's efficient "functional" style.
     this function does not need to be very efficient.  this function sets up the T matrix farily,
     quickly.  calculating the sum takes far longer.  readers are welcomed to improve this code.
  *)
  { nInputStrings, i, s, s2, len, validDigits, j, t, iRow, firstNChars, d, dTest, found, jStart,
    inputList0, inputList = {},    (* copy of input list *)
    nSingleDigits = 0, nRows = 0, firstRow, nSetStrings = 0, setStrings = {},
    firstJChars, bFound, k, dString, s2Appended, maxMatched, nCompare, iSaved, t2 },

  (* preliminaries: check input validity, make a list of input strings with spaces removed. *)

  If[( iBase < 2) || (IntegerQ[iBase] == False),
    Print["Error: Base = ", iBase," is not valid."];
    t = Table[-1, {1}, {1}];    (* return this invalid 1 by 1 matrix *)
    Return[t]
  ];

  inputList0 = removeDups[stringList];    (* remove duplicate strings, but keep the order unchanged *)

  nInputStrings = Length[inputList0];
  If[nInputStrings < 1,
    Print["Error: No input string specified."];
    t = Table[-1, {1}, {iBase}];    (* return this invalid 1 by iBase matrix *)
    Return[t]
  ];

  If[iBase <= 10,    (* make a list of all valid digits in this base *)
    validDigits = CharacterRange["0", ToString[iBase-1]]
  ];
  (* if iBase > 10, how should we check that the digits are valid?
     perhaps we could use lower-case letters a-z for bases 11-36,
     as Mathematica does for BaseForm[ ].  note: we can still enter
     a digit in any base, say 0 in base 100 or base 1000.  for example,
       KempnerSum[0, 10, 100] = 460.5252026385
     and
       KempnerSum[0, 10, 1000] = 6907.756101048 . *)

  For[i = 1, i <= nInputStrings, i++,
    (* make sure this list element is an integer or a string *)
    If[UnsameQ[ Head[inputList0[[i]]] , String ] && UnsameQ[ Head[inputList0[[i]]] , Integer ],
      Print["Error: Invalid input list: value[",i,"] is not a String."];
      If[Head[inputList0[[i]]] === Symbol, Print["Two commas in a row."] ];
      t = Table[-1, {1}, {iBase}];    (* return this invalid (1 by iBase) matrix *)
      Return[t]
    ];

    s = inputList0[[i]];

    If[Head[s] === Integer, s = ToString[s]];
    s2 = StringReplace[s, " " -> ""];    (* for convenience, user can include spaces, but we ignore them *)
    len = StringLength[s2];
    If[len == 0, Continue[]];

    If[iBase <= 10,  (* verify that this input string is a valid number in this base *)
      For[j = 1, j <= len, j++,
        If[MemberQ[validDigits, StringTake[s2, {j}]] == False,  (* jth character *)
          Print["Error: input character '", StringTake[s2, {j}], "' is not a valid digit in base ", iBase];
          t = Table[-1, {1}, {iBase}];    (* return this invalid (1 by iBase) matrix *)
          Return[t]
        ];
      ];
    ];
    (* this string is ok, so add it to inputList *)
    AppendTo[inputList, s2];
    If[len == 1, nSingleDigits++];
  ];    (* end For[ i ] loop *)

  (* if only single digits are present, return this one-row matrix *)
  If[nSingleDigits == nInputStrings,
    (* the first row has zeros in the positions corresponding to
       input strings of length 1.  other positions have non-zero entries. *)
    firstRow = Table[1, {1}, {iBase}];
    For[i = 1, i <= nInputStrings, i++,
      If[StringLength[ inputList[[i]] ] == 1,
        j = ToExpression[ inputList[[i]] ];
        firstRow[[1, j+1]] = 0;
      ];
    ];
    Return[firstRow]
  ];

  (* now we set up the unique strings that define all the sets.
     see the schmelzer-baillie paper for details.

     example 1: suppose the input string is { "314" }.
     let S be the set of all integers not containing this string.
     then we make 3 subsets of S:
     set 2 is all elements of S that end in "3" ;
     set 3 is all elements of S that end in "31" ;
     set 1 is all other elements of S.

     example 2: suppose the input list is { "12", "345", "3467" }.
     let S be the set of all integers containing none of these strings.
     then we make 4 subsets of S:
     set 2 is all elements of S that end in "1" ;
     set 3 is all elements of S that end in "3" ;
     set 4 is all elements of S that end in "34" ;
     set 5 is all elements of S that end in "346" ;
     set 1 is all other elements of S.

     example 3: complicated example from the schmelzer-baillie paper:
     suppose the input list is { "0", "2", "4", "6", "8", "55", "13579" }.
     then we make 6 subsets of S:
     set 2 is all elements of S that end in "5" but not "135" ;
     set 3 is all elements of S that end in "1" ;
     set 4 is all elements of S that end in "13" ;
     set 5 is all elements of S that end in "135" ;
     set 6 is all elements of S that end in "1357" ;
     set 1 is all other elements of S.
  *)

  setStrings = { "" }; nSetStrings = 1;    (* for first set (first row) *)

  For[i = 1, i <= nInputStrings, i++,    (* process the ith input string *)
    s2 = inputList[[i]];
    len = StringLength[s2];
    If[len > 1,    (* input strings of length 1 are processed elsewhere *)
      For[j = 1, j <= len-1, j++,
        firstJChars = StringTake[s2, j];

        (* make sure that string firstJChars is not already in the setStrings list *)
        bFound = False;
        For[k = 1, k <= nSetStrings, k++,
          If[setStrings[[k]] == firstJChars, bFound = True; Break[] ];
        ];

        If[bFound == False,
          AppendTo[setStrings, firstJChars];
          nSetStrings++;
        ];
      ];    (* end For[ j ] loop *)
    ];    (* end If[len > ... 1] *)
  ];    (* end For[ i ] loop *)

  (* at this point, we have setStrings = {1, 3, 34, 346}.
     the number of rows will be the number of elements in setStrings. *)

  nRows = Length[setStrings];
  (* now we know the dimensions of the matrix *)
  t =  Table[1, {nRows}, {iBase}];

  (* now set the elements of the matrix *)

  For[iRow = 1, iRow <= nRows, iRow++,
    s2 = setStrings[[iRow]];
    (* append each digit to string s2 and see which set the appended string belongs to *)
    For[d = 0, d < iBase, d++,
      (* create a string (s2Appended) that consists of the string
         for this set, with the single digit d appended. *)
      dString = ToString[d];
      s2Appended = StringJoin[s2, dString];
      (* if d is a prohibited digit, set t[..] = 0 *)
      If[ Length[Position[inputList, dString]] > 0,
        t[[iRow, d+1]] = 0;
        Continue[];    (* next d *)
      ];
      (* if s2Appended equals any of the prohibited input strings, then set t[..] = 0 *)
      bFound = False;

      For[i = 1, i <= nInputStrings, i++,
        If[ s2Appended == inputList[[i]], t[[iRow, d+1]] = 0;
          bFound = True; Break[] ];  (* end If *)
      ];  (* end For[ i ] loop *)

      If[bFound == True, Continue[] ];

      (* if we get here, then s2Appended is not a prohibited input string.
         see if s2Appended contains a prohibited string. *)
      For[i = 1, i <= nInputStrings, i++,
        If[ Length[StringPosition[ s2Appended, inputList[[i]] ] ] > 0,
           t[[iRow, d+1]] = 0;
           bFound = True; Break[];
        ];
      ];

      If[bFound == True, Continue[] ];

      (* if we get here, then this (row, column) was not set to 0.
         see which set s2Appended belongs in.  an example in the schmelzer-baillie
         paper shows that a string ending in "5" could be in two sets ("5" and "135").
         so, first, try to select the set (if any) whose string equals s2Appended.
      *)

      (* final digits of s2Appended could match more than one setStrings[[i]].
         among the matches, save the value of i for which
         StringLength[ setStrings[[i]] ] is largest.
      *)
      maxMatched = 0;    (* max number of final digits that match *)
      iSaved = 0;
      For[i = 2, i <= nSetStrings, i++,
        nCompare = StringLength[ setStrings[[i]] ];
        (* if s2Appended is shorter than setStrings[[i]], then it cannot match setStrings[[i]] *)
        If[StringLength[s2Appended] < nCompare, Continue[] ];
        (* compare the last (nCompare) characters of s2Appended and setStrings[[i]] *)
        If[ StringTake[s2Appended, -nCompare] == StringTake[setStrings[[i]], -nCompare],
          bFound = True;
          If[StringLength[ setStrings[[i]] ] > maxMatched,
            maxMatched = StringLength[ setStrings[[i]] ];
            iSaved = i;
          ];
        ];
      ];  (* end For[ i ] loop *)

      If[bFound, t[[iRow, d+1]] = iSaved];

    ];  (* end For[ d ] loop *)
  ];    (* end For[ iRow ] loop *)

  t    (* return this matrix *)
];    (* end of setT *)


kSumX[stringList_?VectorQ, T_?MatrixQ, nDecimals0_Integer, iBase_] :=
Block[
  (* the user calls KempnerSum[ ] or kSumFormatted[ ].  those functions call this
     private function kSumX, which does all the work. *)
  { nDecimals, inputLen, nDecSum, f, c, a, S, z, nInputDigits, totalDigits,
    iMaxBase, goalMult, matB, wTrunc2, i, d, l, m, w, h,
    iPrint = 0,
    xi },

  Clear[A, Psi, extraPol, n, iMaxBase];    (* clear the global variables *)

  (* in base 10, if the input is one string of N digits, then the sum will have N+1 digits.
     if there are additional strings, they can only lower the sum.  therefore, the shortest
     string in the list provides an upper bound for the number of digits in the sum. *)
  (* inputLen = Min[StringLength[stringList]]; *)
  (* StringLength[ ] is not Listable in version 4; next line works in versions 4, 5, and 6 *)
  inputLen = Min[Map[StringLength, stringList]];

  (* iMaxBase and goalMult depend on the base.  their values were determined empirically.
     to insure that we get as many decimal places of precision as the user requested. *)

  (* iMaxBase is roughly the log of 1000 to base (iBase). so, for base 10, iMaxBase = 3.
     for base 2, iMaxBase = 10.  for base 100 if we use "Apply[Plus, N[1/S[i, j]^k, nDecSum]]",
     then iMaxBase = 3 uses too much memory.
  *)
  nDecimals = nDecimals0;
  If[nDecimals < 1, nDecimals = 1];

  iMaxBase = Ceiling[ Log[iBase, 1000] ];

  (* for base 1000, KempnerSum[0, 20, 1000] = 6907.75610104793192687449 .
     for this, we need iMaxBase > 1 *)
  If[iMaxBase < 2, iMaxBase = 2];    (* could happen if iBase = 1000 *)

  iBaseSave = iBase;    (* used in kPartialSum *)

  (* goalMult = 1.1 is ok for base 100, but is too small for base 10.
     1.4 is usually enough for base 10.  an exception: 100th decimal place of kSumFormatted["99", 100]
     is different from that of kSumFormatted["99", 110].  so, use 1.5 for base 10.
     for base 2, we need an even larger multiplier: 3.1 is too small, but 3.2 works.
     for bases between 2 and 10, a goalMult < 3.2 might work, but we will use 3.2.
  *)
  goalMult = 1.5;
  If[iBase < 10,   goalMult = 3.2 ];
  If[iBase >= 100, goalMult = 1.1 ];

  (* for base 10, the number of correct decimal places turns out to be about (2/3) * nDecSum.
     therefore, adjust nDecSum accordingly, based on the input (nDecimals).
     also, an N-digit string gives rise to a sum with N+1 digits to the left of the
     decimal point.  for base 100, the sum has 2N+1 digits to the left of the decimal point.
  *)

  nInputDigits = inputLen;
  totalDigits = nDecimals + nInputDigits;    (* works in base 10 *)
  If[iBase > 10, totalDigits = nDecimals + 2*nInputDigits ];

  nDecSum = Floor[goalMult * totalDigits];

  (* for large bases (say 1000), we may have to slightly increase the number of digits
     to prevent this: KempnerSum[0, 10, 1000] = 6907.756101048 (only 9 decimals shown). *)
  If[iBase > 10,    
    nDecSum = nDecSum + Ceiling[Log[10, iBase]*nInputDigits]
  ];

  (* the computation of A here is the same code as kSumGetA. *)
  f[j_, l_, m_] := f[j, l, m] = If[T[[l, m]] == j, 1, 0] ;

  n = Dimensions[T][[1]] ;    (* number of rows *)

  A = (1/iBase) Sum[ Table[f[j, l, m + 1], {j, n}, {l, n}] , {m, 0, iBase-1} ] ;

  c[k_, w_] := c[k, w] = Binomial[k + w - 1, w] ;

  a[k_, w_, m_] := iBase^(-k - w) * c[k, w] * (-1)^w * power2[m, w] ;

  matB[a_?MatrixQ] := (Inverse[# - a] - #) &[IdentityMatrix[Length[a]]];

  wTrunc2[i_, k_] := Max[Floor[(nDecSum + 1)/(i - 1) + 1 - k] + 1, 0] ; (* function of i, k, and nDecSum *)

  extraPol = nDecSum + 1;

(*
  If[iPrint > 0,
    Print["nDecimals = ", nDecimals, ", iMaxBase = ", iMaxBase, ", goalMult = ", goalMult];
    Print["nDecSum = ", nDecSum];
  ];
*)

  S[i_, j_] := S[i, j] =
  If[i == 1, Complement[Extract[Table[d, {d, 0, iBase-1}], Position[T[[1]], j]], {0}] ,
    Block[{ p, el, m, k, elel, mm },
           p = Position[Table[T[[el, m]], {el, n}, {m, iBase}], j]; h = {};
           For[k = 1, k <= Length[p], k++,
             {elel, mm} = p[[k]]; h = Join[h, iBase * S[i - 1, elel] + (mm - 1)]
           ]
          ];    (* end Block *)
(* If[iPrint == 1, Print["h[", i, ",", j,"]=", h] ]; *)
    h    (* return this list of i-digit integers = S[i,j] *)
  ] ;    (* end If *)


(* here are two ways to compute Psi:
     N[Apply[Plus, 1/S[i, j]^k], nDecSum]    this is slow but uses little memory
   or
     Apply[Plus, N[1/S[i, j]^k, nDecSum]]    this is fast but uses lots of memory

   lst1 = Range[11, 99]
   lst1 = Select[Range[11, 99], Mod[#, 10] != 0 &]  (* for KempnerSum[0], remove 20, 30, ..., 90 *)
   there is a difference in how these two expressions are evaluated:
     N[Apply[Plus, 1/lst1^k], nDecSum]
     Apply[Plus, N[1/lst1^k, nDecSum]]
     f1[k_, nDec_] := N[Apply[Plus, 1/lst1^k], nDec] - Apply[Plus, N[1/lst1^k, nDec]]
     f2[nDec_] := { N[Apply[Plus, 1/lst1], nDec] , Apply[Plus, N[1/lst1, nDec]] } // N
*)

  Psi[i_, j_, k_] := Psi[i, j, k] =
    If[i <= iMaxBase,

   (* If[speedMode == 0,
        N[Apply[Plus, 1/S[i, j]^k], nDecSum],  (* slow, but uses little memory; N[long rational] *)
        (* Apply[Plus, N[1/S[i, j]^k, nDecSum]] *)   (* fast, but uses lots of memory; sum of decimals *)
        (* compute only one 1/S[i, j]^k at a time to save memory; 1/21/2008 *)
        (* xSum = 0; For[xi = 1, xi <= Length[S[i, j]], xi++, xSum += N[1/(S[i, j][[xi]])^k, nDecSum]]; xSum *)
        Sum[ N[1 / (S[i, j][[xi]])^k, nDecSum], { xi, Length[S[i, j]] } ]    (* 1/24/2008 *)
      ] *) (* end If[speedMode] *)

      Sum[ N[1 / (S[i, j][[xi]])^k, nDecSum], { xi, Length[S[i, j]] } ]
     ,
      (* below, i > iMaxBase *)
      Sum[f[j, l, m + 1]*Sum[a[k, w, m]*Psi[i - 1, l, k + w],
         {w, 0, wTrunc2[i, k]}], {m, 0, iBase-1}, {l, 1, n}]
    ] ;    (* end If *)

(*
If[iPrint == -1,
  Print["kSumx: extraPol = ", extraPol, ", n = ", n];
  (* print the Psi values that will be used in computing z, below.  the 3rd argument
     is always 1 there, but values of Psi[i-1, j, k] where k > 1 were used above
     to compute Psi[i, j, k]. *)
  Print["  (1) Psi (1..", extraPol, ", 1..", n, ", 1) = ", N[Table[Psi[i, j, 1], {i, 1, extraPol}, {j, 1, n}], 5] // TableForm ];
  Print["  (2) Psi (", extraPol, ", 1..", n, ", 1) = ", N[Table[Psi[extraPol, j, 1], {j, n}], 5] // TableForm ];
];
*)

  (* use normOne[v] instead of Norm[v, 1] so this can run with Mathematica 4.0 *)
  z = Sum[Psi[i, j, 1], {i, 1, extraPol}, {j, 1, n}] +
    normOne[matB[A] . Table[Psi[extraPol, j, 1], {j, n}] ];

  (* return z, rounded to the number of decimal places requested *)
  If[z == 0, kSumSave = 0, kSumSave = N[z, Floor[ 1 + Log[10, z] ] + nDecimals ] ];

  nDecimalsSave = nDecimals;

(* If[iPrint == 1, Print["z = ", z] ]; *)

  kSumSave    (* kSumSave is used in kPartialSum *)

];    (* end of kSumX *)


KempnerSum[T_?MatrixQ, nDecimals_Integer] :=
Block[ (* in this version, we input a T matrix, not a string *)
  (* note: the base is not a parameter; the base is the number of columns in the matrix *)
  (* warning - a matrix is a type of list.  therefore, this KempnerSum[matrix] function must be
     placed before KempnerSum[list] in this source code file. *)
  { n, iBase, i, stringList = { }, s = ""},

  If[nDecimals < 1, nDecimals = 1];
  n = Dimensions[T][[1]];    (* number of rows *)
  If[Length[ Dimensions[T] ] == 1,
    iBase = Length[T],          (* T is a 1 by n matrix *)
    iBase = Dimensions[T][[2]]; (* iBase is the number of columns in the T matrix *)
  ];

  (* make a string of length n.  it doesn't matter what its digits are; only the length matters. *)
  For[i = 1, i <= n, i++, s = StringJoin[s, "0"] ];

  AppendTo[stringList, s];
  kSumX[stringList, T, nDecimals, iBase]
];    (* end of KempnerSum[T matrix, nDec] *)


KempnerSum[T_?MatrixQ] :=
Block[ (* in this version, we input a T matrix, not a string *)
  (* note: the base is not a parameter; the base is the number of columns in the matrix *)
  { },

  KempnerSum[T, nDefaultDecimals]
];    (* end of KempnerSum[T matrix] *)


KempnerSum[inputList_?VectorQ, nDecimals_Integer, iBase_:10] :=
Block[
  { lst, T },
  lst = convertInputListToStrings[inputList];
  If[Length[lst] == 0, Return[0] ];
  T = setT[lst, iBase];
  If[ T[[1, 1]] < 0, Return[0] ];
  kSumX[lst, T, nDecimals, iBase]
] ;   (* end of KempnerSum[list, nDec, base] *)


KempnerSum[inputList_?VectorQ] :=
Block[
  { },
  KempnerSum[inputList, nDefaultDecimals]
];    (* end of KempnerSum[list] *)


KempnerSum[s_String, nDecimals_Integer, iBase_:10] :=
Block[
  { stringList = { s } },
  KempnerSum[stringList, nDecimals, iBase]
] ;   (* end of KempnerSum[string, nDec, base] *)


KempnerSum[s_String] :=
Block[
  { stringList = { s } },
  KempnerSum[stringList, nDefaultDecimals]
];    (* end of KempnerSum[string] *)


KempnerSum[i_Integer, nDecimals_Integer, iBase_:10] :=
Block[
  { stringList = { ToString[i] } },
  KempnerSum[stringList, nDecimals, iBase]
];    (* end of KempnerSum[integer, nDec, base] *)


KempnerSum[i_Integer] :=
Block[
  { stringList = { ToString[i] } },
  KempnerSum[stringList, nDefaultDecimals]
];    (* end of KempnerSum[integer] *)



kSumFormatted[T_?MatrixQ, nDecimals_Integer, iBase_:10] :=
Block[ (* in this version, we input a T matrix *)
  (* returns a formatted value of type NumberForm, not a number *)
  { z = KempnerSum[T, nDecimals] },
  nf[z, nDecimals]
];    (* end of kSumFormatted[matrix, nDec, base] *)

kSumFormatted[T_?MatrixQ] :=
Block[ (* in this version, we input a T matrix *)
  (* returns a formatted value of type NumberForm, not a number *)
  { nDecimals = nDefaultDecimals, z },
  z = KempnerSum[T, nDecimals];
  nf[z, nDecimals]
];    (* end of kSumFormatted[matrix] *)


kSumFormatted[inputList_?VectorQ, nDecimals_Integer, iBase_:10] :=
Block[ (* returns a formatted value of type NumberForm, not a number *)
  { z = KempnerSum[inputList, nDecimals, iBase] },
  nf[z, nDecimals]
] ;   (* end of kSumFormatted[list, nDec, base] *)


kSumFormatted[inputList_?VectorQ] :=
Block[ (* returns a formatted value of type NumberForm, not a number *)
  { nDecimals = nDefaultDecimals, z },
  z = KempnerSum[inputList, nDecimals];
  nf[z, nDecimals]
]  ;  (* end of kSumFormatted[list] *)


kSumFormatted[s_String, nDecimals_Integer, iBase_:10] :=
Block[ (* returns a formatted value of type NumberForm, not a number *)
  { stringList = { s } },
  kSumFormatted[stringList, nDecimals, iBase]
] ;   (* end of kSumFormatted[string, nDec, base] *)


kSumFormatted[s_String] :=
Block[ (* returns a formatted value of type NumberForm, not a number *)
  { stringList = { s } },
  kSumFormatted[stringList, nDefaultDecimals]
]  ;  (* end of kSumFormatted[string] *)


kSumFormatted[i_Integer, nDecimals_Integer, iBase_:10] :=
Block[ (* returns a formatted value of type NumberForm, not a number *)
  { stringList = { ToString[i] } },
  kSumFormatted[stringList, nDecimals, iBase]
]  ;  (* end of kSumFormatted[integer, nDec, base] *)


kSumFormatted[i_Integer] :=
Block[ (* returns a formatted value of type NumberForm, not a number *)
  { stringList = { ToString[i] } },
  kSumFormatted[stringList, nDefaultDecimals]
];    (* end of kSumFormatted[integer] *)


kPartialSum[nDigits_Integer?Positive, nDecimals_Integer] :=
Block[ (* partial sum calculation, using previous KempnerSum *)
  (* using the same input just passed to KempnerSum[ ], compute the partial sum of 1/k,
     where k has <= nDigits digits (that is, denominators are < 10^nDigits).
     warning: this could underflow if nDigits > about 2*10^9.
     first, call KempnerSum[string, decimals].  then, we can call kPartialSum[digits, decimals].
     this function uses the existing values of global variables A, extraPol, n, Psi,
     iBaseSave, and kSumSave, all of which were set in the last call to KempnerSum.
     note: if this returns less than the requested number of decimal places, then you must
     call KempnerSum again with the desired number of places.
  *)
  { partSum = 0, i, j, matBTrunc },

(* formal definition, but this is slow
  matBTrunc[a_?MatrixQ, M_] :=
    ((# - MatrixPower[a, M+1]).Inverse[# - a] - #) &[ IdentityMatrix[Length[a]] ];
*)

  matBTrunc[a_?MatrixQ, M_] :=
  Block[
    (* instead of computing the exact value of a^(M+1), we first convert "a"
       to floating-point, then exponentiate.  if enough decimal places are used,
       this will produce an accurate value of a^(M+1).
    *)
    { nDec, ndigSum, (* a2, *) a3 },

    If[nDecimals < 1, nDecimals = 1];
    ndigSum = 1 + Floor[Log[10, kSumSave]];
    (* nDec = nDecimals + ndigSum + IntegerLength[M+1] + 5; *)
    nDec = nDecimals + ndigSum + (1+Floor[Log[10, M+1]]) + 5;  (* for mma version 5.2 *)

    (* a3 = MatrixPower[a, M+1] ; *)    (* very slow *)
    (* a2 = iBaseSave * a; a3 = N[MatrixPower[a2, M+1], nDec] / iBaseSave^(M+1); *)
    (* a2 = iBaseSave * a; a3 = MatrixPower[N[a2, nDec], M+1] / iBaseSave^(M+1); *)

    a3 = MatrixPower[N[a, nDec], M+1] ;  (* fast *)

    ((# - a3).Inverse[# - a] - #) &[ IdentityMatrix[Length[a]] ]
  ];    (* end of matBTrunc *)


  If[ (extraPol <= 0) || (kSumSave == 0),    (* must call KempnerSum[ ] or kSumFormatted[ ] first *)
    Print["Error: no sum has been computed yet.  Call KempnerSum[ ] or kSumFormatted[ ] first."];
    Return[0]
  ];

  If[nDigits > extraPol,
     (* Check[ ] forces partSum to be 0 if an overflow or underflow occurs *)
     partSum = Check[ Sum[Psi[i, j, 1], {i, 1, extraPol}, {j, 1, n}] +
       normOne[matBTrunc[A, nDigits - extraPol] . Table[Psi[extraPol, j, 1], {j, n}] ] ,
           (* 0, { General::unfl , General::ovfl } ]; *)
           0, General::unfl ];    (* check only for underflow for mma version 5.2 *)
   ,
     (* else: nDigits <= extraPol; just sum up to i = nDigits *)
     partSum = Sum[Psi[i, j, 1], {i, 1, nDigits}, {j, 1, n}]
  ];  (* end If *)

  If[partSum == 0, 0, N[partSum, Floor[ 1 + Log[10, partSum] ] + nDecimals ] ]

] ;   (* end of kPartialSum[nDigits, nDec] *)


kPartialSum[nDigits_Integer?Positive] :=
Block[ (* partial sum calculation, based on previous KempnerSum *)
(* using the previous input to KempnerSum[ ], compute the
   partial sum of 1/k, where k has <= nDigits digits. *)
  { },
  kPartialSum[nDigits, nDefaultDecimals]
];    (* end of kPartialSum[nDigits] *)


Clear[kPartialSumThreshold];
Clear[kPartialSumThresholdX];
kPartialSumThresholdX[pSum0_?NumericQ, nDecDefault_] :=
Block[
  (* this private function is called by public functions kPartialSumThreshold.

     using the previous input to KempnerSum[ ], compute the number of digits
     at which the partial sum exceeds pSum0.
     example:
       KempnerSum[9] = 22.920676619264150.
       kPartialSumThreshold[22] tells approximately how far we must go in the series
       to make the partial sum exceed 22.
       kPartialSumThreshold[22] returns {30, 31, 21.971055078178619, 22.066017232287173}.
       this means that if we add all terms in the series including all those having
       30 digits (that is, denominators < 10^30), the sum will be 21.971055078178619,
       and if we also include terms with denominators having 31 digits, the sum will
       be 22.066017232287173.  therefore, we would need at least some 31-digit
       denominators in order to make the sum exceed 22.

     in general, the input value (22, in this example) will lie  between the partial sums
     that are returned:
       21.971055078178619 < 22 < 22.066017232287173.

     two warnings about using this function:
     1.  if pSum is very close to the actual sum returned by KempnerSum[ ], you may be asked to
     call KempnerSum[ ] again, with more decimals, then to call kPartialSumThreshold[ ] again.

     2.  also, if you specify pSum as a decimal value, mathematica may slightly change
     your input before kPartialSumThreshold can process it.  therefore, if pSum is close
     to the actual sum, you could get incorrect results.

     here is an example showing how this can heppen.  suppose you compute
       KempnerSum[314, 20] = 2299.82978276751833845359.
     then, if you enter
       kPartialSumThreshold[2299.8297827675],
     this function will print out:
       Calculate where the partial sum exceeds 2299.8297827675000917225
       {32386, 32387, 2299.82978276750008978870, 2299.82978276750010807399}
     notice that mathematica has changed your input to 2299.829782767500092.
     then, because kPartialSumThreshold uses this changed input, the output you
     got is not the correct output for the input you specified: notice that
     your input value is not between the two partial sums shown.

     there are three ways to prevent mathematica from changing your input.
     1. you can use mathematica's backquote notation to specify the accuracy of
        your input.  so, you could enter:
          kPartialSumThreshold[2299.8297827675``10]
        this time, you get the correct output:
          Calculate where the partial sum exceeds 2299.8297827675000000000
          {32381, 32382, 2299.82978276749999808677, 2299.82978276750001646395}
     2. enter your input using quotes:
          kPartialSumThreshold["2299.8297827675"]
     3. enter an exact rational number:
         kPartialSumThreshold[2299 + 8297827675/10000000000].
     these three methods all produce the correct output for the input you specified.

     the bottom line is this: if you specify a decimal value for the partial sum,
     it is a good idea to use the backslash notation, such as 2299.8297827675``10.
  *)

  { i, s, psa, psb, psc, a, b, c, nDec, diff, pSum, iLimit = 30,
    errorReturn = { -1, -1, -1, -1 } },

  If[ (extraPol <= 0) || (kSumSave == 0),
    Print["Error: no sum has been computed yet.  Call KempnerSum[ ] or kSumFormatted[ ] first."];
    Return[ errorReturn ]
  ];

  pSum = pSum0;

  If[pSum <= 0, Return[ errorReturn ] ];    (* error *)
  nDec = nDecDefault;

  If[nDec < nDecimalsSave, nDec = nDecimalsSave];
  If[Accuracy[pSum] < nDec, pSum = SetAccuracy[pSum, nDec] ];

  Print["Calculate where the partial sum exceeds ", pSum];

  s = kSumSave;

  If[pSum > s,
    Print["Value must be less than the actual sum (", s, ")."];
    Return[ errorReturn ]
  ];    (* error *)


  If[pSum >= s,
    Print[pSum, " is too close to the actual sum (", s, ")."];
    Print["Either enter a smaller value, or call KempnerSum again with more than ", nDecimalsSave, " decimal places."];
    Return[ errorReturn ]
  ];    (* error *)

  a = 1;
  psa = kPartialSum[a, nDec] ;
  If[pSum < psa,
    Return[ { 0, 1, 0.0, psa }];
  ];
  b = a;
  For[i = 1, i <= iLimit, i++,
    psb = kPartialSum[b, nDec] ;
    If[psb > pSum, Break[ ] ];
    a = b; psa = psb;
    b = b * 2;
  ];

  (* get here with b such that partial sum > pSum *)
  If[psb == pSum,
    Print[pSum, " is too close to the actual sum."];
    Print["Either use a smaller value, or increase the number of decimals (2nd parameter)."];
    Return[ errorReturn ]    (* error *)
  ];
  If[psb < pSum, Return[ errorReturn ] ];    (* error *)

  (* enter the loop below with psa < pSum and psb > pSum.
     compute c = (a + b)/2, then compute psc. *)

  diff = b - a;
  c = a + diff/2;

  For[i = 1, i <= iLimit, i++,
    psc = kPartialSum[c, nDec] ;
    If[psc > pSum,
      b = c; psb = psc;
     ,
      a = c; psa = psc;
    ];
    diff = b - a;
    If[diff < 2, Break[] ];
    c = a + diff/2;

  ];    (* end loop *)

  If[ (pSum <= psa) || (psb <= pSum),
    nDecimalsNeeded = -Floor[Log[10, Abs[psb - psa]]] + 5;
    nDecimalsNeeded = 5*(1 + Floor[(nDecimalsNeeded-1)/5]);  (* round up to mult of 5 *)
    If[nDecimalsNeeded > nDec,
      Print["More decimals are needed.  You should re-compute KempnerSum to at least ",
            nDecimalsNeeded, " decimals, then call kPartialSumThreshold again."] ,
      Print["More precision is needed.  You should re-compute KempnerSum to more decimals,\
 then call kPartialSumThreshold again."];
    ];
    Return[ errorReturn ];
  ];

  Return[ { a, b, psa, psb } ];
];    (* end of kPartialSumThresholdX *)


kPartialSumThreshold[pSumStr_String, nDecDefault_] :=
Block[
  (* a string was entered.  add double backquotes to specify the accuracy,
     then call the private function kPartialSumThresholdX. *)
  { pSum, inputStr2, decPtList, quoteList, nDecimalsInput, nDec2,
    errorReturn = { -1, -1, -1, -1 }
  },

  decPtList = StringPosition[pSumStr, "."];    (* get list of starting, ending positions *)
  quoteList = StringPosition[pSumStr, "`"];

  If[ (Length[decPtList] == 1) && (Length[quoteList] == 0),
    (* input has one decimal point and no backquotes (usual case); number of
       decimals = number of chars between decimal point and end of string.
       append backquotes and a number to specify the accuracy of the input,
       then convert the new string to a floating-point number with that accuracy.
    *)
    nDecimalsInput = StringLength[pSumStr] - decPtList[[1]][[1]] ;
    nDec2 = Max[nDecimalsInput + 2, nDecDefault];
    inputStr2 = pSumStr <> "``" <> ToString[nDec2] ;
    pSum = ToExpression[inputStr2];
   ,
    (* otherwise (backquotes already in use, or input is an integer):
       just convert the input string to a number *)
    pSum = ToExpression[pSumStr]
  ];

  If[ (pSum == $Failed) || (NumericQ[pSum] == False),
    Print["Invalid input"];  (* cannot convert input to a numeric value *)
    Return[ errorReturn ]
  ];
  kPartialSumThresholdX[pSum, nDecDefault]
];    (* end of kPartialSumThreshold[string, decimals] *)


kPartialSumThreshold[pSumStr_String] :=
Block[
  { },
  kPartialSumThreshold[pSumStr, nDefaultDecimals]
];    (* end of kPartialSumThreshold[string] *)


kPartialSumThreshold[pSum0_?NumericQ] :=
Block[
(* a numeric value was entered.  assume the user has included double backquotes
    if they are needed; this calls the private function kPartialSumThresholdX. *)
  { },
  kPartialSumThresholdX[pSum0, nDefaultDecimals]
];

kPartialSumThreshold[pSum0_?NumericQ, nDecDefault_] :=
Block[
(* a numeric value was entered.  assume the user has included double backquotes
    if they are needed.  if they did not, there is no way to include them now.
    this calls the private function kPartialSumThresholdX. *)
  { },
  kPartialSumThresholdX[pSum0, nDecDefault]
];



(* these return the A and T matrices *)

kSumGetT[inputList_?VectorQ, iBase_:10] :=
Block[ (* given the input list, return the matrix A *)
  { lst },
  lst = convertInputListToStrings[inputList];
  If[Length[lst] == 0, Return[ {{0}} ] ];
  setT[lst, iBase]    (* return this matrix *)
];    (* end of kSumGetT[list] *)

kSumGetT[s_String, iBase_:10] :=
Block[ (* given the input string s, return the matrix A *)
  { stringList = { s } },
  kSumGetT[stringList, iBase]
] ;   (* end of kSumGetT[string] *)

kSumGetT[i_Integer, iBase_:10] :=
Block[ (* given the input integer, return the matrix A *)
  { stringList = { ToString[i] } },
  kSumGetT[stringList, iBase]
] ;   (* end of kSumGetT[integer] *)


kSumShowA[] := A;    (* show the current (private context) A matrix for the previously-entered input *)

kSumGetA[inputList_?VectorQ, iBase_:10] :=
Block[ (* given the input list, return the matrix A *)
  { lst2, T, f, j, l, m, A, n },

  (* use only local variables here, not the global variables A and n.
     this allows the user to call KempnerSum, then kSumGetA, then kPartialSum,
     without kSumGetA interfering with the global variables.
  *)

  lst2 = convertInputListToStrings[inputList];
  If[Length[lst2] == 0, Return[ {{0}} ] ];
  T = setT[lst2, iBase];

  (* the computation of A here is the same code as kSumX *)
  f[j_, l_, m_] := f[j, l, m] = If[T[[l, m]] == j, 1, 0] ;

  n = Dimensions[T][[1]] ;    (* number of rows *)

  A = (1/iBase) Sum[ Table[f[j, l, m + 1], {j, n}, {l, n}] , {m, 0, iBase-1} ] ;

  A    (* return this matrix *)

];    (* end of kSumGetA[list] *)

kSumGetA[s_String, iBase_:10] :=
Block[ (* given the input string s, return the matrix A *)
  { stringList = {} },
  AppendTo[stringList, s];
  kSumGetA[stringList, iBase]
];    (* end of kSumGetA[string] *)

kSumGetA[i_Integer, iBase_:10] :=
Block[ (* given the input integer, return the matrix A *)
  { stringList = {} },
  AppendTo[stringList, ToString[i]];
  kSumGetA[stringList, iBase]
];    (* end of kSumGetA[integer] *)






kSumTimeAndMemory[nDigits_] :=
Block[
  (* display the estimated time and memory needed to calculate KempnerSum[9, nDigits].
     based on calculations on a dell laptop for 100, 200, ..., 600 digits. *)
  { nd, t1, t0, tFast, mFast},
  nd = nDigits;
  If[nd < 50, nd = 50];

  t1 = Timing[ KempnerSum[9, 100] ][[1]];

  (* t0 = actual time for KempnerSum[9, 100] on the laptop used for benchmark *)
  t0 = 3.647 ;

  tFast = .55227 + .017407*nd + .00017173*nd^2 + (3.3708*10^-7)*nd^3 ;
  mFast = (6.5614*10^6) + 2836.1*nd + 18.432*nd^2 + .0054213*nd^3 ;

  (* the following cubic polynomial works better for 100 <= nd <= 1000 *)

  Print["Estimated time and memory for KempnerSum[9, ", nd, "]:" ];
  Print[Round[tFast*t1/t0], " seconds, ", Round[mFast/1000000], " MBytes of memory"];
  Print["  Note: KempnerSum[99, ", nd, "], KempnerSum[999, ", nd,"] etc., will use more time and memory." ];
];    (* end of kSumTimeAndMemory *)


kSumSetDefaultDecimals[i_Integer] :=
Block[
  (* set the number of default decimal places *)
  { },
  If[i < 1, i = 1];
  If[nDefaultDecimals == i,
    Print["Default number of decimal places is now set to ", i] ,
    Print["Default number of decimal places has been changed from ", nDefaultDecimals, " to ", i]
  ];
  nDefaultDecimals = i;
  nDefaultDecimals    (* return this value *)
];

kSumShowDefaultDecimals := nDefaultDecimals;



End[ ]         (* end the private context *)

(* protect the exported symbols *)
Protect[KempnerSum, kSumFormatted, kPartialSum, kSumGetT, kSumGetA, kSumShowA];
Protect[kSumTimeAndMemory, kSumSetDefaultDecimals, kSumShowDefaultDecimals];
Protect[kPartialSumThreshold];

EndPackage[ ]  (* end the package context *)
