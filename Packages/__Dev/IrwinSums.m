(* ::Package:: *)

(* ::Subtitle:: *)
(*A package to compute sums of irwin series*)


(* ::Subsection::Closed:: *)
(*Version information*)


(* ::Text:: *)
(*Mathematica Version: 4.0+ *)
(**)
(*Package Version: Version 1.3, August 16, 2008 *)
(**)
(*Author: Robert Baillie *)
(**)
(*Translator: GalAster - \:9171\:7d2b\:541b *)
(**)
(*Copyright: Non-Commercial *)


(* ::Subsection::Closed:: *)
(*Summary*)


(* ::Text:: *)
(*This package computes sums of "Irwin" series.*)
(**)
(*The harmonic series 1/1 + 1/2 + 1/3 + ... + 1/n + ... diverges. This means that the sum can be made as large as desired by adding enough terms.*)
(**)
(*Suppose we delete from this series all terms with a "9" in the denominator. That is, we remove 1/9, 1/19, 1/29, etc. Then the remaining series converges to a sum less than 80. This was proved in 1914 by A. J. Kempner.*)
(**)
(*In 1916, Irwin proved that the 对1/n求和 where n has at most a fixed number of occurrences of one or more digits, is a convergent series. This is a generalization of Kempner's result: in Kempner's series, the digit 9 occurs zero times. It also follows that the 对1/n求和 where n has exactly n1 occurrences of digit d1, n2 occurrences of d2, etc., converges.*)
(**)
(*However, these series converge much too slowly to get even a rough estimate of the sum simply by adding up the terms.*)
(**)
(*This package computes the sums of these series to high precision.*)
(**)
(**)
(**)
(**)
(*Keywords: kempner, irwin, harmonic series*)
(**)
(**)
(**)
(*Sources:*)
(*The original article that proves convergence of these series is:*)
(*J. Kempner, "A Curious Convergent Series",*)
(*American Mathematical Monthly, vol. 21, pages 48-50 (1914).*)
(**)
(*F. Irwin, "A Curious Convergent Series",*)
(*American Mathematical Monthly, vol. 23, pages 149-152 (1916).*)
(**)
(*R. Baillie, "Summing The Curious Series of Kempner and Irwin", May, 2008. Available at http://arxiv.org/abs/0806.4410 .*)


(* ::Subsection::Closed:: *)
(*Test*)


(* :Examples:
IrwinSum[9, 0] = 对1/n求和 where n has no 9's = 22.920676619264150 . IrwinSum[9, 1] = 对1/n求和 where n has exactly one 9 = 23.044287080747848 . IrwinSum[{9, 3}, {2, 0}] = 对1/n求和 where n has two 9's and no 3's
= 2.593253652747189 .

IrwinSum[9, 2] (this computes the 对1/n求和 where n has two 9's) Detailed explanation of the output:
1. sum = 23.026040265961244
2. sum for all 3 'at most' conditions = 68.991003965973242 3. sum for 0 occurrences = 22.920676619264150
4. sum for 1 occurrences = 23.044287080747848
5. sum for 2 occurrences = 23.026040265961244
Line 1: This is the requested 对1/n求和 where n has two 9's.
Line 2: Three sums were computed as part of this calculation: the sums of 1/n where n has zero, one, or two 9's. Line 3 shows the total of these three sums. This is just the 对1/n求和 where n has at most two 9's.
Line 3 is the 对1/n求和 where n has zero 9's. Line 4 is the 对1/n求和 where n has one 9.
Line 5 is the 对1/n求和 where n has two 9's.

IrwinSum[{9, 3}, {2, 1}] (compute the 对1/n求和 where n has two 9's and one 3) Detailed explanation of the output:
1. sum = 4.169026439566082
2. sum for all 6 'at most' conditions = 34.282119242240692 Line 1: This is the 对1/n求和 where n has two 9's and one 3.
Line 2: Six sums were computed as part of this calculation. They are the sums of 1/n where n has:
zero 9's and zero 3's zero 9's and one 3 one 9 and zero 3's one 9 and one 3
two 9's and zero 3's two 9's and one 3.
Line 2 shows the total of these six sums. This is just the 对1/n求和 where n has at most two 9's and one 3.
*)


(* ::Subsection:: *)
(*Main Package*)


BeginPackage[ "IrwinSums`" ];
(* usage messages for this context, and for the exported functions *) 
IrwinSums::usage = "IrwinSums.m 是一个用于计算 限位调和级数 (Irwin series) 的程序包.\r
这个程序包包含了函数 IrwinSum, iSumFormatted, iPartialSum, iPartialSumThreshold,以及选项 setPrintLevel." ;
(* here are usage messages for individual functions *)
IrwinSum::usage = "IrwinSum[d, k] 对 1/n 求和,数字d在每一项的分母只能出现k次.\r
IrwinSum[{d1, d2, ...}, {k1, k2, ...}] 表示数字 di 只能出现 ki 次.\r
IrwinSum[d, k, m] 以及 IrwinSum[{d1, d2, ...}, {k1, k2, ...}, m] 指定进制为m进制.\r\n

Examples:\r
  IrwinSum[9, 0] = 22.920676619264150 对1/n求和,去掉所有含9的项.\r
  IrwinSum[9, 1]  = 23.044287080747848 对1/n求和,但是保留只有一个9的项.\r
  IrwinSum[{9, 3}, {2, 0}]  = 2.593253652747189 对1/n求和,去掉所有含3的项,但是保留有两个9的项.\r\n
IrwinSum 的第三个参数表示精度,默认情况下是15位.\r
IrwinSum 的第四个参数表示进制,默认情况下是10进制.\r
如果你想要写后面的参数那么必须填前面的参数,不得省略.\r\n
举几个2进制下的例子:\r
IrwinSum[1, 1, 20, 2] = 对1/n求和,求和所有只含1个1的项,取20位精度,在二进制下.显然这个和等价于十进制下的 1/1 + 1/2 + 1/4 + ...= 2 .
IrwinSum[1, 3, 20, 2] = 对1/n求和,求和所有只含3个1的项,取20位精度,在二进制下. = 1.42859154585263812400 .
IrwinSum[0, 0, 20, 2] = 对1/n求和,去掉所有含0的项,取20位精度,在二进制下 = 1.60669515241529176378 ." ;

iSumFormatted::usage = "和 IrwinSum 基本一样,但是自带5位的格式化效果\r 注意:结果是个NumberForm ";

iPartialSum::usage = "iPartialSum[d, k, p] 对1/n求和,只对小于 10^p 的项筛选求和. \r
Example: IrwinSum[9, 0] = 22.920676619264150; iPartialSum[9,0, 30] = 21.971055078178619 .\r
iPartialSum[1, 1, 6, 15, 2] = 1.96875, 对1/n求和,n是二进制中只有1个1的所有项,计算到2^6项为止,取15位精度.\r
注意到这个和其实就是 1/1 + 1/2 + 1/4 + 1/8 + 1/16 + 1/32 = 63/32 = 1.96875." ;

iPartialSumThreshold::usage = "iPartialSumThreshold[d, k, s] 给出为了超过 s 需要对多少位的项求和.\r
  返回值有4个 { d1, d2, s1, s2 }.\r\n
Example 1. \r
iPartialSumThreshold[9, 1, 23] 计算只有一个9的限位和需要多少项才能超过 23.返回值 {80, 22.995762680948152, 81, 23.000125707332644} 则代表在求到某个81位数的时候和才超过23.\r
iPartialSum[9, 1, 80] = 22.995762680948152\r
iPartialSum[9, 1, 81] = 23.000125707332644 .\r\n
Example 2. \r
这个例子表明如果你使用浮点数那么有时会出错.\r
iPartialSumThreshold[9, 1, 23.044287080747] 返回 {-1, -1, -1, -1} 这是精度不够导致的,正确的输入方法是 23.044287080747``25 \r
iPartialSumThreshold[9, 1, 23.044287080747``25] 返回了正确的结果 {327, 23.04428708074693636344610077, 328, 23.04428708074702511802366170} .\r
以下语法同样成立:\r
 iPartialSumThreshold[{d1, d2 ,...}, {k1, k2, ...}, s] \r
 iPartialSumThreshold[d, k, s, nDecimals, base]  \r
 iPartialSumThreshold[{d1, d2 ,...}, {k1, k2, ...}, s, nDecimals, base]." ;

setPrintLevel::usage = "Set the print level = 0, 1, 2, 3, 4. Larger values produce more output. Default = 1." ;

Begin["`Private`"] (* begin the private context (implementation) *) Off[ General::spell1 ];
Off[ General::spell ];

(* global variables are here *)

(* sjk and sjkPrev are tables (arrays) of size maxJ by maxIndexUsed. the first dimension is a power, the second is an index number 1 .. maxIndexUsed.*)
sjk;
sjkPrev;

cumulativeSums1; (* this is a small array, size = countList[[1]] + 1 *) iSumPrintLevel = 1; (* this is the default print level *)
Unprotect[IrwinSum, iSumFormatted, iPartialSum, iPartialSumThreshold, setPrintLevel]; (* nf0 formats a number to (nDec) decimal places *)
nf0[y_, nDec_] := If[y == 0, 0, N[y, Max[1, Floor[1+Log[10, Abs[y]]]] + nDec] ];

(* nf1 formats a number to (nDec) decimal places, with a space every 5 digits *)
nf1[y_, nDec_] := If[y == 0, 0, NumberForm[y, Max[1, Floor[1+Log[10, Abs[y]]]] + nDec, DigitBlock->5, NumberSeparator->{""," "}] ];

(* depending on the flag, this does either standard or "NumberForm" formatting *) nf[y_, nDec_, iFlag_:0] := If[iFlag == 0, nf0[y, nDec], nf1[y, nDec] ];

Clear[power];
(* power2 is just like Power[], but power2 returns 0^0 = 1 with no warning messages *) power[0, 0] := 1; (* special case *)
power[x_, y_] := Power[x, y] ; (* in other cases, return same as built-in function *)

Clear[isThisASpecialDigit];
isThisASpecialDigit[iDigit_, nConditions_, digitList_List] := Block[
(* if iDigit is in the list of special digits, then return the 1-based location in the list; otherwise, return 0. *)
{ j },
For[j = 1, j <= nConditions, j++, If[iDigit == digitList[[j]], Return[j]]
];

0 (* return this value *)

]; (* end of isThisASpecialDigit *)

Clear[bn];
bn[iBase_Integer, n_Integer, nConditions_Integer, digitList_?VectorQ] := Block[
(* this is for a set of special digits
compute sum(k = 0 through iBase-1) of k^n (for k not a special digit). example: if iBase = 10, one special digit (d),
compute bn = 9 (n = 0), bn = 0^n + 1^n + ... + 9^n - d^n (n > 0).*)
{ bn = 0, k },
If[n == 0, Return[iBase - nConditions] ]; (* iBase minus number of special digits *) For[k = 1, k <= iBase - 1, k++,
If[isThisASpecialDigit[k, nConditions, digitList] != 0, Continue[] ]; bn += power[k, n];
];

Return[bn];
]; (* end of bn *)

Clear[getArrayIndexFromList];
getArrayIndexFromList[n_Integer, ci_?VectorQ, iList_?VectorQ] := Block[
(* this is called with nConditions = n and countList = ci. this function is the inverse of getListFromArrayIndex. *)
{ i, IrwinSum = 0, iProd = 1 }, IrwinSum = iList[[1]];
For[i = 2, i <= n, i++,
iProd = iProd * (ci[[i-1]] + 1); IrwinSum += iList[[i]] * iProd;
];

IrwinSum + 1 (* return this value; all arrays in mathematica are 1-based *)

]; (* end of getArrayIndexFromList *)

getListFromArrayIndex[iArrayIndex_Integer, n_Integer, ci_?VectorQ] := Block[
(*
given an array index, find the list iList[1..n] that gives this index. this is called with nConditions = n and countList = ci.
this function is the inverse of getArrayIndexFromList.
*)
{ i, iProd = 1, iTemp, iList = Table[0, {n}] }, iProd = Product[ci[[i]] + 1, {i, n-1}];
iTemp = iArrayIndex;
iTemp = iTemp - 1; (* for mma *) For[i = n-1, i >= 1, i--,
iList[[i+1]] = Floor[ iTemp/iProd ];
iTemp = iTemp - iProd*iList[[i+1]]; (* remainder *) iProd = Floor[ iProd/(ci[[i]] + 1) ];
];
iList[[1]] = iTemp;

iList (* return this list *)

]; (* end of getListFromArrayIndex *)

updateCumulativeSums1[iBase_Integer, nConditions_Integer, countList_?VectorQ] := Block[
(* if there is one condition, and we are summing the series for k occurrences of a digit, then keep track of all cumulative sums for i = 0, 1, 2, ..., k occurrences of that digit. *)
{ i, iArrayIndex, kOccurFound },
kOccurFound = Table[ 0, {iBase + 1} ]; For[i = 0, i <= countList[[1]], i++,
kOccurFound[[1]] = i;
iArrayIndex = getArrayIndexFromList[nConditions, countList, kOccurFound]; cumulativeSums1[[i+1]] += sjkPrev[[ 1, iArrayIndex ]];
];
]; (* end of updateCumulativeSums1 *)

printAllSums1[iDigit_Integer, nDec_Integer, nConditions_Integer, countList_?VectorQ, iFormatted_Integer] :=
Block[
(* if there is just one condition, then print
the sum for k occurrences, k = 0, 1, 2, ... . *)
{ k },
If[nConditions != 1, Return[] ];
If[iDigit > 0, Print[" iDigit = ", iDigit] ]; For[k = 0, k <= countList[[1]], k++,
Print[" sum for ", k, " occurrences = ", nf[cumulativeSums1[[k+1]] , nDec, iFormatted] ]
];
]; (* end of printAllSums1 *)

directSummation[iBase_Integer, numDigits_Integer, maxPower_Integer, nConditions_Integer, digitList_?VectorQ, countList_?VectorQ, nDec_Integer] :=
Block[
(* compute partial sums by directly adding terms whose denominators have (numDigits) digits *)
{ nTerms, (* return this value *)
iStart, iLast, i, iNumber, iDigitPos, iQuot, iRemainder, iDigit, jPower, k, kFound, iOK, iMatch, sumK = 0,
kOccurFound, iArrayIndex,
xNumb, xRecip, xRecipPower
},

kOccurFound = Table[0, {nConditions}]; (* 1-based, one for each special digit *) nTerms = 0;
iStart = iBase^(numDigits - 1); iLast = iBase^numDigits - 1;

For[i = iStart, i <= iLast, i++,
(* break up (i) into individual digits *) (* clear all counts *)
For[k = 1, k <= nConditions, k++, kOccurFound[[k]] = 0]; iNumber = i;
For[iDigitPos = 1, iDigitPos <= numDigits, iDigitPos++, iQuot = Floor[ iNumber/iBase ];
iRemainder = iNumber - iBase*iQuot; iDigit = iRemainder;
kFound = isThisASpecialDigit[iDigit, nConditions, digitList];
(* if this is the digit for condition number 2, increment the 2nd total *) If[kFound > 0,
kOccurFound[[kFound]]++ ;
];
iNumber = iQuot;
]; (* end for iDigitPos loop *)

iOK = 1;
iMatch = 1;
For[k = 1, k <= nConditions, k++, If[kOccurFound[[k]] > countList[[k]],
iOK = 0; (* this (i) had too many occurrences of one or more digits *) Goto[nextI]; (* go to next iteration of main (i) loop *)
];
If[kOccurFound[[k]] < countList[[k]], iMatch = 0];
]; (* end for k loop *)

If[iMatch == 1, (* (i) has n1 occurrences of digit d1, n2 of d2, etc. *) nTerms = nTerms + 1
];
xNumb = i;
xRecip = N[1/xNumb, nDec];

(* now compute the place in the sjkPrev array where this sum belongs *) iArrayIndex = getArrayIndexFromList[nConditions, countList, kOccurFound]; sjkPrev[[ 1, iArrayIndex ]] += xRecip;

xRecipPower = xRecip; (* xNumb^(-1) *) For[jPower = 2, jPower <= maxPower, jPower++,
xRecipPower = xRecipPower*xRecip; (* xNumb^(-jPower) *) sjkPrev[[ jPower, iArrayIndex ]] += xRecipPower;
];

Label[nextI];

]; (* end For i loop *) nTerms (* return this value *)

]; (* end of directSummation *)

Clear[computeMaxPowerNeeded];
computeMaxPowerNeeded[iBase_Integer, nDecimals_Integer, dsDigits_Integer] := Block[
(* enter with nDecimals, which specifies the desired tolerance, and dsDigits = 2nd parameter to directSummation.
let a = iBase^(dsDigits - 1), b = iBase^dsDigits - 1, and eps = 10^-nDecimals.
directSummation computes Sum[1/n^j, {n, a, b}] for j = 1, 2, ..., k.
we want to estimate the value of k that makes Sum[1/n^k, {n, a, b}] < eps .
in bases 2 through 10, a good approximation is Log[iBase, 10] * nDecimals/(dsDigits - 1).
if this is not close to a value that makes the sum < eps, then we must solve an equation. we estimate the sum by an integral:
Integrate[x^-c, {x, a, b}] = (b^(1-c) - a^(1-c))/(1-c). then we find the value of c that makes this < eps.
*)
{ eps, a, b, r0, k, ns, c, r },

eps = 10^-nDecimals; (* maximum tolerance *)
(* a and b are the lower and upper limits in directSummation *) a = iBase^(dsDigits - 1);
b = iBase^dsDigits - 1;
(* this r0 is usually a good approximation to the value we want *) r0 = Ceiling[ Log[iBase, 10] * nDecimals/(dsDigits - 1) ];
(* find a value that guarantees NSum < eps *) For[k = r0, k <= 10*r0, k++,
ns = NSum[1/n^k, {n, a, b}];
ns = Re[ns]; (* needed only for mathematica version 5.2 *) If[ns < eps, Goto[loopDone] ];
]; (* end for k loop *)

(* if we get here, we will have to solve the equation *)
r = FindRoot[(b^(1 - c) - a^(1 - c))/(1 - c) == eps, {c, r0} ];
r0 = Ceiling[c /. r]; (* get the numerical value from the rule *) (* find a value that guarantees NSum < eps *)
For[k = r0, k <= 10*r0, k++, ns = NSum[1/n^k, {n, a, b}];
ns = Re[ns]; (* needed only for mathematica version 5.2 *) If[ns < eps, Goto[loopDone] ];
]; (* end for k loop *)
Return[ -r0 ]; (* return this negative value to indicate an error *)

Label[loopDone];
r0 + 2 (* add 2 more powers just to be safe *)
]; (* end of computeMaxPowerNeeded *)

Clear[computeIrwinSum];
computeIrwinSum[iBase_Integer, digitList_?VectorQ, countList_?VectorQ, nDecimals0_Integer, iFormatted_Integer,
nDigits_:0, threshold_:-1 ] :=
Block[
(* this private function is the main calculation routine for the package. if nDigits > 0, this computes the partial sum through denominators
of (nDigits) digits, that is, denominators that are < 10^nDigits,
and then terminates without computing the complete sum. this parameter is used when the user calls iPartialSum[ ].
threshold is used when the user calls iPartialSumThreshold.

this usually returns a single number: either the sum >= 0, or an error value < 0. however, if a threshold > 0 was specified as input, then a 2-element list
{ sum, digit number } is returned.
*)
{
(* iPrint
= 0 for no output except for the final result,
= 1 for minimal output,
= 2 for some extra output,
= 3 for some output for each iteration of the loop,
= 4 for more output during the loop
*) iPrint,
nShow = 10, (* number of digits, for displaying during the loop *) nConditions, (* number of digits with a condition placed on them *) directSumDigits,
nDecimals, nDec,
i, j, nMax, n, j1, k1, (* index values in various loops *) iDigit, jPower, iDigitStart, k, numTerms, iAllTiny,
(* given a digit distribution, the array index values tell where to store a sum in the sjk and sjkPrev arrays *)
iArrayIndex, iArrayIndex0, iArrayIndex2, maxIndexUsed, maxJ,
jMaxPower,
maxDigits, (* arbitrary upper limit on outer loop, to prevent infinite loop *) maxTermAddedI, (* max term added for one iteration of the iDigit loop *) maxTermAddedJ, (* max term added for one iteration of the jPower loop *) maxTermArray,
iSpecialSum = 0, iDone = 0, tiny1, tiny2,
(* sum = cumulative sum that we are looking for, with exactly n1 d1's, n2 d2's, etc. *) requestedSum = 0,
(* sumOneDigit = requested sum for a single value of iDigit (that is, one power of 10) *) sumOneDigit = 0,
(* sumSmallerK is the cumulative sum over all 0 <= k1 <= n1, 0 <= k2 <= n2, etc. *) sumSmallerK = 0,
sumB1, sumB2, term1, term2, bnx2, ajn, kList, tableOfLists,
time0, time1, time2
},

iPrint = iSumPrintLevel;

(* we use nDecimals because we cannot reset the input parameter nDecimals0 *) nDecimals = nDecimals0;
If[nDecimals < 5, nDecimals = 5]; nDec = nDecimals + 2;

If[iPrint >= 2,
Print["iBase = ", iBase, ", digitList = ", digitList, ", countList = ", countList]; Print["nDecimals = ", nDecimals];
];

time0 = TimeUsed[];
nConditions = Length[digitList];

(* validate all input entered by the user *) If[ (iBase < 2) || (iBase > 10),
Print["Base = ", iBase, " must be in the range from 2 through 10."]; Return[0];
];

If[nConditions < 1,
Printf[" error 1: nConditions = ", nConditions, " must be at least 1."]; Return[0];
];
If[nConditions > iBase,
Print[" error 2: nConditions = ", nConditions, " must be <= base = ", iBase]; Return[0];
];

If[nConditions != Length[countList],
Print["Mismatch: digit list and count list have different lengths (", Length[digitList], " and ", Length[countList], ")"];
Return[0];
];

For[i = 1, i <= nConditions, i++,
If[ (digitList[[i]] < 0) || (digitList[[i]] >= iBase),
Print["digit # ", i, " = ", digitList[[i]], " is not valid in base ", iBase]; Return[0];
];
For[j = i+1, j <= nConditions, j++, If[digitList[[i]] == digitList[[j]],
Print["error: digit # ", i, " = ", digitList[[i]], " is duplicated"]; Return[0];
];
]; (* end for j loop *) If[countList[[i]] < 0,
Print["count # ", i, " = ", countList[[i]], " must be 0 or greater."]; Return[0];
];
]; (* end for i loop *)

(* if every non-zero digit has zero occurrences, then we have an empty sum. example: the 对1/n求和 where n has no 1 in base 2. just return 0 and a message. *)
If[nConditions == iBase-1, j = 0;
For[i = 0, i <= iBase-1, i++,
If[ (countList[[i]] == 0) && (digitList[[i]] != 0), j++;
]; (* end If[ ] *)
]; (* end for i loop *) If[j == iBase-1,
Print["all non-zero digits have 0 occurrences: this is an empty sum"]; Return[0];
];
]; (* end If[ ] *)

(* at this point, we can assume all input is valid. proceed with the calculation. *)

tiny1 = 1/10^(2 * nDec) ; tiny2 = 1/10^(nDec + 5) ;

(* the larger nDecimals is, the larger maxDigits and jMaxPower need to be. in most cases, maxDigits = 60 * nDecimals works.
maxDigits is used only to make sure the outer loop is finite, so we can set it to any reasonable value.
*)
maxDigits = 60 * nDecimals; If[maxDigits < 500, maxDigits = 500];
(* if nConditions = 1 and countList[[1]] = (say) 434, maxDigits needs to be fairly large *) If[Max[countList] > 10, maxDigits = maxDigits * 6];
If[iPrint >= 2, Print["maxDigits = ", maxDigits] ];

(* directSumDigits is roughly the log of 1000 to base (iBase). so, for base 10, directSumDigits = 3. for base 2, directSumDigits = 10.
*)
directSumDigits = Ceiling[ Log[iBase, 1000] ];

maxJ = computeMaxPowerNeeded[iBase, nDecimals, directSumDigits]; If[maxJ < 0,
maxJ = -maxJ; (* change back to positive, then print it *) Print["Could not find good estimate for maxJ; computed maxJ = ", maxJ]; Return[0];
];

If[iPrint >= 2,
Print["nDecimals = ", nDecimals, ", directSumDigits = ", directSumDigits, ", computed maxJ = ", maxJ];
];

jMaxPower = maxJ; (* this value depends on nDecimals *)

(* if all digits 0 through iBase-1 are assigned a maximum number of occurrences, then the series is finite. in this case, the number of digits in the largest possible denominator is the sum of the countList[[ ]] values. example: if we want one each of 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, then the (finite) series terminates after 10-digit denominators.
*)
If[nConditions == iBase,
iSpecialSum = Sum[countList[[i]], {i, nConditions}]
];

maxTermArray = Table[ 0, { maxJ } ];

(* sjk and sjkPrev are tables (arrays) of size maxJ by maxIndexUsed *) maxIndexUsed = Product[1 + countList[[i]], {i, nConditions}];

If[iPrint >= 3,
Print["maxIndexUsed = ", maxIndexUsed];
];

kList = Table[ 0, {nConditions + 1} ];
tableOfLists = Table[ 0, { maxIndexUsed }, { nConditions + 1 } ]; cumulativeSums1 = Table[ 0, { countList[[1]] + 1 } ];

For[i = 1, i <= maxIndexUsed, i++,
(* for each index value, compute the list and store it in tableOfLists. *) kList = getListFromArrayIndex[i, nConditions, countList];

(* debugging: check that the mapping and its inverse work properly *) iArrayIndex = getArrayIndexFromList[nConditions, countList, kList];
If[iArrayIndex != i,
Print["error: i = ", i, " != iArrayIndex = ", iArrayIndex]; Return[0];
];

For[j = 1, j <= nConditions, j++, tableOfLists[[i, j]] = kList[[j]]
];
]; (* end for i loop *)

iArrayIndex0 = getArrayIndexFromList[nConditions, countList, countList]; If[iArrayIndex0 != maxIndexUsed,
Print["error: iArrayIndex0 != maxIndexUsed: iArrayIndex0 = ", iArrayIndex0, ", maxIndexUsed = ", maxIndexUsed];
Return[0];
];

(* initialize sjkPrev and sjk to be 2-dimensional arrays *) sjk = Table[ 0, { maxJ }, { maxIndexUsed } ];

(* directly compute the sum for denominators having <= (directSumDigits) digits, that is, for denominators <= iBase^(directSumDigits) - 1.
*)
For[iDigit = 1, iDigit <= directSumDigits, iDigit++, (* set sjkPrev to an array of 0's *)
sjkPrev = Table[ 0, { maxJ }, { maxIndexUsed } ];
directSummation[iBase, iDigit, jMaxPower, nConditions, digitList, countList, nDec]; (* requestedSum is the partial sum, so far, of the desired series *)
requestedSum += sjkPrev[[ 1, iArrayIndex0 ]];

For[i = 1, i <= maxIndexUsed, i++, sumSmallerK += sjkPrev[[ 1, i ]]
]; (* end For i loop *)

If[ (nDigits > 0) && (nDigits == iDigit), iDone = 1
];

If[ (threshold > 0) && (requestedSum > threshold), Return[ { requestedSum , iDigit } ];
];

If[iDone == 1,
If[iPrint >= 1,
If[iBase == 10,
Print["partial sum through ", iDigit, " digits = ", requestedSum] ,
Print["partial sum through ", iDigit, " (base ", iBase, ") digits = ", requestedSum]
];
If[ (nConditions != 1) || (countList[[1]] != 0),
Print[" partial sum for all ", maxIndexUsed, " 'at most' conditions = ", sumSmallerK]
];
]; (* If[iPrint] *) Break[];
]; (* end if iDone == 1 *)

If[iPrint >= 3, Print[iDigit, " digits"]
];

If[iPrint >= 3,
Print[" partial sum for ", iDigit, " digits = ", sjkPrev[[ 1, iArrayIndex0 ]] ]; Print[" sum = ", requestedSum]
];

If[iPrint >= 3,
(* no need for next line if nConditions = 1 and countList[[1]] = 0 *) If[ (nConditions != 1) || (countList[[1]] != 0),
Print[" sum for all ", maxIndexUsed, " 'at most' conditions = ", sumSmallerK]
];
];

If[nConditions == 1, updateCumulativeSums1[iBase, nConditions, countList] ];

]; (* end For iDigit direct summation loop *)

time1 = TimeUsed[]; If[iDone == 1,
Goto[endLoops]
];

If[iPrint >= 2,
Print["direct sum through ", directSumDigits, " digits = ", requestedSum]
];

iDigitStart = directSumDigits + 1;

(* here is the main loop *)

For[iDigit = iDigitStart, iDigit <= maxDigits, iDigit++, (* outermost loop *) (* compute the sum of numbers in the set that have have (iDigit) digits,
using the sums of reciprocals of powers over numbers having (iDigit-1) digits
*)
maxTermAddedI = 0; (* largest term added for this value of iDigit *) maxTermArray = Table[ 0, { jMaxPower } ]; (* set maxTermArray to all 0's *)
For[jPower = 1, jPower <= jMaxPower, jPower++, nMax = jMaxPower - jPower;
maxTermAddedJ = 0; (* largest term added for this value of jPower *) For[iArrayIndex = 1, iArrayIndex <= maxIndexUsed, iArrayIndex++,
(* for this index value, set kList = corresponding n-tuple. *) kList = getListFromArrayIndex[iArrayIndex, nConditions, countList];

sumB1 = sumB2 = 0;
(* here is the "infinite" loop, where n goes from 0 to infinity *) For[n = 0, n <= nMax, n++,
ajn = Binomial[jPower + n - 1, n] / power[iBase, jPower + n]; If[OddQ[n], ajn = -ajn]; (* this is now (-1)^n * ajn *)

For[k = 1, k <= nConditions, k++, If[kList[[k]] > 0,
(* this is for { 10 * S(i, k1, k2, ..., k(k)-1, ..., kn) + d(k) } *) kList[[k]] = kList[[k]] - 1; (* decrement the kth index value *) iArrayIndex2 = getArrayIndexFromList[nConditions, countList, kList];
term1 = power[digitList[[k]], n] * ajn * sjkPrev[[ jPower + n, iArrayIndex2 ]]; sumB1 = sumB1 + term1;
maxTermAddedJ = Max[maxTermAddedJ, Abs[term1]];
kList[[k]] = kList[[k]] + 1; (* restore the kth index value *)
]; (* end if kList[k] *)
]; (* end For k loop *)

(* this is for the final set in the above union
{ 10 * S(i, k1, k2, ..., kn) + d. where d is none of d1, d2, ..., dn }
*)

bnx2 = bn[iBase, n, nConditions, digitList];
term2 = bnx2 * ajn * sjkPrev[[ jPower + n, iArrayIndex ]]; maxTermAddedJ = Max[maxTermAddedJ, Abs[term2]];
sumB2 = sumB2 + term2;

]; (* end For n loop *)

sjk[[jPower, iArrayIndex]] = sumB1 + sumB2;
]; (* end For iArrayIndex loop *)
maxTermArray[[jPower]] = maxTermAddedJ; maxTermAddedI = Max[maxTermAddedI, maxTermAddedJ];

]; (* end For jPower loop *)

sumOneDigit = sjk[[1, iArrayIndex0 ]]; (* sum for this many digits *) requestedSum += sumOneDigit;

If[ (threshold > 0) && (requestedSum > threshold), Return[ { requestedSum , iDigit } ];
];

For[i = 1, i <= maxIndexUsed, i++,
sumSmallerK += sjk[[1, i]] (* update the sum for all 'at most' conditions *)
];

If[ (nDigits > 0) && (nDigits == iDigit), (* request is to compute partial sums *) iDone = 1;
If[iPrint >= 1,
If[iBase == 10,
Print["partial sum through ", iDigit, " digits = ", requestedSum] ,
Print["partial sum through ", iDigit, " (base ", iBase, ") digits = ", requestedSum]
];
If[ (nConditions != 1) || (countList[[1]] != 0),
Print[" partial sum for all ", maxIndexUsed, " 'at most' conditions = ", sumSmallerK]
];
]; (* If[iPrint] *) Break[];
];

sjkPrev = sjk; (* this copies the entire array *)
If[nConditions == 1, updateCumulativeSums1[iBase, nConditions, countList] ]; If[iPrint >= 3,
Print[" partial sum for ", iDigit, " digits = ", N[sumOneDigit, nShow], ", total = ", N[requestedSum, nShow] ]
];

If[iPrint >= 4,
(* no need for next line if nConditions = 1 and countList[[1]] = 0 *) If[ (nConditions != 1) || (countList[[1]] != 0),
Print[" sum for all ", maxIndexUsed, " 'at most' conditions = ", N[sumSmallerK, nShow] ]
];
];

If[ (iSpecialSum > 0) && (iDigit > iSpecialSum), iDone = 1;
Print["this is a finite series that terminates after ", iSpecialSum, " digits"];
];

(* if all terms for a given jPower were very small, then we can decrease jMaxPower *) If[jMaxPower > 2,
j1 = jMaxPower;
For[jPower = jMaxPower, jPower >= 2, jPower--, If[maxTermArray[[jPower]] < tiny1,
j1 = jPower
,
Break[]
];
]; (* end for jPower loop *) If[jMaxPower != j1,
If[iPrint >= 4,
Print["iDigit = ", iDigit, ": changing jMaxPower from ", jMaxPower, " to ", j1]
];
jMaxPower = j1;
];
]; (* end if jMaxPower > 2 *)

If[nDigits > 0,
Continue[]; (* if calculating partial sums only, skip checks for convergence *)
];

(* if all sjk values are tiny, then we are done *) iAllTiny = 1;
For[k1 = 0, k1 <= maxIndexUsed, k1++, If[sjk[[2, k1 ]] > tiny2,
iAllTiny = 0; (* not all are tiny *) Break[];
]
];

If[ (iAllTiny == 1) && (sjk[[1, iArrayIndex0 ]] < tiny2),
If[ (sjk[[1, iArrayIndex0 ]] != 0) && (sjk[[1, iArrayIndex0 ]] / requestedSum < tiny2), iDone = 1;
time2 = TimeUsed[];
time2 = Round[time2-time0]; If[iPrint >= 2,
Print["last iteration of main loop:"];
Print[" max term added = ", maxTermAddedI, ", sum for ", iDigit, " digits = ", nf[sumOneDigit, nShow]];
];
If[iPrint >= 2,
Print["iteration done after ", iDigit, "-digit denominators (", time2, " seconds)."]
];
]; (* end If sjk ... *)
]; (* end If iAllTiny ... *)

If[ (iDigit == maxDigits) && (iDone == 0),
Print["last iteration (", maxDigits, ") of main loop, but no convergence yet."]; Print[" suggestion: make 'maxDigits' (now = ", maxDigits, ") larger,"];
Print[" read the file in again, and start over."];
Print[" partial sum for ", iDigit, " digits = ", N[sjk[[1, iArrayIndex0 ]], nShow] ]; requestedSum = 0;
Break[]
];

If[ (iDone == 1) && (iPrint > 0),
Print["sum = ", nf[requestedSum, nDecimals, iFormatted] ];
(* no need for next line if nConditions = 1 and countList[[1]] = 0 *) If[ (nConditions != 1) || (countList[[1]] != 0),
Print[" sum for all ", maxIndexUsed, " 'at most' conditions = ", nf[sumSmallerK, nDecimals, iFormatted] ];
If[nConditions == 1, printAllSums1[0, nDecimals, nConditions, countList, iFormatted] ];
];
Break[]
];

]; (* end For iDigit loop *) Label[endLoops];
(* return just the value of the requested sum *) nf[requestedSum, nDecimals, iFormatted]

]; (* end of private function computeIrwinSum *)

(* here are the functions that the user can call:
IrwinSum, iSumFormatted, iPartialSum, iPartialSumThreshold, setPrintLevel *)

Clear[IrwinSum];
IrwinSum[digitList_?VectorQ, countList_?VectorQ, nDecimals_:15, iBase_:10, iFormatted_:0] := Block[
(* examples:
IrwinSum[ { 9 }, { 3 } ] = 对1/n求和 where n has exactly three 9's.
IrwinSum[ { 9 }, { 3 }, 30 ] = same calculation, display result rounded to 30 decimals. IrwinSum[{9, 3}, {2, 0}] = 对1/n求和 where n has two 9's and no 3's
= 2.593253652747189 .
*)
{ },
computeIrwinSum[iBase, digitList, countList, nDecimals, iFormatted]

]; (* end of IrwinSum[digit list, count list, decimals, base] *)

IrwinSum[d_Integer, iCount_Integer, nDecimals_:15, iBase_:10, iFormatted_:0] := Block[
(* examples:
IrwinSum[ 9, 0 ] = 对1/n求和 where n has no 9's. IrwinSum[ 9, 2 ] = 对1/n求和 where n has two 9's.
IrwinSum[ 9, 2, 30 ] = same calculation, to 30 decimals.
*)

{ },
(* just call the "list" version of IrwinSum *)
IrwinSum[ { d }, { iCount }, nDecimals, iBase, iFormatted ]

]; (* end of IrwinSum[digit, count, decimals, base] *)

Clear[iSumFormatted];
iSumFormatted[digitList_?VectorQ, countList_?VectorQ, nDecimals_:15, iBase_:10] := Block[
{ iFormatted = 1 },
IrwinSum[digitList, countList, nDecimals, iBase, iFormatted]
]; (* end of iSumFormatted[digit list, count list, decimals, base] *)

iSumFormatted[d_Integer, iCount_Integer, nDecimals_:15, iBase_:10] := Block[
{ iFormatted = 1 },
IrwinSum[d, iCount, nDecimals, iBase, iFormatted]
]; (* end of iSumFormatted[digit, count, decimals, base] *)

Clear[setPrintLevel]; setPrintLevel[i_Integer] := Block[
{ i2 },
i2 = i;
(* 0 is the minimum value a user can set. however, some functions like iPartialSumThreshold can set it lower *)
If[i2 < 0, i2 = 0, Null]; iSumPrintLevel = i2;
Print["print level set to ", iSumPrintLevel]
]; (* end of setPrintLevel *)

Clear[iPartialSum];
iPartialSum[digitList_?VectorQ, countList_?VectorQ, nDigits_Integer?Positive, nDecimals_:15, iBase_:10] :=
Block[
(* examples of "list" version of iPartialSum:
iPartialSum[ {9}, {0}, 30 ] = sum to 10^30 of 1/n where n has no 9's.
iPartialSum[ { 9, 0 }, { 3, 1 }, 20 ] = sum to 10^20 of 1/n where n has three 9's and one
0.
*)
{ iFormatted = 0 },
computeIrwinSum[iBase, digitList, countList, nDecimals, iFormatted, nDigits]

]; (* end of iPartialSum[digit list, count list, numDigits, decimals, base] *)

iPartialSum[d_Integer, iCount_Integer, nDigits_Integer?Positive, nDecimals_:15, iBase_:10] := Block[
(* examples:
iPartialSum[ 9, 0, 20 ] = sum to 10^20 of 1/n where n has no 9's. iPartialSum[ 9, 2, 20 ] = sum to 10^20 of 1/n where n has two 9's. iPartialSum[ 9, 2, 20, 30 ] = same calculation, to 30 decimals.
iPartialSum[1, 1, 6, 15, 2] = partial 对1/n求和 where n has one 1
in base 2, for n < 2^6, to 15 decimals.
*)
{ },
(* just call the "list" version of iPartialSum *) iPartialSum[ { d }, { iCount }, nDigits, nDecimals, iBase ]

]; (* end of iPartialSum[digit, count, numDigits, decimals, base] *)

Clear[iPartialSumThreshold];
iPartialSumThreshold[digitList_?VectorQ, countList_?VectorQ, threshold_?Positive, nDecimals_:15, iBase_:10] :=
Block[
(* example of "list" version of iPartialSum: the 对1/n求和 where n has three 9's and one 0, is IrwinSum[{9, 0}, {3, 1}] = 2.888545932755274 . therefore, the threshold must be less than
this

*)

value. in this example, we take the threshold to be 2. iPartialSumThreshold[ { 9, 0 }, { 3, 1 }, 2 ]
= {27, 1.910422503190251, 28, 2.0043388417551473}
{ iFormatted = 0, nDigits = 0, totalSum, iPrintLevelSave,
xSum1 = 0, nDig1 = 0, xSum2, nDig2, errorReturn = {-1, -1, -1, -1}, tAcc, nDec2,
errStr = "Use backquote notation iPartialSumThreshold[ digit, count, threshold``nDecimals]
],\
or enclose the threshold in double quotes"
},

tAcc = 1 + Floor[Accuracy[threshold]]; If[tAcc == Infinity,
nDec2 = nDecimals,
nDec2 = Max[nDecimals, tAcc]
];

iPrintLevelSave = iSumPrintLevel;
iSumPrintLevel = -1; (* suppress all printed output except error messages *) totalSum = IrwinSum[digitList, countList, nDec2, iBase];
If[threshold > totalSum,
Print["Error: your threshold is greater than the sum of the entire series."]; iSumPrintLevel = iPrintLevelSave;
Return[ errorReturn ] (* error *)
];
If[threshold == totalSum,
Print["Error: your threshold is very close to the sum of the entire series. You need more accuracy. ", errStr];
iSumPrintLevel = iPrintLevelSave; Return[ errorReturn ] (* error *)
];

{ xSum2 , nDig2 } = computeIrwinSum[iBase, digitList, countList, nDec2, iFormatted, nDigits, threshold];
If[nDig2 > 1,
nDig1 = nDig2 - 1;
xSum1 = iPartialSum[digitList, countList, nDig1, nDec2, iBase];
];

If[ ( ! (xSum1 < threshold)) && (nDig2 > 1), (* xSum1 should be less than the threshold.
if it is not, decrease the number of digits by 1 *) nDig2 = nDig1;
xSum2 = xSum1; nDig1 = nDig2 - 1;
xSum1 = iPartialSum[digitList, countList, nDig1, nDec2, iBase];
];

iSumPrintLevel = iPrintLevelSave;

(* if the computed xSum1 and xSum2 are correct, then
xSum1 must be < threshold and xSum2 must be >= threshold. *) If[ ! (xSum1 < threshold),
Print["Not enough accuracy [1]. ", errStr]; Return[ errorReturn ] (* error *)
];
If[ ! (xSum2 >= threshold),
Print["Not enough accuracy [2]. ", errStr];
Return[ errorReturn ] (* error *)
];

{ nDig1, xSum1, nDig2, xSum2 }

]; (* end of iPartialSumThreshold[digit list, count list, threshold, decimals, base] *)

iPartialSumThreshold[d_Integer, iCount_Integer, threshold_?Positive, nDecimals_:15, iBase_:10] := Block[
(* examples:
iPartialSumThreshold[9, 1, 23]: for the 对1/n求和 where n has one 9,
whose sum is IrwinSum[9, 1] = 23.044287080747848, iPartialSumThreshold[9, 1, 23] computes about how far we need to go to reach a partial sum of 23.
the output is {80, 22.995762680948152, 81, 23.000125707332644}
interpretation of output:
the partial 对1/n求和 through n < 10^80 is 22.99576..., which is less than 23. however, if we include all terms through 81 digits (that is, n < 10^81),
then the partial sum is 23.000125707332644, greater than your threshold. now that we have the 80 and 81, we can use iPartialSum to verify this result:
iPartialSum[9, 1, 80] = 22.995762680948152
iPartialSum[9, 1, 81] = 23.000125707332644 .
iPartialSumThreshold[9, 1, 23, 30]: same calculation, to 30 decimals.

IrwinSum[1, 1, 15, 2] = 2 = sum (to 15 decimals) of 1/n where n has one 1 in base 2. iPartialSumThreshold[1, 1, 1.99, 15, 2] = point at which above series reaches 1.99:
{7, 1.984375000000000, 8, 1.9921875000000000}
iPartialSum confirms this result:
iPartialSum[1, 1, 7, 15, 2] = 1.984375000000000
iPartialSum[1, 1, 8, 15, 2] = 1.992187500000000 .
iPartialSumThreshold[9, 1, 23.044287080747] fails because the threshold is too close to the sum of the entire series. the best solution is to use Mathematica's backquote notation to increase the accuracy of the threshold. 23.044287080747``25 = 23.044287080747000000000000. the following gives the correct answer:
iPartialSumThreshold[9, 1, 23.044287080747``25] returns
{327, 23.04428708074693636344610077, 328, 23.04428708074702511802366170} .
you can also use double quotes to convert the threshold to a string: iPartialSumThreshold[9, 1, "23.044287080747"]
also returns the correct result
{327, 23.044287080746936363, 328, 23.044287080747025118} .
*)

{ },

(* call the list version of this function *)
iPartialSumThreshold[ { d }, { iCount }, threshold, nDecimals, iBase ]

]; (* end of iPartialSumThreshold[digit, count, threshold, decimals, base] *)

iPartialSumThreshold[digitList_?VectorQ, countList_?VectorQ, pSumStr_String, nDecimals_:15, iBase_:10] :=
Block[
(* the threshold was entered as a string. add double backquotes to specify the accuracy, then call another version of iPartialSumThreshold. *)
{ pSum, inputStr2, decPtList, quoteList, nDecimalsInput, nDec2, errorReturn = { -1, -1, -1, -1 }
},

decPtList = StringPosition[pSumStr, "."]; (* get list of starting, ending positions *) quoteList = StringPosition[pSumStr, "`"];

If[ (Length[decPtList] == 1) && (Length[quoteList] == 0),
(* input has one decimal point and no backquotes (usual case); number of decimals = number of chars between decimal point and end of string. append backquotes and a number to specify the accuracy of the input,
then convert the new string to a floating-point number with that accuracy.
*)
nDecimalsInput = StringLength[pSumStr] - decPtList[[1]][[1]] ;
(* nDec2 = Max[nDecimalsInput + 2, nDecimals]; *) (* from kempnerSums.m *) nDec2 = Max[nDecimalsInput + 5, nDecimals];
inputStr2 = pSumStr <> "``" <> ToString[nDec2] ;
pSum = ToExpression[inputStr2];
,
(* otherwise (backquotes already in use, or input is an integer): just convert the input string to a number *)
pSum = ToExpression[pSumStr]
];

If[ (pSum == $Failed) || (NumericQ[pSum] == False),
Print["Invalid input"]; (* cannot convert input to a numeric value *) Return[ errorReturn ]
];

iPartialSumThreshold[digitList, countList, pSum, nDecimals, iBase]

]; (* end of iPartialSumThreshold[digit list, count list, threshold string, decimals, base] *)

iPartialSumThreshold[d_Integer, iCount_Integer, pSumStr_String, nDecimals_:15, iBase_:10] := Block[
{ },
iPartialSumThreshold[ { d }, { iCount }, pSumStr, nDecimals, iBase]
]; (* end of iPartialSumThreshold[digit, count, threshold string, decimals, base] *) End[ ] (* end the private context *)
(* protect the exported symbols *)
Protect[IrwinSum, iSumFormatted, iPartialSum, iPartialSumThreshold, setPrintLevel]; EndPackage[ ] (* end the package context *)
