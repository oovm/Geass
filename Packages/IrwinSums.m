IrwinSums::usage = "IrwinSums.m 是一个用于计算 限位调和级数 (Irwin series) 的程序包.\r这个程序包包含了函数 IrwinSum, iSumFormatted, iPartialSum, iPartialSumThreshold,以及选项 setPrintLevel.";
IrwinSum::usage = "IrwinSum[d, k] 对 1/n 求和,数字d在每一项的分母只能出现k次.\rIrwinSum[{d1, d2, ...}, {k1, k2, ...}] 表示数字 di 只能出现 ki 次.\rIrwinSum[d, k, m] 以及 IrwinSum[{d1, d2, ...}, {k1, k2, ...}, m] 指定进制为m进制.\r\nExamples:\r  IrwinSum[9, 0] = 22.920676619264150 对1/n求和,去掉所有含9的项.\r  IrwinSum[9, 1]  = 23.044287080747848 对1/n求和,但是保留只有一个9的项.\r  IrwinSum[{9, 3}, {2, 0}]  = 2.593253652747189 对1/n求和,去掉所有含3的项,但是保留有两个9的项.\r\nIrwinSum 的第三个参数表示精度,默认情况下是15位.\rIrwinSum 的第四个参数表示进制,默认情况下是10进制.\r如果你想要写后面的参数那么必须填前面的参数,不得省略.\r\n举几个2进制下的例子:\rIrwinSum[1, 1, 20, 2] = 对1/n求和,求和所有只含1个1的项,取20位精度,在二进制下.显然这个和等价于十进制下的 1/1 + 1/2 + 1/4 + ...= 2 .IrwinSum[1, 3, 20, 2] = 对1/n求和,求和所有只含3个1的项,取20位精度,在二进制下. = 1.42859154585263812400 .IrwinSum[0, 0, 20, 2] = 对1/n求和,去掉所有含0的项,取20位精度,在二进制下 = 1.60669515241529176378 .";
iSumFormatted::usage = "和 IrwinSum 基本一样,但是自带5位的格式化效果\r 注意:结果是个NumberForm ";
iPartialSum::usage = "iPartialSum[d, k, p] 对1/n求和,只对小于 10^p 的项筛选求和. \rExample: IrwinSum[9, 0] = 22.920676619264150; iPartialSum[9,0, 30] = 21.971055078178619 .\riPartialSum[1, 1, 6, 15, 2] = 1.96875, 对1/n求和,n是二进制中只有1个1的所有项,计算到2^6项为止,取15位精度.\r注意到这个和其实就是 1/1 + 1/2 + 1/4 + 1/8 + 1/16 + 1/32 = 63/32 = 1.96875.";
iPartialSumThreshold::usage = "iPartialSumThreshold[d, k, s] 给出为了超过 s 需要对多少位的项求和.\r  返回值有4个 { d1, d2, s1, s2 }.\r\nExample 1. \riPartialSumThreshold[9, 1, 23] 计算只有一个9的限位和需要多少项才能超过 23.返回值 {80, 22.995762680948152, 81, 23.000125707332644} 则代表在求到某个81位数的时候和才超过23.\riPartialSum[9, 1, 80] = 22.995762680948152\riPartialSum[9, 1, 81] = 23.000125707332644 .\r\nExample 2. \r这个例子表明如果你使用浮点数那么有时会出错.\riPartialSumThreshold[9, 1, 23.044287080747] 返回 {-1, -1, -1, -1} 这是精度不够导致的,正确的输入方法是 23.044287080747``25 \riPartialSumThreshold[9, 1, 23.044287080747``25] 返回了正确的结果 {327, 23.04428708074693636344610077, 328, 23.04428708074702511802366170} .\r以下语法同样成立:\r iPartialSumThreshold[{d1, d2 ,...}, {k1, k2, ...}, s] \r iPartialSumThreshold[d, k, s, nDecimals, base]  \r iPartialSumThreshold[{d1, d2 ,...}, {k1, k2, ...}, s, nDecimals, base].";
setPrintLevel::usage = "Set the print level = 0, 1, 2, 3, 4. Larger values produce more output. Default = 1.";
Begin["`IrwinSums`"];
Off[ General::spell1 ];
Off[ General::spell ];
sjk;
sjkPrev;
cumulativeSums1;
iSumPrintLevel = 1;
Unprotect[IrwinSum, iSumFormatted, iPartialSum, iPartialSumThreshold, setPrintLevel];
nf0[y_, nDec_] := If[
	y == 0,
	0,
	N[y, Max[1, Floor[1 + Log[10, Abs[y]]]] + nDec]
];
nf1[y_, nDec_] := If[
	y == 0,
	0,
	NumberForm[y, Max[1, Floor[1 + Log[10, Abs[y]]]] + nDec, DigitBlock -> 5, NumberSeparator -> {"", " "}]
];
nf[y_, nDec_, iFlag_ : 0] := If[
	iFlag == 0,
	nf0[y, nDec],
	nf1[y, nDec]
];
power[0, 0] := 1;
power[x_, y_] := Power[x, y];
isThisASpecialDigit[iDigit_, nConditions_, digitList_List] := Block[
 { j },
	For[j = 1, j <= nConditions, j++,
		If[
			iDigit == digitList[[j]],
			Return[j]
		]];
	0
];
bn[iBase_Integer, n_Integer, nConditions_Integer, digitList_?VectorQ] := Block[
 { bn = 0, k },
	If[
		n == 0,
		Return[iBase - nConditions]
	];
	For[k = 1, k <= iBase - 1, k++, If[ isThisASpecialDigit[k, nConditions, digitList] != 0,
		Continue[]
	];
	bn += power[k, n];];
	Return[bn];
];
getArrayIndexFromList[n_Integer, ci_?VectorQ, iList_?VectorQ] := Block[
 { i, IrwinSum = 0, iProd = 1 },
	IrwinSum = iList[[1]];
	For[i = 2, i <= n, i++, iProd = iProd * (ci[[i - 1]] + 1);
	IrwinSum += iList[[i]] * iProd;];
	IrwinSum + 1
];
getListFromArrayIndex[iArrayIndex_Integer, n_Integer, ci_?VectorQ] := Block[
 { i, iProd = 1, iTemp, iList = Table[0, {n}] },
	iProd = Product[ci[[i]] + 1, {i, n - 1}];
	iTemp = iArrayIndex;
	iTemp = iTemp - 1;
	For[i = n - 1, i >= 1, i--, iList[[i + 1]] = Floor[ iTemp / iProd ];
	iTemp = iTemp - iProd * iList[[i + 1]];
	iProd = Floor[ iProd / (ci[[i]] + 1) ];];
	iList[[1]] = iTemp;
	iList
];
updateCumulativeSums1[iBase_Integer, nConditions_Integer, countList_?VectorQ] := Block[
 { i, iArrayIndex, kOccurFound },
	kOccurFound = Table[ 0, {iBase + 1} ];
	For[i = 0, i <= countList[[1]], i++, kOccurFound[[1]] = i;
	iArrayIndex = getArrayIndexFromList[nConditions, countList, kOccurFound];
	cumulativeSums1[[i + 1]] += sjkPrev[[ 1, iArrayIndex ]];];
];
printAllSums1[iDigit_Integer, nDec_Integer, nConditions_Integer, countList_?VectorQ, iFormatted_Integer] := Block[
 { k },
	If[ nConditions != 1,
		Return[]
	];
	If[ iDigit > 0,
		Print[" iDigit = ", iDigit]
	];
	For[k = 0, k <= countList[[1]], k++, Print[" sum for ", k, " occurrences = ", nf[cumulativeSums1[[k + 1]] , nDec, iFormatted] ]];
];
directSummation[iBase_Integer, numDigits_Integer, maxPower_Integer, nConditions_Integer, digitList_?VectorQ, countList_?VectorQ, nDec_Integer] := Block[
 { nTerms,
		iStart, iLast, i, iNumber, iDigitPos, iQuot, iRemainder, iDigit, jPower, k, kFound, iOK, iMatch, sumK = 0, kOccurFound, iArrayIndex, xNumb, xRecip, xRecipPower},
	kOccurFound = Table[0, {nConditions}];
	nTerms = 0;
	iStart = iBase^(numDigits - 1);
	iLast = iBase^numDigits - 1;
	For[i = iStart, i <= iLast, i++,
		For[k = 1, k <= nConditions, k++, kOccurFound[[k]] = 0];
		iNumber = i;
		For[iDigitPos = 1, iDigitPos <= numDigits, iDigitPos++, iQuot = Floor[ iNumber / iBase ];
		iRemainder = iNumber - iBase * iQuot;
		iDigit = iRemainder;
		kFound = isThisASpecialDigit[iDigit, nConditions, digitList];
		If[ kFound > 0,
			kOccurFound[[kFound]]++;
		];
		iNumber = iQuot;];
		iOK = 1;
		iMatch = 1;
		For[k = 1, k <= nConditions, k++, If[ kOccurFound[[k]] > countList[[k]],
			iOK = 0;
			Goto[nextI];
		];
		If[ kOccurFound[[k]] < countList[[k]],
			iMatch = 0
		];];
		If[ iMatch == 1,
			nTerms = nTerms + 1
		];
		xNumb = i;
		xRecip = N[1 / xNumb, nDec];
		iArrayIndex = getArrayIndexFromList[nConditions, countList, kOccurFound];
		sjkPrev[[ 1, iArrayIndex ]] += xRecip;
		xRecipPower = xRecip;
		For[jPower = 2, jPower <= maxPower, jPower++, xRecipPower = xRecipPower * xRecip;
		sjkPrev[[ jPower, iArrayIndex ]] += xRecipPower;];
		Label[nextI];];
	nTerms
];
computeMaxPowerNeeded[iBase_Integer, nDecimals_Integer, dsDigits_Integer] := Block[
 { eps, a, b, r0, k, ns, c, r },
	eps = 10^-nDecimals;
	a = iBase^(dsDigits - 1);
	b = iBase^dsDigits - 1;
	r0 = Ceiling[ Log[iBase, 10] * nDecimals / (dsDigits - 1) ];
	For[k = r0, k <= 10 * r0, k++, ns = NSum[1 / n^k, {n, a, b}];
	ns = Re[ns];
	If[ ns < eps,
		Goto[loopDone]
	];];
	r = FindRoot[(b^(1 - c) - a^(1 - c)) / (1 - c) == eps, {c, r0} ];
	r0 = Ceiling[c /. r];
	For[k = r0, k <= 10 * r0, k++, ns = NSum[1 / n^k, {n, a, b}];
	ns = Re[ns];
	If[ ns < eps,
		Goto[loopDone]
	];];
	Return[ -r0 ];
	Label[loopDone];
	r0 + 2
];
computeIrwinSum[iBase_Integer, digitList_?VectorQ, countList_?VectorQ, nDecimals0_Integer, iFormatted_Integer, nDigits_ : 0, threshold_ : -1 ] := Block[
 {
		iPrint, nShow = 10,
		nConditions,
		directSumDigits, nDecimals, nDec, i, j, nMax, n, j1, k1,
		iDigit, jPower, iDigitStart, k, numTerms, iAllTiny,
		iArrayIndex, iArrayIndex0, iArrayIndex2, maxIndexUsed, maxJ, jMaxPower, maxDigits,
		maxTermAddedI,
		maxTermAddedJ,
		maxTermArray, iSpecialSum = 0, iDone = 0, tiny1, tiny2,
		requestedSum = 0,
		sumOneDigit = 0,
		sumSmallerK = 0, sumB1, sumB2, term1, term2, bnx2, ajn, kList, tableOfLists, time0, time1, time2},
	iPrint = iSumPrintLevel;
	nDecimals = nDecimals0;
	If[ nDecimals < 5,
		nDecimals = 5
	];
	nDec = nDecimals + 2;
	If[ iPrint >= 2,
		Print["iBase = ", iBase, ", digitList = ", digitList, ", countList = ", countList];
		Print["nDecimals = ", nDecimals];
	];
	time0 = TimeUsed[];
	nConditions = Length[digitList];
	If[ (iBase < 2) || (iBase > 10),
		Print["Base = ", iBase, " must be in the range from 2 through 10."];
		Return[0];
	];
	If[ nConditions < 1,
		Print[" error 1: nConditions = ", nConditions, " must be at least 1."];
		Return[0];
	];
	If[ nConditions > iBase,
		Print[" error 2: nConditions = ", nConditions, " must be <= base = ", iBase];
		Return[0];
	];
	If[ nConditions != Length[countList],
		Print["Mismatch: digit list and count list have different lengths (", Length[digitList], " and ", Length[countList], ")"];
		Return[0];
	];
	For[i = 1, i <= nConditions, i++, If[ (digitList[[i]] < 0) || (digitList[[i]] >= iBase),
		Print["digit # ", i, " = ", digitList[[i]], " is not valid in base ", iBase];
		Return[0];
	];
	For[j = i + 1, j <= nConditions, j++, If[ digitList[[i]] == digitList[[j]],
		Print["error: digit # ", i, " = ", digitList[[i]], " is duplicated"];
		Return[0];
	];];
	If[ countList[[i]] < 0,
		Print["count # ", i, " = ", countList[[i]], " must be 0 or greater."];
		Return[0];
	];];
	If[ nConditions == iBase - 1,
		j = 0;
		For[i = 0, i <= iBase - 1, i++, If[ (countList[[i]] == 0) && (digitList[[i]] != 0),
			j++;
		];
		];
		If[ j == iBase - 1,
			Print["all non-zero digits have 0 occurrences: this is an empty sum"];
			Return[0];
		];
	];
	tiny1 = 1 / 10^(2 * nDec);
	tiny2 = 1 / 10^(nDec + 5);
	maxDigits = 60 * nDecimals;
	If[ maxDigits < 500,
		maxDigits = 500
	];
	If[ Max[countList] > 10,
		maxDigits = maxDigits * 6
	];
	If[ iPrint >= 2,
		Print["maxDigits = ", maxDigits]
	];
	directSumDigits = Ceiling[ Log[iBase, 1000] ];
	maxJ = computeMaxPowerNeeded[iBase, nDecimals, directSumDigits];
	If[ maxJ < 0,
		maxJ = -maxJ;
		Print["Could not find good estimate for maxJ; computed maxJ = ", maxJ];
		Return[0];
	];
	If[ iPrint >= 2,
		Print["nDecimals = ", nDecimals, ", directSumDigits = ", directSumDigits, ", computed maxJ = ", maxJ];
	];
	jMaxPower = maxJ;
	If[ nConditions == iBase,
		iSpecialSum = Sum[countList[[i]], {i, nConditions}]
	];
	maxTermArray = Table[ 0, { maxJ } ];
	maxIndexUsed = Product[1 + countList[[i]], {i, nConditions}];
	If[ iPrint >= 3,
		Print["maxIndexUsed = ", maxIndexUsed];
	];
	kList = Table[ 0, {nConditions + 1} ];
	tableOfLists = Table[ 0, { maxIndexUsed }, { nConditions + 1 } ];
	cumulativeSums1 = Table[ 0, { countList[[1]] + 1 } ];
	For[i = 1, i <= maxIndexUsed, i++,
		kList = getListFromArrayIndex[i, nConditions, countList];
		iArrayIndex = getArrayIndexFromList[nConditions, countList, kList];
		If[ iArrayIndex != i,
			Print["error: i = ", i, " != iArrayIndex = ", iArrayIndex];
			Return[0];
		];
		For[j = 1, j <= nConditions, j++, tableOfLists[[i, j]] = kList[[j]]];];
	iArrayIndex0 = getArrayIndexFromList[nConditions, countList, countList];
	If[ iArrayIndex0 != maxIndexUsed,
		Print["error: iArrayIndex0 != maxIndexUsed: iArrayIndex0 = ", iArrayIndex0, ", maxIndexUsed = ", maxIndexUsed];
		Return[0];
	];
	sjk = Table[ 0, { maxJ }, { maxIndexUsed } ];
	For[iDigit = 1, iDigit <= directSumDigits, iDigit++,
		sjkPrev = Table[ 0, { maxJ }, { maxIndexUsed } ];
		directSummation[iBase, iDigit, jMaxPower, nConditions, digitList, countList, nDec];
		requestedSum += sjkPrev[[ 1, iArrayIndex0 ]];
		For[i = 1, i <= maxIndexUsed, i++, sumSmallerK += sjkPrev[[ 1, i ]]];
		If[ (nDigits > 0) && (nDigits == iDigit),
			iDone = 1
		];
		If[ (threshold > 0) && (requestedSum > threshold),
			Return[ { requestedSum , iDigit } ];
		];
		If[ iDone == 1,
			If[ iPrint >= 1,
				If[ iBase == 10,
					Print["partial sum through ", iDigit, " digits = ", requestedSum],
					Print["partial sum through ", iDigit, " (base ", iBase, ") digits = ", requestedSum]
				];
				If[ (nConditions != 1) || (countList[[1]] != 0),
					Print[" partial sum for all ", maxIndexUsed, " 'at most' conditions = ", sumSmallerK]
				];
			];
			Break[];
		];
		If[ iPrint >= 3,
			Print[iDigit, " digits"]
		];
		If[ iPrint >= 3,
			Print[" partial sum for ", iDigit, " digits = ", sjkPrev[[ 1, iArrayIndex0 ]] ];
			Print[" sum = ", requestedSum]
		];
		If[ iPrint >= 3,
			If[ (nConditions != 1) || (countList[[1]] != 0),
				Print[" sum for all ", maxIndexUsed, " 'at most' conditions = ", sumSmallerK]
			];
		];
		If[ nConditions == 1,
			updateCumulativeSums1[iBase, nConditions, countList]
		];];
	time1 = TimeUsed[];
	If[ iDone == 1,
		Goto[endLoops]
	];
	If[ iPrint >= 2,
		Print["direct sum through ", directSumDigits, " digits = ", requestedSum]
	];
	iDigitStart = directSumDigits + 1;
	For[iDigit = iDigitStart, iDigit <= maxDigits, iDigit++,
		maxTermAddedI = 0;
		maxTermArray = Table[ 0, { jMaxPower } ];
		For[jPower = 1, jPower <= jMaxPower, jPower++, nMax = jMaxPower - jPower;
		maxTermAddedJ = 0;
		For[iArrayIndex = 1, iArrayIndex <= maxIndexUsed, iArrayIndex++,
			kList = getListFromArrayIndex[iArrayIndex, nConditions, countList];
			sumB1 = sumB2 = 0;
			For[n = 0, n <= nMax, n++, ajn = Binomial[jPower + n - 1, n] / power[iBase, jPower + n];
			If[ OddQ[n],
				ajn = -ajn
			];
			For[k = 1, k <= nConditions, k++, If[ kList[[k]] > 0,
				kList[[k]] = kList[[k]] - 1;
				iArrayIndex2 = getArrayIndexFromList[nConditions, countList, kList];
				term1 = power[digitList[[k]], n] * ajn * sjkPrev[[ jPower + n, iArrayIndex2 ]];
				sumB1 = sumB1 + term1;
				maxTermAddedJ = Max[maxTermAddedJ, Abs[term1]];
				kList[[k]] = kList[[k]] + 1;
			];
			];
			bnx2 = bn[iBase, n, nConditions, digitList];
			term2 = bnx2 * ajn * sjkPrev[[ jPower + n, iArrayIndex ]];
			maxTermAddedJ = Max[maxTermAddedJ, Abs[term2]];
			sumB2 = sumB2 + term2;];
			sjk[[jPower, iArrayIndex]] = sumB1 + sumB2;];
		maxTermArray[[jPower]] = maxTermAddedJ;
		maxTermAddedI = Max[maxTermAddedI, maxTermAddedJ];];
		sumOneDigit = sjk[[1, iArrayIndex0 ]];
		requestedSum += sumOneDigit;
		If[ (threshold > 0) && (requestedSum > threshold),
			Return[ { requestedSum , iDigit } ];
		];
		For[i = 1, i <= maxIndexUsed, i++, sumSmallerK += sjk[[1, i]]
		];
		If[ (nDigits > 0) && (nDigits == iDigit),
			iDone = 1;
			If[ iPrint >= 1,
				If[ iBase == 10,
					Print["partial sum through ", iDigit, " digits = ", requestedSum],
					Print["partial sum through ", iDigit, " (base ", iBase, ") digits = ", requestedSum]
				];
				If[ (nConditions != 1) || (countList[[1]] != 0),
					Print[" partial sum for all ", maxIndexUsed, " 'at most' conditions = ", sumSmallerK]
				];
			];
			Break[];
		];
		sjkPrev = sjk;
		If[ nConditions == 1,
			updateCumulativeSums1[iBase, nConditions, countList]
		];
		If[ iPrint >= 3,
			Print[" partial sum for ", iDigit, " digits = ", N[sumOneDigit, nShow], ", total = ", N[requestedSum, nShow] ]
		];
		If[ iPrint >= 4,
			If[ (nConditions != 1) || (countList[[1]] != 0),
				Print[" sum for all ", maxIndexUsed, " 'at most' conditions = ", N[sumSmallerK, nShow] ]
			];
		];
		If[ (iSpecialSum > 0) && (iDigit > iSpecialSum),
			iDone = 1;
			Print["this is a finite series that terminates after ", iSpecialSum, " digits"];
		];
		If[ jMaxPower > 2,
			j1 = jMaxPower;
			For[jPower = jMaxPower, jPower >= 2, jPower--, If[ maxTermArray[[jPower]] < tiny1,
				j1 = jPower,
				Break[]
			];];
			If[ jMaxPower != j1,
				If[ iPrint >= 4,
					Print["iDigit = ", iDigit, ": changing jMaxPower from ", jMaxPower, " to ", j1]
				];
				jMaxPower = j1;
			];
		];
		If[ nDigits > 0,
			Continue[];
		];
		iAllTiny = 1;
		For[k1 = 0, k1 <= maxIndexUsed, k1++, If[ sjk[[2, k1 ]] > tiny2,
			iAllTiny = 0;
			Break[];
		]];
		If[ (iAllTiny == 1) && (sjk[[1, iArrayIndex0 ]] < tiny2),
			If[ (sjk[[1, iArrayIndex0 ]] != 0) && (sjk[[1, iArrayIndex0 ]] / requestedSum < tiny2),
				iDone = 1;
				time2 = TimeUsed[];
				time2 = Round[time2 - time0];
				If[ iPrint >= 2,
					Print["last iteration of main loop:"];
					Print[" max term added = ", maxTermAddedI, ", sum for ", iDigit, " digits = ", nf[sumOneDigit, nShow]];
				];
				If[ iPrint >= 2,
					Print["iteration done after ", iDigit, "-digit denominators (", time2, " seconds)."]
				];
			];
		];
		If[ (iDigit == maxDigits) && (iDone == 0),
			Print["last iteration (", maxDigits, ") of main loop, but no convergence yet."];
			Print[" suggestion: make 'maxDigits' (now = ", maxDigits, ") larger,"];
			Print[" read the file in again, and start over."];
			Print[" partial sum for ", iDigit, " digits = ", N[sjk[[1, iArrayIndex0 ]], nShow] ];
			requestedSum = 0;
			Break[]
		];
		If[ (iDone == 1) && (iPrint > 0),
			Print["sum = ", nf[requestedSum, nDecimals, iFormatted] ];
			If[ (nConditions != 1) || (countList[[1]] != 0),
				Print[" sum for all ", maxIndexUsed, " 'at most' conditions = ", nf[sumSmallerK, nDecimals, iFormatted] ];
				If[ nConditions == 1,
					printAllSums1[0, nDecimals, nConditions, countList, iFormatted]
				];
			];
			Break[]
		];];
	Label[endLoops];
	nf[requestedSum, nDecimals, iFormatted]
];
IrwinSum[digitList_?VectorQ, countList_?VectorQ, nDecimals_ : 15, iBase_ : 10, iFormatted_ : 0] := Block[
 { },
	computeIrwinSum[iBase, digitList, countList, nDecimals, iFormatted]
];
IrwinSum[d_Integer, iCount_Integer, nDecimals_ : 15, iBase_ : 10, iFormatted_ : 0] := Block[
 { },
	IrwinSum[ { d }, { iCount }, nDecimals, iBase, iFormatted ]
];
iSumFormatted[digitList_?VectorQ, countList_?VectorQ, nDecimals_ : 15, iBase_ : 10] := Block[
	{ iFormatted = 1 },
	IrwinSum[digitList, countList, nDecimals, iBase, iFormatted]
];
iSumFormatted[d_Integer, iCount_Integer, nDecimals_ : 15, iBase_ : 10] := Block[
	{ iFormatted = 1 },
	IrwinSum[d, iCount, nDecimals, iBase, iFormatted]
];
setPrintLevel[i_Integer] := Block[
	{ i2 },
	i2 = i;
	If[ i2 < 0,
		i2 = 0,
		Null
	];
	iSumPrintLevel = i2;
	Print["print level set to ", iSumPrintLevel]
];
iPartialSum[digitList_?VectorQ, countList_?VectorQ, nDigits_Integer?Positive, nDecimals_ : 15, iBase_ : 10] := Block[
 { iFormatted = 0 },
	computeIrwinSum[iBase, digitList, countList, nDecimals, iFormatted, nDigits]
];
iPartialSum[d_Integer, iCount_Integer, nDigits_Integer?Positive, nDecimals_ : 15, iBase_ : 10] := Block[
 { },
	iPartialSum[ { d }, { iCount }, nDigits, nDecimals, iBase ]
];
iPartialSumThreshold[digitList_?VectorQ, countList_?VectorQ, threshold_?Positive, nDecimals_ : 15, iBase_ : 10] := Block[
 { iFormatted = 0, nDigits = 0, totalSum, iPrintLevelSave, xSum1 = 0, nDig1 = 0, xSum2, nDig2, errorReturn = {-1, -1, -1, -1}, tAcc, nDec2, errStr = "Use backquote notation iPartialSumThreshold[ digit, count, threshold``nDecimals]],\or enclose the threshold in double quotes"},
	tAcc = 1 + Floor[Accuracy[threshold]];
	If[ tAcc == Infinity,
		nDec2 = nDecimals,
		nDec2 = Max[nDecimals, tAcc]
	];
	iPrintLevelSave = iSumPrintLevel;
	iSumPrintLevel = -1;
	totalSum = IrwinSum[digitList, countList, nDec2, iBase];
	If[ threshold > totalSum,
		Print["Error: your threshold is greater than the sum of the entire series."];
		iSumPrintLevel = iPrintLevelSave;
		Return[ errorReturn ]
	];
	If[ threshold == totalSum,
		Print["Error: your threshold is very close to the sum of the entire series. You need more accuracy. ", errStr];
		iSumPrintLevel = iPrintLevelSave;
		Return[ errorReturn ]
	];
	{ xSum2 , nDig2 } = computeIrwinSum[iBase, digitList, countList, nDec2, iFormatted, nDigits, threshold];
	If[ nDig2 > 1,
		nDig1 = nDig2 - 1;
		xSum1 = iPartialSum[digitList, countList, nDig1, nDec2, iBase];
	];
	If[ ( ! (xSum1 < threshold)) && (nDig2 > 1),
		nDig2 = nDig1;
		xSum2 = xSum1;
		nDig1 = nDig2 - 1;
		xSum1 = iPartialSum[digitList, countList, nDig1, nDec2, iBase];
	];
	iSumPrintLevel = iPrintLevelSave;
	If[ ! (xSum1 < threshold),
		Print["Not enough accuracy [1]. ", errStr];
		Return[ errorReturn ]
	];
	If[ ! (xSum2 >= threshold),
		Print["Not enough accuracy [2]. ", errStr];
		Return[ errorReturn ]
	];
	{ nDig1, xSum1, nDig2, xSum2 }
];
iPartialSumThreshold[d_Integer, iCount_Integer, threshold_?Positive, nDecimals_ : 15, iBase_ : 10] := Block[
 { },
	iPartialSumThreshold[ { d }, { iCount }, threshold, nDecimals, iBase ]
];
iPartialSumThreshold[digitList_?VectorQ, countList_?VectorQ, pSumStr_String, nDecimals_ : 15, iBase_ : 10] := Block[
 { pSum, inputStr2, decPtList, quoteList, nDecimalsInput, nDec2, errorReturn = { -1, -1, -1, -1 }},
	decPtList = StringPosition[pSumStr, "."];
	quoteList = StringPosition[pSumStr, "`"];
	If[ (Length[decPtList] == 1) && (Length[quoteList] == 0),
		nDecimalsInput = StringLength[pSumStr] - decPtList[[1]][[1]];
		nDec2 = Max[nDecimalsInput + 5, nDecimals];
		inputStr2 = pSumStr <> "``" <> ToString[nDec2];
		pSum = ToExpression[inputStr2];,
		pSum = ToExpression[pSumStr]
	];
	If[ (pSum == $Failed) || (NumericQ[pSum] == False),
		Print["Invalid input"];
		Return[ errorReturn ]
	];
	iPartialSumThreshold[digitList, countList, pSum, nDecimals, iBase]
];
iPartialSumThreshold[d_Integer, iCount_Integer, pSumStr_String, nDecimals_ : 15, iBase_ : 10] := Block[
	{ },
	iPartialSumThreshold[ { d }, { iCount }, pSumStr, nDecimals, iBase]
];
SetAttributes[
	{IrwinSum, iSumFormatted, iPartialSum, iPartialSumThreshold, setPrintLevel},
	{Protected, ReadProtected}
];
End[]