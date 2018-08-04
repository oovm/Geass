KempnerSums::usage = "KempnerSums.m是一个用来计算 缺位调和级数(Kempner series)的程序包.\r\n
本程序包包含函数KempnerSum[X],kSumFormatted[X],kPartialSum[nDigits],kPartialSumThreshold[s],kSumGetA[X],kSumGetT[X]\r
以及选项kSumShowA,kSumTimeAndMemory,kSumSetDefaultDecimals,kSumShowDefaultDecimals.";
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
Begin["`KempnerSums`"];
Off[ General::spell1 ];
Off[ General::spell ];
nDefaultDecimals = 15;
nf[y_, nDec_] := If[
	y == 0,
	0,
	NumberForm[y, Max[ 1, Floor[1 + Log[10, Abs[y]]] ] + nDec, DigitBlock -> 5, NumberSeparator -> {"", " "}]
];
removeDups[inputList_?VectorQ] := Block[
	{ },
	elimReps[u_, v_] := If[MemberQ[u, v], u, Append[u, v]];
	Fold[elimReps, {First[inputList]}, Rest[inputList]]
];
normOne[v_?VectorQ] := Apply[Plus, Map[Abs, v] ];
power2[0, 0] := 1; power2[x_, y_] := Power[x, y] ;
    
A = Null;
extraPol = 0;
n = 0;
Psi = Null;
iBaseSave = 0;
kSumSave = 0;
nDecimalsSave = 0;
convertInputListToStrings[inputList_?VectorQ] := Block[
	{ len, i, str, outputList = { }, bSet },
	len = Length[inputList];
	For[i = 1, i <= len, i++,
		bSet = False;
		If[StringQ[ inputList[[i]] ], str = inputList[[i]]; bSet = True];
		If[IntegerQ[ inputList[[i]] ], str = ToString[ inputList[[i]] ]; bSet = True];
		If[bSet == False,
			Print["Error: Invalid input list: value #", i , " is neither a String nor an Integer."];
			Return[ {} ]
		];
		AppendTo[outputList, str];
	];
	outputList
];
setT[stringList_?VectorQ, iBase_ : 10] := Block[
	{ nInputStrings, i, s, s2, len, validDigits, j, t, iRow, firstNChars, d, dTest, found, jStart,
		inputList0, inputList = {}, 
		nSingleDigits = 0, nRows = 0, firstRow, nSetStrings = 0, setStrings = {},
		firstJChars, bFound, k, dString, s2Appended, maxMatched, nCompare, iSaved, t2 },
	If[( iBase < 2) || (IntegerQ[iBase] == False),
		Print["Error: Base = ", iBase, " is not valid."];
		t = Table[-1, {1}, {1}];    
		Return[t]
	];
	inputList0 = removeDups[stringList];    
	nInputStrings = Length[inputList0];
	If[nInputStrings < 1,
		Print["Error: No input string specified."];
		t = Table[-1, {1}, {iBase}];    
		Return[t]
	];
	If[iBase <= 10, 
		validDigits = CharacterRange["0", ToString[iBase - 1]]
	];
	For[i = 1, i <= nInputStrings, i++,
		If[UnsameQ[ Head[inputList0[[i]]] , String ] && UnsameQ[ Head[inputList0[[i]]] , Integer ],
			Print["Error: Invalid input list: value[", i, "] is not a String."];
			If[Head[inputList0[[i]]] === Symbol, Print["Two commas in a row."] ];
			t = Table[-1, {1}, {iBase}];    
			Return[t]
		];
		s = inputList0[[i]];
		If[Head[s] === Integer, s = ToString[s]];
		s2 = StringReplace[s, " " -> ""];    
		len = StringLength[s2];
		If[len == 0, Continue[]];
		If[iBase <= 10, 
			For[j = 1, j <= len, j++,
				If[MemberQ[validDigits, StringTake[s2, {j}]] == False, 
					Print["Error: input character '", StringTake[s2, {j}], "' is not a valid digit in base ", iBase];
					t = Table[-1, {1}, {iBase}];    
					Return[t]
				];
			];
		];
		AppendTo[inputList, s2];
		If[len == 1, nSingleDigits++];
	];    
	If[nSingleDigits == nInputStrings,
		firstRow = Table[1, {1}, {iBase}];
		For[i = 1, i <= nInputStrings, i++,
			If[StringLength[ inputList[[i]] ] == 1,
				j = ToExpression[ inputList[[i]] ];
				firstRow[[1, j + 1]] = 0;
			];
		];
		Return[firstRow]
	];
	setStrings = { "" }; nSetStrings = 1;    
	For[i = 1, i <= nInputStrings, i++, 
		s2 = inputList[[i]];
		len = StringLength[s2];
		If[len > 1, 
			For[j = 1, j <= len - 1, j++,
				firstJChars = StringTake[s2, j];
				bFound = False;
				For[k = 1, k <= nSetStrings, k++,
					If[setStrings[[k]] == firstJChars, bFound = True; Break[] ];
				];
				If[bFound == False,
					AppendTo[setStrings, firstJChars];
					nSetStrings++;
				];
			];    
		];    
	];    
	nRows = Length[setStrings];
	t = Table[1, {nRows}, {iBase}];
	For[iRow = 1, iRow <= nRows, iRow++,
		s2 = setStrings[[iRow]];
		For[d = 0, d < iBase, d++,
			dString = ToString[d];
			s2Appended = StringJoin[s2, dString];
			If[ Length[Position[inputList, dString]] > 0,
				t[[iRow, d + 1]] = 0;
				Continue[];    
			];
			bFound = False;
			For[i = 1, i <= nInputStrings, i++,
				If[ s2Appended == inputList[[i]], t[[iRow, d + 1]] = 0;
				bFound = True; Break[] ];  
			];  
			If[bFound == True, Continue[] ];
			For[i = 1, i <= nInputStrings, i++,
				If[ Length[StringPosition[ s2Appended, inputList[[i]] ] ] > 0,
					t[[iRow, d + 1]] = 0;
					bFound = True; Break[];
				];
			];
			If[bFound == True, Continue[] ];
			maxMatched = 0;    
			iSaved = 0;
			For[i = 2, i <= nSetStrings, i++,
				nCompare = StringLength[ setStrings[[i]] ];
				If[StringLength[s2Appended] < nCompare, Continue[] ];
				If[ StringTake[s2Appended, -nCompare] == StringTake[setStrings[[i]], -nCompare],
					bFound = True;
					If[StringLength[ setStrings[[i]] ] > maxMatched,
						maxMatched = StringLength[ setStrings[[i]] ];
						iSaved = i;
					];
				];
			];  
			If[bFound, t[[iRow, d + 1]] = iSaved];
		];  
	];    
	t    
];    
kSumX[stringList_?VectorQ, T_?MatrixQ, nDecimals0_Integer, iBase_] := Block[
	{ nDecimals, inputLen, nDecSum, f, c, a, S, z, nInputDigits, totalDigits,
		iMaxBase, goalMult, matB, wTrunc2, i, d, l, m, w, h,
		iPrint = 0,
		xi },
	Clear[A, Psi, extraPol, n, iMaxBase];    
	inputLen = Min[Map[StringLength, stringList]];
	nDecimals = nDecimals0;
	If[nDecimals < 1, nDecimals = 1];
	iMaxBase = Ceiling[ Log[iBase, 1000] ];
	If[iMaxBase < 2, iMaxBase = 2];    
	iBaseSave = iBase;    
	goalMult = 1.5;
	If[iBase < 10, goalMult = 3.2 ];
	If[iBase >= 100, goalMult = 1.1 ];
	nInputDigits = inputLen;
	totalDigits = nDecimals + nInputDigits;    
	If[iBase > 10, totalDigits = nDecimals + 2 * nInputDigits ];
	nDecSum = Floor[goalMult * totalDigits];
	If[iBase > 10,
		nDecSum = nDecSum + Ceiling[Log[10, iBase] * nInputDigits]
	];
	f[j_, l_, m_] := f[j, l, m] = If[T[[l, m]] == j, 1, 0] ;
	n = Dimensions[T][[1]] ;    
	A = (1 / iBase) Sum[ Table[f[j, l, m + 1], {j, n}, {l, n}] , {m, 0, iBase - 1} ] ;
	c[k_, w_] := c[k, w] = Binomial[k + w - 1, w] ;
	a[k_, w_, m_] := iBase^(-k - w) * c[k, w] * (-1)^w * power2[m, w] ;
	matB[a_?MatrixQ] := (Inverse[# - a] - #) &[IdentityMatrix[Length[a]]];
	wTrunc2[i_, k_] := Max[Floor[(nDecSum + 1) / (i - 1) + 1 - k] + 1, 0] ; 
	extraPol = nDecSum + 1;
	S[i_, j_] := S[i, j] =
		If[i == 1, Complement[Extract[Table[d, {d, 0, iBase - 1}], Position[T[[1]], j]], {0}] ,
			Block[{ p, el, m, k, elel, mm },
				p = Position[Table[T[[el, m]], {el, n}, {m, iBase}], j]; h = {};
				For[k = 1, k <= Length[p], k++,
					{elel, mm} = p[[k]]; h = Join[h, iBase * S[i - 1, elel] + (mm - 1)]
				]
			];    
			h    
		] ;    
	Psi[i_, j_, k_] := Psi[i, j, k] = If[
		i <= iMaxBase,
		Sum[ N[1 / (S[i, j][[xi]])^k, nDecSum], { xi, Length[S[i, j]] } ]
		,
		Sum[
			f[j, l, m + 1] * Sum[a[k, w, m] * Psi[i - 1, l, k + w],
				{w, 0, wTrunc2[i, k]}], {m, 0, iBase - 1}, {l, 1, n}
		]
	] ;    
	z = Sum[
		Psi[i, j, 1], {i, 1, extraPol}, {j, 1, n}] +
		normOne[matB[A] . Table[Psi[extraPol, j, 1], {j, n}]
		];
	If[z == 0, kSumSave = 0, kSumSave = N[z, Floor[ 1 + Log[10, z] ] + nDecimals ] ];
	nDecimalsSave = nDecimals;
	kSumSave    
];    
KempnerSum[T_?MatrixQ, nDecimals_Integer] := Block[
	{ n, iBase, i, stringList = { }, s = ""},
	If[nDecimals < 1, nDecimals = 1];
	n = Dimensions[T][[1]];    
	If[Length[ Dimensions[T] ] == 1,
		iBase = Length[T], 
		iBase = Dimensions[T][[2]]; 
	];
	For[i = 1, i <= n, i++, s = StringJoin[s, "0"] ];
	AppendTo[stringList, s];
	kSumX[stringList, T, nDecimals, iBase]
];    
KempnerSum[T_?MatrixQ] := Block[
	{ },
	KempnerSum[T, nDefaultDecimals]
];    
KempnerSum[inputList_?VectorQ, nDecimals_Integer, iBase_ : 10] := Block[
	{ lst, T },
	lst = convertInputListToStrings[inputList];
	If[Length[lst] == 0, Return[0] ];
	T = setT[lst, iBase];
	If[ T[[1, 1]] < 0, Return[0] ];
	kSumX[lst, T, nDecimals, iBase]
] ;   
KempnerSum[inputList_?VectorQ] := Block[
	{ },
	KempnerSum[inputList, nDefaultDecimals]
];    
KempnerSum[s_String, nDecimals_Integer, iBase_ : 10] := Block[
	{ stringList = { s } },
	KempnerSum[stringList, nDecimals, iBase]
] ;   
KempnerSum[s_String] := Block[
	{ stringList = { s } },
	KempnerSum[stringList, nDefaultDecimals]
];    
KempnerSum[i_Integer, nDecimals_Integer, iBase_ : 10] := Block[
	{ stringList = { ToString[i] } },
	KempnerSum[stringList, nDecimals, iBase]
];    
KempnerSum[i_Integer] := Block[
	{ stringList = { ToString[i] } },
	KempnerSum[stringList, nDefaultDecimals]
];    
kSumFormatted[T_?MatrixQ, nDecimals_Integer, iBase_ : 10] := Block[
	{ z = KempnerSum[T, nDecimals] },
	nf[z, nDecimals]
];    
kSumFormatted[T_?MatrixQ] := Block[
	{ nDecimals = nDefaultDecimals, z },
	z = KempnerSum[T, nDecimals];
	nf[z, nDecimals]
];    
kSumFormatted[inputList_?VectorQ, nDecimals_Integer, iBase_ : 10] := Block[
	{ z = KempnerSum[inputList, nDecimals, iBase] },
	nf[z, nDecimals]
] ;
kSumFormatted[inputList_?VectorQ] := Block[
	{ nDecimals = nDefaultDecimals, z },
	z = KempnerSum[inputList, nDecimals];
	nf[z, nDecimals]
]  ;
kSumFormatted[s_String, nDecimals_Integer, iBase_ : 10] := Block[
	{ stringList = { s } },
	kSumFormatted[stringList, nDecimals, iBase]
] ;
kSumFormatted[s_String] := Block[
	{ stringList = { s } },
	kSumFormatted[stringList, nDefaultDecimals]
]  ;
kSumFormatted[i_Integer, nDecimals_Integer, iBase_ : 10] := Block[
	{ stringList = { ToString[i] } },
	kSumFormatted[stringList, nDecimals, iBase]
]  ;
kSumFormatted[i_Integer] := Block[
	{ stringList = { ToString[i] } },
	kSumFormatted[stringList, nDefaultDecimals]
];
kPartialSum[nDigits_Integer?Positive, nDecimals_Integer] := Block[
	{ partSum = 0, i, j, matBTrunc },
	matBTrunc[a_?MatrixQ, M_] := Block[
		{ nDec, ndigSum,  a3 },
		If[nDecimals < 1, nDecimals = 1];
		ndigSum = 1 + Floor[Log[10, kSumSave]];
		nDec = nDecimals + ndigSum + (1 + Floor[Log[10, M + 1]]) + 5;  
		    
		a3 = MatrixPower[N[a, nDec], M + 1] ;  
		((# - a3).Inverse[# - a] - #) &[ IdentityMatrix[Length[a]] ]
	];    
	If[ (extraPol <= 0) || (kSumSave == 0), 
		Print["Error: no sum has been computed yet.  Call KempnerSum[ ] or kSumFormatted[ ] first."];
		Return[0]
	];
	If[nDigits > extraPol,
		partSum = Check[ Sum[Psi[i, j, 1], {i, 1, extraPol}, {j, 1, n}] +
			normOne[matBTrunc[A, nDigits - extraPol] . Table[Psi[extraPol, j, 1], {j, n}] ] ,
			0, General::unfl ];    
		,
		partSum = Sum[Psi[i, j, 1], {i, 1, nDigits}, {j, 1, n}]
	];  
	If[partSum == 0, 0, N[partSum, Floor[ 1 + Log[10, partSum] ] + nDecimals ] ]
] ;   
kPartialSum[nDigits_Integer?Positive] := Block[
	{ },
	kPartialSum[nDigits, nDefaultDecimals]
];
kPartialSumThresholdX[pSum0_?NumericQ, nDecDefault_] := Block[
	{ i, s, psa, psb, psc, a, b, c, nDec, diff, pSum, iLimit = 30,
		errorReturn = { -1, -1, -1, -1 } },
	If[ (extraPol <= 0) || (kSumSave == 0),
		Print["Error: no sum has been computed yet.  Call KempnerSum[ ] or kSumFormatted[ ] first."];
		Return[ errorReturn ]
	];
	pSum = pSum0;
	If[pSum <= 0, Return[ errorReturn ] ];    
	nDec = nDecDefault;
	If[nDec < nDecimalsSave, nDec = nDecimalsSave];
	If[Accuracy[pSum] < nDec, pSum = SetAccuracy[pSum, nDec] ];
	Print["Calculate where the partial sum exceeds ", pSum];
	s = kSumSave;
	If[pSum > s,
		Print["Value must be less than the actual sum (", s, ")."];
		Return[ errorReturn ]
	];    
	If[pSum >= s,
		Print[pSum, " is too close to the actual sum (", s, ")."];
		Print["Either enter a smaller value, or call KempnerSum again with more than ", nDecimalsSave, " decimal places."];
		Return[ errorReturn ]
	];    
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
	If[psb == pSum,
		Print[pSum, " is too close to the actual sum."];
		Print["Either use a smaller value, or increase the number of decimals (2nd parameter)."];
		Return[ errorReturn ]    
	];
	If[psb < pSum, Return[ errorReturn ] ];    
	diff = b - a;
	c = a + diff / 2;
	For[i = 1, i <= iLimit, i++,
		psc = kPartialSum[c, nDec] ;
		If[psc > pSum,
			b = c; psb = psc;
			,
			a = c; psa = psc;
		];
		diff = b - a;
		If[diff < 2, Break[] ];
		c = a + diff / 2;
	];    
	If[ (pSum <= psa) || (psb <= pSum),
		nDecimalsNeeded = -Floor[Log[10, Abs[psb - psa]]] + 5;
		nDecimalsNeeded = 5 * (1 + Floor[(nDecimalsNeeded - 1) / 5]);  
		If[nDecimalsNeeded > nDec,
			Print["More decimals are needed.  You should re-compute KempnerSum to at least ",
				nDecimalsNeeded, " decimals, then call kPartialSumThreshold again."] ,
			Print["More precision is needed.  You should re-compute KempnerSum to more decimals,\
 then call kPartialSumThreshold again."];
		];
		Return[ errorReturn ];
	];
	Return[ { a, b, psa, psb } ];
];
kPartialSumThreshold[pSumStr_String, nDecDefault_] := Block[
	{ pSum, inputStr2, decPtList, quoteList, nDecimalsInput, nDec2,
		errorReturn = { -1, -1, -1, -1 }
	},
	decPtList = StringPosition[pSumStr, "."];    
	quoteList = StringPosition[pSumStr, "`"];
	If[ (Length[decPtList] == 1) && (Length[quoteList] == 0),
		nDecimalsInput = StringLength[pSumStr] - decPtList[[1]][[1]] ;
		nDec2 = Max[nDecimalsInput + 2, nDecDefault];
		inputStr2 = pSumStr <> "``" <> ToString[nDec2] ;
		pSum = ToExpression[inputStr2];
		,
		pSum = ToExpression[pSumStr]
	];
	If[ (pSum == $Failed) || (NumericQ[pSum] == False),
		Print["Invalid input"];  
		Return[ errorReturn ]
	];
	kPartialSumThresholdX[pSum, nDecDefault]
];
kPartialSumThreshold[pSumStr_String] := Block[
	{ },
	kPartialSumThreshold[pSumStr, nDefaultDecimals]
];
kPartialSumThreshold[pSum0_?NumericQ] := Block[
	{ },
	kPartialSumThresholdX[pSum0, nDefaultDecimals]
];
kPartialSumThreshold[pSum0_?NumericQ, nDecDefault_] := Block[
	{ },
	kPartialSumThresholdX[pSum0, nDecDefault]
];
kSumGetT[inputList_?VectorQ, iBase_ : 10] := Block[
	{ lst },
	lst = convertInputListToStrings[inputList];
	If[Length[lst] == 0, Return[ {{0}} ] ];
	setT[lst, iBase]    
];    
kSumGetT[s_String, iBase_ : 10] := Block[
	{ stringList = { s } },
	kSumGetT[stringList, iBase]
] ;   
kSumGetT[i_Integer, iBase_ : 10] := Block[
	{ stringList = { ToString[i] } },
	kSumGetT[stringList, iBase]
] ;   
kSumShowA[] := A;    
kSumGetA[inputList_?VectorQ, iBase_ : 10] := Block[
	{ lst2, T, f, j, l, m, A, n },
	lst2 = convertInputListToStrings[inputList];
	If[Length[lst2] == 0, Return[ {{0}} ] ];
	T = setT[lst2, iBase];
	f[j_, l_, m_] := f[j, l, m] = If[T[[l, m]] == j, 1, 0] ;
	n = Dimensions[T][[1]] ;    
	A = (1 / iBase) Sum[ Table[f[j, l, m + 1], {j, n}, {l, n}] , {m, 0, iBase - 1} ] ;
	A    
];
kSumGetA[s_String, iBase_ : 10] := Block[
	{ stringList = {} },
	AppendTo[stringList, s];
	kSumGetA[stringList, iBase]
];
kSumGetA[i_Integer, iBase_ : 10] := Block[
	{ stringList = {} },
	AppendTo[stringList, ToString[i]];
	kSumGetA[stringList, iBase]
];
kSumTimeAndMemory[nDigits_] := Block[
	{ nd, t1, t0, tFast, mFast},
	nd = nDigits;
	If[nd < 50, nd = 50];
	t1 = Timing[ KempnerSum[9, 100] ][[1]];
	t0 = 3.647 ;
	tFast = .55227 + .017407 * nd + .00017173 * nd^2 + (3.3708 * 10^-7) * nd^3 ;
	mFast = (6.5614 * 10^6) + 2836.1 * nd + 18.432 * nd^2 + .0054213 * nd^3 ;
	Print["Estimated time and memory for KempnerSum[9, ", nd, "]:" ];
	Print[Round[tFast * t1 / t0], " seconds, ", Round[mFast / 1000000], " MBytes of memory"];
	Print["  Note: KempnerSum[99, ", nd, "], KempnerSum[999, ", nd, "] etc., will use more time and memory." ];
];
kSumSetDefaultDecimals[i_Integer] := Block[
	{ },
	If[i < 1, i = 1];
	If[nDefaultDecimals == i,
		Print["Default number of decimal places is now set to ", i] ,
		Print["Default number of decimal places has been changed from ", nDefaultDecimals, " to ", i]
	];
	nDefaultDecimals = i;
	nDefaultDecimals    
];
kSumShowDefaultDecimals := nDefaultDecimals;
SetAttributes[
	{
		KempnerSum, kSumFormatted, kPartialSum, kSumGetT, kSumGetA, kSumShowA,
		kSumTimeAndMemory, kSumSetDefaultDecimals, kSumShowDefaultDecimals,
		kPartialSumThreshold
	},
	{Protected, ReadProtected}
];
EndPackage[];