(* ::Package:: *)
(* ::Title:: *)
(*ExPrime(特殊素数包)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template(v1.3)*)
(**)
(*Author:GalAster*)
(*Creation Date: 2017-12-31*)
(*Copyright: Mozilla Public License Version 2.0*)
(* ::Program:: *)
(*1.软件产品再发布时包含一份原始许可声明和版权声明。*)
(*2.提供快速的专利授权。*)
(*3.不得使用其原始商标。*)
(*4.如果修改了源代码，包含一份代码修改说明。*)
(* ::Text:: *)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["ExPrime`"];
DayPrimes::usage = "
	DayPrimes[year],给出当年的日期质数.\n
	DayPrimes[year1,year2],从year1到year2所有的日期质数
";
SecPrimes::usage = "SecPrimes[day],给出当天所有的质数秒";
MultPrime::usage = "PlusPrime[n]生成n以内所有可以由两个素数相乘得到的整数";
PlusPrime::usage = "PlusPrime[n]生成n以内所有可以由两个素数相加得到的整数";
ManyPrime::usage = "ManyPrime[n]生成n以内所有可以由s个素数相乘得到的整数";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
ExPrime::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
ExPrime$Version="V1.0";
ExPrime$LastUpdate="2018-01-01";
(* ::Subsubsection:: *)
(*DayPrimes/SecPrimes*)
DayPrimes[st_]:=DayPrimes[st,st];
DayPrimes[st_,ft_]:=Block[
	{d,days},
	d=DateRange[DateObject[{ToExpression@st, 1,1}],DateObject[{ToExpression@ft,12,31}]];
	days=DateString[#,{"Year","Month","Day"}]&/@d;
	Select[ToExpression/@days,PrimeQ]
];
SecPrimes[day_]:=SecPrimes[day]=Block[
	{h,m,s,sec},
	h=StringJoin[ToString/@#]&/@Table[IntegerDigits[i,10,2],{i,0,23}];
	m=s=StringJoin[ToString/@#]&/@Table[IntegerDigits[i,10,2],{i,0,59}];
	sec=ToString[day]<>#&/@StringJoin@@@Tuples[{h,m,s}];
	Select[ToExpression/@sec,PrimeQ]
];



(* ::Subsubsection:: *)
(*功能块 2*)
MultPrime[n_]:=Union@@Table[p*TakeWhile[Prime[Range[PrimePi[n]]],p*#1<n&],{p,TakeWhile[l,#1<Sqrt[n]&]}];
PlusPrime[n_]:=Union@@Table[p+TakeWhile[Prime[Range[PrimePi[n]]],p*#1<n&],{p,TakeWhile[l,#1<Sqrt[n]&]}];
ManyPrime[n_,s_]:=Select[Range[n],PrimeOmega[#1]==s&];


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;
SetAttributes[
	{ },
	{Protected,ReadProtected}
];
EndPackage[];
