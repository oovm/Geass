(* ::Package:: *)
(* ::Title:: *)
(*Example(样板包)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template*)
(**)
(*Author:我是作者*)
(*Creation Date:我是创建日期*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["ExPrime`"];
ExampleFunction::usage = "这里应该填这个函数的说明,如果要换行用\"\\r\"\r就像这样";
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
ExampleFunction[2]="我就是个示例函数,什么功能都没有";


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;
SetAttributes[
	{ },
	{Protected,ReadProtected}
];
EndPackage[];
