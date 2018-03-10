(* ::Package:: *)
(* ::Title:: *)
(*Isomerism(同分异构包)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template*)
(**)
(* ::Text:: *)
(*Author:我是作者*)
(*Creation Date:我是创建日期*)
(*Copyright: Mozilla Public License Version 2.0*)
(* ::Program:: *)
(*1.软件产品再发布时包含一份原始许可声明和版权声明。*)
(*2.提供快速的专利授权。*)
(*3.不得使用其原始商标。*)
(*4.如果修改了源代码，包含一份代码修改说明。*)
(**)
(* ::Text:: *)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["Isomerism`"];
ExampleFunction::usage = "这里应该填这个函数的说明,如果要换行用\"\\r\"\r就像这样";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
Isomerism::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Isomerism$Version="V1.0";
Isomerism$LastUpdate="2018-03-03";

A[z_]:=Evaluate@Normal@Fold[
	Series[1+z/6(#^3+3# ComposeSeries[#,z^2+O[z]^#2]+2 ComposeSeries[#,z^3+O[z]^#2]),{z,0,#2}]&,
	1+O[z],Range@Floor@n
];

A[z_]:=Evaluate@Normal@Fold[
	Series[1+z/3(#^3+2 ComposeSeries[#,z^3+O[z]^#2]),{z,0,#2}]&,
	1+O[z],Range@Floor[n/2]
];


(* ::Subsubsection:: *)
(*AlkaneCount*)
AlkaneSeries2D[n_Integer]:=Block[
	{A,P,Q,S,G},
	A[z_]:=Evaluate@Normal@Fold[
		Series[1+z/6(#^3+3# ComposeSeries[#,z^2+O[z]^#2]+2 ComposeSeries[#,z^3+O[z]^#2]),{z,0,#2}]&,
		1+O[z],Range@Floor[n/2]
	];
	P[z_]=z CycleIndexPolynomial[SymmetricGroup[4],Array[A[z^#]&,4]];
	Q[z_]=CycleIndexPolynomial[SymmetricGroup[2],Array[A[z^#]-1&,2]];
	S[z_]=A[z^2];
	Series[P[z]-Q[z]+S[z]-1,{z,0,n}]
];
AlkaneSeries3D[n_Integer]:=Block[
	{A,P,Q,S,G},
	A[z_]:=Evaluate@Normal@Fold[
		Series[1+z/3(#^3+2 ComposeSeries[#,z^3+O[z]^#2]),{z,0,#2}]&,
		1+O[z],Range@Floor[n/2]
	];
	P[z_]=z CycleIndexPolynomial[AlternatingGroup[4],Array[A[z^#]&,4]];
	Q[z_]=CycleIndexPolynomial[SymmetricGroup[2],Array[A[z^#]-1&,2]];
	S[z_]=A[z^2];
	Series[P[z]-Q[z]+S[z]-1,{z,0,n}]
];
AlkaneCount[n_,OptionsPattern[]]:=Block[
	{},
	Switch[OptionValue[]


	];
	Switch[Head@n,
		Integer,AlkaneSeries[n],
		List,AlkaneSeries[n]
	]
];


(* ::Subsubsection:: *)
(*功能块 2*)
GroupDirectProduct[g1_,g2_]:=With[
	{order1=GroupOrder@g1,order2=GroupOrder@g2,r,pd},
	r=Thread[Range[order2]->(order1+Range[order2])];
	pd=Outer[PermutationProduct,GroupElements[g1],GroupElements[g2]/.r];
	PermutationGroup@Flatten@pd
]

GroupDirectProduct[AlternatingGroup@5,CyclicGroup@2]
FiniteGroupData[{"DirectProduct",
	{
		{"AlternatingGroup",5},
		{"CyclicGroup",2}
	}
},"PermutationGroupRepresentation"]

(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;
SetAttributes[
	{ },
	{Protected,ReadProtected}
];
EndPackage[];
