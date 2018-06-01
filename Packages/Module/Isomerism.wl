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
MolecularDegree::usage = "";
MolecularQ::usage = "";
MolecularFind::usage = "";
MolecularShow::usage = "";
MolecularShow3D::usage = "";
AlkaneSeries2D::usage = "临时";
AlkaneSeries3D::usage = "临时";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
Isomerism::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Isomerism$Version = "V1.0";
Isomerism$LastUpdate = "2018-03-03";

(* ::Subsubsection:: *)
(*AlkaneCount*)
AlkaneSeries2D[n_Integer] := Block[
	{A, P, Q, S, G},
	A[z_] := Evaluate@Normal@Fold[
		Series[1 + z / 6(#^3 + 3# ComposeSeries[#, z^2 + O[z]^#2] + 2 ComposeSeries[#, z^3 + O[z]^#2]), {z, 0, #2}]&,
		1 + O[z], Range@Floor[n / 2]
	];
	P[z_] = z CycleIndexPolynomial[SymmetricGroup[4], Array[A[z^#]&, 4]];
	Q[z_] = CycleIndexPolynomial[SymmetricGroup[2], Array[A[z^#] - 1&, 2]];
	S[z_] = A[z^2];
	Series[P[z] - Q[z] + S[z] - 1, {z, 0, n}]
];
AlkaneSeries3D[n_Integer] := Block[
	{A, P, Q, S, G},
	A[z_] := Evaluate@Normal@Fold[
		Series[1 + z / 3(#^3 + 2 ComposeSeries[#, z^3 + O[z]^#2]), {z, 0, #2}]&,
		1 + O[z], Range@Floor[n / 2]
	];
	P[z_] = z CycleIndexPolynomial[AlternatingGroup[4], Array[A[z^#]&, 4]];
	Q[z_] = CycleIndexPolynomial[SymmetricGroup[2], Array[A[z^#] - 1&, 2]];
	S[z_] = A[z^2];
	Series[P[z] - Q[z] + S[z] - 1, {z, 0, n}]
];
AlkaneCount[n_, OptionsPattern[]] := Block[
	{},
	Switch[OptionValue[]


	];
	Switch[Head@n,
		Integer, AlkaneSeries[n],
		List, AlkaneSeries[n]
	]
];


(*
GroupDirectProduct[g1_, g2_] := With[
	{order1 = GroupOrder@g1, order2 = GroupOrder@g2, r, pd},
	r = Thread[Range[order2] -> (order1 + Range[order2])];
	pd = Outer[PermutationProduct, GroupElements[g1], GroupElements[g2] /. r];
	PermutationGroup@Flatten@pd
];

GroupDirectProduct[AlternatingGroup@5, CyclicGroup@2];
FiniteGroupData[{"DirectProduct",
	{
		{"AlternatingGroup", 5},
		{"CyclicGroup", 2}
	}
}, "PermutationGroupRepresentation"];
*)


(* ::Subsubsection:: *)
(*功能块 2*)
MolecularDegree[c_Integer, h_Integer, n_Integer] := (2 * c + 2 - h + n) / 2; (*Degrees of Unsaturation*)
IsoIterator[c_, h_, o_, n_, OptionsPattern[]] := Block[
	{sol = {}, deg = MolecularDegree[c, h, n], NextMolecular},
	If[!IntegerQ@deg, Return@Failure];
	NextMolecular[nC_Integer, nH_Integer, nO_Integer, nN_Integer, curM_, cMap_, tC_, tH_, tO_, tN_, dg_, dU_] := Module[
		{tM, sortM, fB, fB2},
		If[Length[sol] < $RecursionLimit,
			If[nC == nH == nO == nN == 0 && Total[curM[[All, 1]]] == 0,
				sol = Append[sol, cMap],
				If[nC + nH + nO + nN > 0 && Total[curM[[All, 1]]] > 0,
					sortM = Reverse[Sort[curM]];
					fB := sortM[[1, 1]];
					If[fB >= 1 && nC > 0, (*C1*)
						tM = Append[sortM, {3, {"C", tC}}];
						tM[[1, 1]] -= 1;
						NextMolecular[nC - 1, nH, nO, nN, tM, Append[cMap, UndirectedEdge[{"C", tC}, sortM[[1, 2]]]], tC + 1, tH, tO, tN, dg, dU]
					];
					If[fB >= 1 && nO > 0, (*O1*)
						tM = Append[sortM, {1, {"O", tO}}];
						tM[[1, 1]] -= 1;
						NextMolecular[nC, nH, nO - 1, nN, tM, Append[cMap, UndirectedEdge[{"O", tO}, sortM[[1, 2]]]], tC, tH, tO + 1, tN, dg, dU]
					];
					If[fB >= 1 && nH > 0, (*H1*)
						tM = Append[sortM, {0, {"H", tH}}];
						tM[[1, 1]] -= 1;
						NextMolecular[nC, nH - 1, nO, nN, tM, Append[cMap, UndirectedEdge[{"H", tH}, sortM[[1, 2]]]], tC, tH + 1, tO, tN, dg, dU];
					];
					If[fB >= 1 && nN > 0, (*N1*)
						tM = Append[sortM, {2, {"N", tN}}];
						tM[[1, 1]] -= 1;
						NextMolecular[nC, nH, nO, nN - 1, tM, Append[cMap, UndirectedEdge[{"N", tN}, sortM[[1, 2]]]], tC, tH, tO, tN + 1, dg, dU]
					];
					If[fB >= 1 && Length[sortM] >= 2 && sortM[[2, 1]] >= 1 && dg - dU >= 1,
						tM = sortM;
						tM[[1, 1]] -= 1;
						tM[[2, 1]] -= 1;
						NextMolecular[nC, nH, nO, nN, tM, Append[cMap, UndirectedEdge[sortM[[1, 2]], sortM[[2, 2]]]], tC, tH, tO, tN, dg, dU + 1]
					];
					If[fB >= 2 && nC > 0 && dg - dU >= 1, (*C2*)
						tM = Append[sortM, {2, {"C", tC}}];
						tM[[1, 1]] -= 2;
						NextMolecular[nC - 1, nH, nO, nN, tM, Append[Append[cMap, UndirectedEdge[{"C", tC}, sortM[[1, 2]]]], UndirectedEdge[{"C", tC}, sortM[[1, 2]]]], tC + 1, tH, tO, tN, dg, dU + 1]
					];
					If[fB >= 2 && nO > 0 && dg - dU >= 1, (*O2*)
						tM = Append[sortM, {0, {"O", tO}}];
						tM[[1, 1]] -= 2;
						NextMolecular[nC, nH, nO - 1, nN, tM, Append[Append[cMap, UndirectedEdge[{"O", tO}, sortM[[1, 2]]]], UndirectedEdge[{"O", tO}, sortM[[1, 2]]]], tC, tH, tO + 1, tN, dg, dU + 1]
					];
					If[fB >= 2 && nN > 0 && dg - dU >= 1, (*N2*)
						tM = Append[sortM, {1, {"N", tN}}];
						tM[[1, 1]] -= 2;
						NextMolecular[nC, nH, nO, nN - 1, tM, Append[Append[cMap, UndirectedEdge[{"N", tN}, sortM[[1, 2]]]], UndirectedEdge[{"N", tN}, sortM[[1, 2]]]], tC, tH, tO, tN + 1, dg, dU + 1]
					];
					If[fB >= 3 && nC > 0 && dg - dU >= 2, (*C3*)
						tM = Append[sortM, {1, {"C", tC}}];
						tM[[1, 1]] -= 3;
						NextMolecular[nC - 1, nH, nO, nN, tM, Append[Append[Append[cMap, UndirectedEdge[{"C", tC}, sortM[[1, 2]]]], UndirectedEdge[{"C", tC}, sortM[[1, 2]]]], UndirectedEdge[{"C", tC}, sortM[[1, 2]]]], tC + 1, tH, tO, tN, dg, dU + 2]
					];
					If[fB >= 3 && nN > 0 && dg - dU >= 2, (*N3*)
						tM = Append[sortM, {0, {"N", tN}}];
						tM[[1, 1]] -= 3;
						NextMolecular[nC, nH, nO, nN - 1, tM, Append[Append[Append[cMap, UndirectedEdge[{"N", tN}, sortM[[1, 2]]]], UndirectedEdge[{"N", tN}, sortM[[1, 2]]]], UndirectedEdge[{"N", tN}, sortM[[1, 2]]]], tC, tH, tO, tN + 1, dg, dU + 2]
					];
				]
			]
		]
	];
	NextMolecular[c - 1, h, o, n, {{4, {"C", 1}}}, {}, 2, 1, 1, 1, deg, 0];
	Return[sol]
];
MolecularQ[nC_, nH_, nO_, nN_] := Module[
	{ deg = MolecularDegree[nC, nH, nN]},
	And @@ {
		IntegerQ[deg] ,
		deg >= 0 ,
		nH + nO + nN >= 1 ,
		nC + nH + nO + nN >= 1 ,
		!(nC == 1 && ((nO != 2 && nH == nN == 0) || (nN > 1 && nH == nO == 0)))
	}
];
MolecularFind[c_Integer, h_Integer, o_Integer : 0, n_Integer : 0] := Block[
	{raw, modi, all, pos },
	If[!MolecularQ[c, h, o, n, "Step" -> OptionValue["Step"]], Print["Molecular not Exist!"]];
	raw = IsoIterator[nC, nH, nO];
	modi = CanonicalGraph /@ Graph /@ DeleteDuplicates /@ Map[Sort, raw, 2];
	all = DeleteDuplicates[modi];
	pos = Flatten[Table[Position[mod, all[[i]], 1, 1], {i, 1, Length[all]}]];
	Return[raw[[pos]]]
];

MolecularShow = Graph[#, EdgeStyle -> Darker@Green,
	GraphLayout -> {"SpringEmbedding", "EnergyControl" -> "NonMonotonic"},
	VertexSize -> {{"C", _} -> 0.7, {"O", _} -> 0.55, {"H", _} -> 0.35, {"N", _} -> 0.6},
	VertexStyle -> {{"C", _} -> Lighter[Black], {"O", _} -> Lighter[Red], {"H", _} -> LightBlue, {"N", _} -> Lighter[Blue]}
]&;
MolecularShow3D = Graph3D[#, EdgeStyle -> Darker@Green,
	GraphLayout -> {"SpringEmbedding", "EnergyControl" -> "NonMonotonic"},
	VertexSize -> {{"C", _} -> 0.7, {"O", _} -> 0.55, {"H", _} -> 0.35, {"N", _} -> 0.6},
	VertexStyle -> {{"C", _} -> Lighter[Black], {"O", _} -> Lighter[Red], {"H", _} -> LightBlue, {"N", _} -> Lighter[Blue]}
]&;


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
EndPackage[];