(* ::Package:: *)
(* ::Title:: *)
(*QuinticEquation(样板包)*)
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
BeginPackage["QuinticEquation`"];
ExampleFunction::usage = "这里应该填这个函数的说明,如果要换行用\"\\r\"\r就像这样";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
QuinticEquation::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
QuinticEquation$Version="V1.0";
QuinticEquation$LastUpdate="2016-11-11";
(* ::Subsubsection:: *)
(*GaloisGroupQuintic*)
GaloisGroupQuintic[expr_]:=Module[
	{var},
	var=Union[Select[Level[expr,{-1}],Head[#1]===Symbol&]];
	If[Length[var]==1&&Exponent[expr,var[[1]]]==5,galois[expr,var[[1]]],Return[$Failed]]
];
galois[expr_,var_]:=If[Exponent[expr,var]==5&&And@@(#1===Integer||#1===Rational&)/@Head/@CoefficientList[expr,var],
	Module[{factor},
		If[Head[factor=Factor[expr]]===Times&&Length[Complement[(Exponent[#1,var]&)/@List@@factor,{0,5}]]>0,Message[GaloisGroup::red];
		Select[(Irreducible[#1,var,Exponent[#1,var]]&)/@List@@factor,#1=!=Null&],
			If[Head[factor=Factor[expr]]===Power,Message[GaloisGroup::red];
			Table[SymmetricGroup[1],{i,Exponent[expr,var]}],{Irreducible[expr,var,Exponent[expr,var]]}]]
	],
	$Failed
];
Irreducible[expr_,var_,n_]:=Module[
	{isFactorable,intRoot,roots},
	{isFactorable,intRoot,roots}=Resolvent[expr/Coefficient[expr,var,n],var];
	If[isFactorable,
		SolvableGroup[Sqrt[Discriminant[expr,var]],expr,var,intRoot,roots],
		If[IntegerQ[Sqrt[Discriminant[expr,var]]],
			AlternatingGroup[n],
			SymmetricGroup[n]],
		$Failed]
]/;n==5;
Irreducible[__]:=Null;
Resolvent[expr_,var_]:=Module[
	{roots,integralRoot,temp},
	roots=List@@NRoots[expr==0,var,20]/._==(p_):>p;
	temp=Stabilizer/@Representatives[roots];
	integralRoot=Position[Chop/@(Round[#1]-#1&)/@temp,_Integer];
	If[Length[integralRoot]>=1,
		{True,Round[temp[[integralRoot[[1,1]]]]],roots},
		{False,$Failed,$Failed}]
];
Representatives[{x1_,x2_,x3_,x4_,x5_}]:={
	{x1,x2,x3,x4,x5},
	{x2,x1,x3,x4,x5},
	{x3,x2,x1,x4,x5},
	{x4,x2,x3,x1,x5},
	{x5,x2,x3,x4,x1},
	{x1,x5,x3,x4,x2}
};
Stabilizer[{x1_,x2_,x3_,x4_,x5_}]:=x1^2*(x2*x5+x3*x4)
	+x2^2*(x1*x3+x4*x5)
	+x3^2*(x1*x5+x2*x4)
	+x4^2*(x1*x2+x3*x5)
	+x5^2*(x1*x4+x2*x3);
SolvableGroup[disc_,r__]:=If[!IntegerQ[disc],MetacyclicGroup[20],DihedralOrCyclic[r]];
DihedralOrCyclic[expr_,var_,intRoot_,roots_List]:=Module[
	{disc,p},
	fNew[x1_,x2_,x3_,x4_,x5_]:=x1*x2^2+x2*x3^2+x3*x4^2+x4*x5^2+x5*x1^2;
	NewResolvent[{x1_,x2_,x3_,x4_,x5_}]:=(var-fNew[x1,x2,x3,x4,x5])*(var-fNew[x2,x1,x5,x4,x3]);
	p=Position[Round/@Stabilizer/@Representatives[roots],intRoot][[1,1]];
	disc=Discriminant[NewResolvent[Representatives[roots][[p]]],var];
	disc=disc/.{e_Real:>Round[e],e_Complex:>Round[e]};
	If[disc==0,Return[SingularCase[expr,var,roots,5]]];
	If[IntegerQ[Sqrt[disc]],CyclicGroup[5],DihedralGroup[10]]
];
SingularCase[expr_,var_,roots_,n_]:=Module[
	{newp,newRoots,stabs,realr,p,disc},
	newp=Tschirnhaus[expr,var,n];
	newRoots=newp/.({var->#1}&)/@roots;
	stabs=Stabilizer/@N[Representatives[newRoots],32];
	realr=Select[Round[stabs],IntegerQ];
	If[Length[realr]==0,Return[$Failed]];
	p=Position[Round[stabs],First[realr]][[1,1]];
	disc=Discriminant[NewResolvent[Representatives[newRoots][[p]]],var];
	If[IntegerQ[Sqrt[disc]],CyclicGroup[5],DihedralGroup[10]]
];
Tschirnhaus[expr_,var_,n_]:=Module[
	{z,randp,newp},
	randp=Sum[RandomInteger[{-1,0}]*z^(n-k-2),{k,0,n-2}];
	newp=Resultant[expr/.var->z,var-randp,z];
	If[FreeQ[PolynomialGCD[newp,D[newp,var]],var],
		newp,
		Tschirnhaus[expr,var,n]
	]
];

(* ::Subsubsection:: *)
(*功能块 1*)

(* ::Subsubsection:: *)
(*功能块 1*)


(* ::Subsubsection:: *)
(*HermiteQuinticSolve*)
HermiteQuinticSolve[rho_,t_]:=Module[
	{k,b,q},
	k=Tan[(1/4)*ArcSin[16/(25*Sqrt[5]*rho^2)]]//Simplify;
	b=((k^2)^(1/8)*If[Re[rho]==0,-Sign[Im[rho]],Sign[Re[rho]]])/(2*5^(3/4)*Sqrt[k]*Sqrt[1-k^2]);
	q=EllipticNomeQ[k^2];({t->#1}&)/@{
		b*((-1)^(3/4)*(InverseEllipticNomeQ[q^(1/5)/E^((1/5)*(2*I)*Pi)]^(1/8)
			+I*InverseEllipticNomeQ[E^((1/5)*(2*I)*Pi)*q^(1/5)]^(1/8))*(InverseEllipticNomeQ[q^(1/5)/E^((1/5)*(4*I)*Pi)]^(1/8)
			+InverseEllipticNomeQ[E^((1/5)*(4*I)*Pi)*q^(1/5)]^(1/8))*((q^(5/8)*InverseEllipticNomeQ[q^5]^(1/8))/(q^5)^(1/8)
			+InverseEllipticNomeQ[q^(1/5)]^(1/8))),
		b*(E^((1/4)*(3*I)*Pi)*InverseEllipticNomeQ[E^((1/5)*(2*I)*Pi)*q^(1/5)]^(1/8)
			-InverseEllipticNomeQ[q^(1/5)]^(1/8))*(InverseEllipticNomeQ[q^(1/5)/E^((1/5)*(2*I)*Pi)]^(1/8)/E^((1/4)*(3*I)*Pi)
			+I*InverseEllipticNomeQ[E^((1/5)*(4*I)*Pi)*q^(1/5)]^(1/8))*(I*InverseEllipticNomeQ[q^(1/5)/E^((1/5)*(4*I)*Pi)]^(1/8)
			+(q^(5/8)*InverseEllipticNomeQ[q^5]^(1/8))/(q^5)^(1/8)),
		b*(InverseEllipticNomeQ[q^(1/5)/E^((1/5)*(2*I)*Pi)]^(1/8)/E^((1/4)*(3*I)*Pi)
			-I*InverseEllipticNomeQ[q^(1/5)/E^((1/5)*(4*I)*Pi)]^(1/8))*(-InverseEllipticNomeQ[q^(1/5)]^(1/8)
			-I*InverseEllipticNomeQ[E^((1/5)*(4*I)*Pi)*q^(1/5)]^(1/8))*((q^(5/8)*InverseEllipticNomeQ[q^5]^(1/8))/(q^5)^(1/8)
			+E^((1/4)*(3*I)*Pi)*InverseEllipticNomeQ[E^((1/5)*(2*I)*Pi)*q^(1/5)]^(1/8)),
		b*(InverseEllipticNomeQ[q^(1/5)]^(1/8)
			-I*InverseEllipticNomeQ[q^(1/5)/E^((1/5)*(4*I)*Pi)]^(1/8))*((-E^((1/4)*(3*I)*Pi))*InverseEllipticNomeQ[E^((1/5)*(2*I)*Pi)*q^(1/5)]^(1/8)
			-I*InverseEllipticNomeQ[E^((1/5)*(4*I)*Pi)*q^(1/5)]^(1/8))*(InverseEllipticNomeQ[q^(1/5)/E^((1/5)*(2*I)*Pi)]^(1/8)/E^((1/4)*(3*I)*Pi)
			+(q^(5/8)*InverseEllipticNomeQ[q^5]^(1/8))/(q^5)^(1/8)),
		b*(InverseEllipticNomeQ[q^(1/5)]^(1/8)
			-InverseEllipticNomeQ[q^(1/5)/E^((1/5)*(2*I)*Pi)]^(1/8)/E^((1/4)*(3*I)*Pi))*(I*InverseEllipticNomeQ[q^(1/5)/E^((1/5)*(4*I)*Pi)]^(1/8)
			-InverseEllipticNomeQ[E^((1/5)*(2*I)*Pi)*q^(1/5)]^(1/8)*E^((1/4)*(3*I)*Pi))*((InverseEllipticNomeQ[q^5]^(1/8)*q^(5/8))/(q^5)^(1/8)
			-I*InverseEllipticNomeQ[E^((1/5)*(4*I)*Pi)*q^(1/5)]^(1/8))
	}
];



(* ::Subsubsection:: *)
(*功能块 1*)


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];
