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
GaloisGroup::usage = "";
SolveQuintic::usage = "";
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
(*GaloisGroup*)
GaloisGroup::"red" = "The quintic is reducible.";
GaloisGroup[expr_]:=Module[
	{var},
	var=Union[Select[Level[expr,{-1}],Head[#1]===Symbol&]];
	If[Length[var]==1&&Exponent[expr,var[[1]]]==5,galois[expr,var[[1]]],Return[$Failed]]];
galois[expr_,var_]:=If[Exponent[expr,var]==5&&And@@(#1===Integer||#1===Rational&)/@Head/@CoefficientList[expr,var],
	Module[{factor},
		If[Head[factor=Factor[expr]]===Times&&Length[Complement[(Exponent[#1,var]&)/@List@@factor,{0,5}]]>0,Message[GaloisGroup::red];
		Select[(Irreducible[#1,var,Exponent[#1,var]]&)/@List@@factor,#1=!=Null&],
			If[Head[factor=Factor[expr]]===Power,Message[GaloisGroup::red];
			Table[SymmetricGroup[1],{i,Exponent[expr,var]}],{Irreducible[expr,var,Exponent[expr,var]]}]]],
	$Failed
];
Irreducible[expr_,var_,n_]:=Module[
	{isfactorable,introot,roots},
	{isfactorable,introot,roots}=Resolvent[expr/Coefficient[expr,var,n],var];
	If[isfactorable,
		SolvableGroup[Sqrt[Discriminant[expr,var]],expr,var,introot,roots],
		If[IntegerQ[Sqrt[Discriminant[expr,var]]],AlternatingGroup[n],SymmetricGroup[n]],
		$Failed
	]
]/;n==5;
Irreducible[__]:=Null;
Resolvent[expr_,var_]:=Module[
	{roots,integralroot,temp},
	roots=List@@NRoots[expr==0,var,20]/._==(p_):>p;
	temp=Stabilizer/@Representatives[roots];
	integralroot=Position[Chop/@(Round[#1]-#1&)/@temp,_Integer];
	If[Length[integralroot]>=1,{True,Round[temp[[integralroot[[1,1]]]]],roots},{False,$Failed,$Failed}]
];
Representatives[{x1_,x2_,x3_,x4_,x5_}]:={
	{x1,x2,x3,x4,x5},
	{x2,x1,x3,x4,x5},
	{x3,x2,x1,x4,x5},
	{x4,x2,x3,x1,x5},
	{x5,x2,x3,x4,x1},
	{x1,x5,x3,x4,x2}
};
Stabilizer[{x1_,x2_,x3_,x4_,x5_}]:=x1^2*(x2*x5+x3*x4)+x2^2*(x1*x3+x4*x5)+x3^2*(x1*x5+x2*x4)+x4^2*(x1*x2+x3*x5)+x5^2*(x1*x4+x2*x3);
SolvableGroup[disc_,r__]:=If[!IntegerQ[disc],MetacyclicGroup[20],DihedralOrCyclic[r]];
DihedralOrCyclic[expr_,var_,introot_,roots_List]:=Module[
	{disc,p},
	Fnew[x1_,x2_,x3_,x4_,x5_]:=x1*x2^2+x2*x3^2+x3*x4^2+x4*x5^2+x5*x1^2;
	NewResolvent[{x1_,x2_,x3_,x4_,x5_}]:=(var-Fnew[x1,x2,x3,x4,x5])*(var-Fnew[x2,x1,x5,x4,x3]);
	p=Position[Round/@Stabilizer/@Representatives[roots],introot][[1,1]];
	disc=Discriminant[NewResolvent[Representatives[roots][[p]]],var];
	disc=disc/.{e_Real:>Round[e],e_Complex:>Round[e]};
	If[disc==0,Return[SingularCase[expr,var,roots,5]]];
	If[IntegerQ[Sqrt[disc]],CyclicGroup[5],DihedralGroup[10]]
];
SingularCase[expr_,var_,roots_,n_]:=Module[
	{newp,newroots,stabs,realr,p,disc},
	newp=Tschirnhaus[expr,var,n];newroots=newp/.({var->#1}&)/@roots;
	stabs=Stabilizer/@N[Representatives[newroots],32];
	realr=Select[Round[stabs],IntegerQ];
	If[Length[realr]==0,Return[$Failed]];
	p=Position[Round[stabs],First[realr]][[1,1]];
	disc=Discriminant[NewResolvent[Representatives[newroots][[p]]],var];
	If[IntegerQ[Sqrt[disc]],CyclicGroup[5],DihedralGroup[10]]
];
Tschirnhaus[expr_,var_,n_]:=Module[
	{z,randp,newp},
	randp=Sum[Random[Integer,{-1,0}]*z^(n-k-2),{k,0,n-2}];
	newp=Resultant[expr/.var->z,var-randp,z];
	If[FreeQ[PolynomialGCD[newp,D[newp,var]],var],newp,Tschirnhaus[expr,var,n]]
];

(* ::Subsubsection:: *)
(*功能块 1*)
SolvableQ::usage="SolvableQ[poly, x] yields True if poly, a quintic in
the variable x, is solvable in radicals, and yields False
otherwise.";
SolveQuintic::usage="RadicalQuinticSolve[lhs == rhs, x] attempts to solve the
reduced quintic lhs == rhs in the variable x in terms of
radicals.";
SolveQuintic::rad="This quintic is unsolvable in radicals.";
SolvableQ[e_,x_]:=QuinticByRadicalsQ[e,x]//First;
QuinticByRadicalsQ[expr_,x_]:=Module[
	{coefs,factor},
	If[Head[factor=Factor[expr]]===Times&&Length[Complement[Exponent[#,x]&/@(List@@factor),{0,5}]]>0,
		Return[{True,$Failed}]
	];
	coefs=CoefficientList[expr,x];
	If[And@@((#===Integer||#===Rational)&/@(Head/@coefs)),If[Last[coefs]=!=1,coefs=coefs/Last[coefs]];
	If[coefs[[-2]]=!=0,Return[QuinticByRadicalsQ[expr/.x->x-coefs[[-2]]/(5 coefs[[-1]]),x]]];
	factor=Factor[resolvent@@Append[Drop[coefs,-2]/Last[coefs],x]];
	If[Head[factor]===Times,
		If[MemberQ[Exponent[#,x]&/@(List@@factor),1],
			{True,(Cases[factor,p_. x+r_.][[1]])/.{p_. x+r_.:>-r/p}},
			{False,$Failed}
		],{False,$Failed}
	],{False,$Failed}
	]
]/;PolynomialQ[expr,x]&&Exponent[expr,x]==5;
resolvent[s_, r_, q_, p_, x_] :=x^6 + 8 r x^5 + (2 p q^2 - 6 p^2 r + 40 r^2 -
		50 q s) x^4 + (-2 q^4 + 21 p q^2 r - 40 p^2 r^2 + 160 r^3 -
		15 p^2 q s - 400 q r s + 125 p s^2) x^3 + (p^2 q^4 -
		6 p^3 q^2 r - 8 q^4 r + 9 p^4 r^2 + 76 p q^2 r^2 -
		136 p^2 r^3 + 400 r^4 - 50 p q^3 s + 90 p^2 q r s -
		1400 q r^2 s + 625 q^2 s^2 + 500 p r s^2) x^2 + (-2 p q^6 +
		19 p^2 q^4 r - 51 p^3 q^2 r^2 + 3 q^4 r^2 + 32 p^4 r^3 +
		76 p q^2 r^3 - 256 p^2 r^4 + 512 r^5 - 31 p^3 q^3 s -
		58 q^5 s + 117 p^4 q r s + 105 p q^3 r s + 260 p^2 q r^2 s -
		2400 q r^3 s - 108 p^5 s^2 - 325 p^2 q^2 s^2 + 525 p^3 r s^2 +
		2750 q^2 r s^2 - 500 p r^2 s^2 + 625 p q s^3 -
		3125 s^4) x + (q^8 - 13 p q^6 r + p^5 q^2 r^2 +
		65 p^2 q^4 r^2 - 4 p^6 r^3 - 128 p^3 q^2 r^3 + 17 q^4 r^3 +
		48 p^4 r^4 - 16 p q^2 r^4 - 192 p^2 r^5 + 256 r^6 -
		4 p^5 q^3 s - 12 p^2 q^5 s + 18 p^6 q r s + 12 p^3 q^3 r s -
		124 q^5 r s + 196 p^4 q r^2 s + 590 p q^3 r^2 s -
		160 p^2 q r^3 s - 1600 q r^4 s - 27 p^7 s^2 - 150 p^4 q^2 s^2 -
		125 p q^4 s^2 - 99 p^5 r s^2 - 725 p^2 q^2 r s^2 +
		1200 p^3 r^2 s^2 + 3250 q^2 r^2 s^2 - 2000 p r^3 s^2 -
		1250 p q r s^3 + 3125 p^2 s^4 - 9375 r s^4);
SolveQuintic[a_==b_,x_]:=Module[
	{temp},
	temp=QuinticByRadicalsQ[a-b,x];
	If[temp[[1]],
		If[temp[[2]]===$Failed,
			Return[Solve[a==b,x]],If[(#===Integer||#===Rational)&[Head[temp[[2]]]],
			tosolve[a-b,x,temp[[2]]],
			Message[SolveQuintic::rad];
			Return[$Failed]]],Message[SolveQuintic::rad];
	Return[$Failed]
	]
];
tosolve[pol_,x_,r_]:=Module[
	{coefs},
	coefs=CoefficientList[pol,x];
	If[Last[coefs]=!=1,coefs=coefs/Last[coefs]];
	If[coefs[[-2]]=!=0,coefs=CoefficientList[pol/.x->x-coefs[[-2]]/(5 coefs[[-1]]),x]];
	If[coefs[[3]]==coefs[[4]]==coefs[[5]]==0,
		canonicalQuintic[coefs[[2]],
			coefs[[1]],x,r,Sqrt[Discriminant[pol,x]]],
		$Failed]
];
canonicalQuintic[a_,b_,x_,r_,dis_]:=Module[
	{temp,l0,l1,l2,l3,l4},
	temp=Roots[x^2+(T1[a,b,r]+T2[a,b,r] dis) x+(T3[a,b,r]+T4[a,b,r] dis)==0,x];
	l1=temp[[1,2]]/.Sqrt[w_]:>I Sqrt[-w]/;Negative[w];
	l4=temp[[2,2]]/.Sqrt[w_]:>I Sqrt[-w]/;Negative[w];
	temp=Roots[x^2+(T1[a,b,r]-T2[a,b,r] dis) x+(T3[a,b,r]-T4[a,b,r] dis)==0,x];
	l2=temp[[1,2]]/.Sqrt[w_]:>I Sqrt[-w]/;Negative[w];
	l3=temp[[2,2]]/.Sqrt[w_]:>I Sqrt[-w]/;Negative[w];
	l0=Expand[-(l1+l2+l3+l4)];
	If[(Expand[(l1-l4)(l2-l3)-V[a,b,r] dis]/.Sqrt[w_]:>Sqrt[w//Expand])=!=0,temp=l1;
	l1=l4;
	l4=temp];
	Inner[List[Rule[#1,#2]]&,Table[x,{5}],LagrangeResolvent[{l0,l1,l2,l3,l4}],List]
];
LagrangeResolvent[{l0_,l1_,l2_,l3_,l4_}]:=Module[
	{prim1,prim2,prim3,prim4,r1,r2,r3,r4,p,q},
	prim1=(-q^2/Sqrt[5]+2^(1/2)*I*p)/4;
	prim2=(p^2/Sqrt[5]-2^(1/2)*I*q)/4;
	prim3=(p^2/Sqrt[5]+2^(1/2)*I*q)/4;
	prim4=(-q^2/Sqrt[5]-2^(1/2)*I*p)/4;
	r1=simp[{l0,l1,l2,l3,l4}.{1,prim1,prim2,prim3,prim4},p,q];
	r2=simp[{l0,l3,l1,l4,l2}.{1,prim1,prim2,prim3,prim4},p,q];
	r3=simp[{l0,l2,l4,l1,l3}.{1,prim1,prim2,prim3,prim4},p,q];
	r4=simp[{l0,l4,l3,l2,l1}.{1,prim1,prim2,prim3,prim4},p,q];
	r1=If[Positive@N[r1],r1^(1/5),-(-r1)^(1/5)];
	r2=If[Positive@N[r2],r2^(1/5),-(-r2)^(1/5)];
	r3=If[Positive@N[r3],r3^(1/5),-(-r3)^(1/5)];
	r4=If[Positive@N[r4],r4^(1/5),-(-r4)^(1/5)];
	prim1=(-1-5^(1/2)+2^(1/2)*I*(5-5^(1/2))^(1/2))/4;
	prim2=(-1+5^(1/2)-2^(1/2)*I*(5+5^(1/2))^(1/2))/4;
	prim3=(-1+5^(1/2)+2^(1/2)*I*(5+5^(1/2))^(1/2))/4;
	prim4=(-1-5^(1/2)-2^(1/2)*I*(5-5^(1/2))^(1/2))/4;
	{
		{r1,r2,r3,r4}.{1,1,1,1},
		{r1,r2,r3,r4}.{prim4,prim3,prim2,prim1},
		{r1,r2,r3,r4}.{prim3,prim1,prim4,prim2},
		{r1,r2,r3,r4}.{prim2,prim4,prim1,prim3},
		{r1,r2,r3,r4}.{prim1,prim2,prim3,prim4}
	}/5
];
simp[expr_,a_,b_]:=Collect[expr,
	{a,b}]/.{w_ a^2:>Expand[w (5-5^(1/2))],
	w_ b^2:>Expand[w (5+5^(1/2))]}/.{
	a->(5-5^(1/2))^(1/2),
	b->(5+5^(1/2))^(1/2)
}/.{Sqrt[w_]:>Sqrt[w//Expand]};
T1[a_,b_,r_]:=(512 a^5-15625 b^4+768 a^4 r+416a^3 r^2+112 a^2 r^3+24 a r^4+4 r^5)/(50 b^3);
T2[a_,b_,r_]:=(3840 a^5-78125b^4+4480a^4 r+2480a^3 r^2+760a^2 r^3+140a r^4+30r^5)/(512a^5 b+6250 b^5);
T3[a_,b_,r_]:=(-18880 a^5+781250b^4-34240a^4 r-21260a^3 r^2-5980a^2 r^3-1255 a r^4-240r^5)/(2 b^2);
T4[a_,b_,r_]:=(68800 a^5+25000a^4 r+11500a^3 r^2+3250a^2 r^3+375 a r^4+100 r^5)/(512 a^5+6250 b^4);
V[a_,b_,r_]:=(-1036800 a^5+48828125 b^4-2280000a^4 r-1291500a^3 r^2-399500a^2 r^3-76625a r^4-16100r^5)/(256a^5+3125b^4);
(* ::Subsubsection:: *)
(*功能块 1*)



(* ::Subsubsection:: *)
(*功能块 1*)


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];
