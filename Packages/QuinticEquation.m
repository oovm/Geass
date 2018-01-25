MetacyclicGroup::usage = "亚循环群";
GaloisGroup::usage = "计算五次方程的伽罗瓦群";
QuinticSolve::usage = "根式求解五次方程";
SolvableGroupQ::usage="判定是否是可解群,有可能会判定错误.";
QuinticEquation::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
QuinticEquation$Version="V1.1";
QuinticEquation$LastUpdate="2017-12-29";
Format[MetacyclicGroup[n_], TraditionalForm] :=	TraditionalForm["MetacyclicGroup[" <> ToString@n <> "]"];
QuinticSolve::notQE="`1` 不是五次多项式或者含有符号变量!";
QuinticSolve::noRad="该式无法用根式求解! 请先计算其伽罗瓦群.";
QuinticSolve::noSol="`1` 无法用现有方法求解! 或者其伽罗瓦群不是可解群.";
QuinticEquationQ[poly_,var_]:=Block[
	{coes=CoefficientList[poly,var]},
	And@@(NumericQ/@coes)&&Length@coes==6&&Exponent[poly,var]==5
];
QuinticSolve[poly_,var_]:=Block[
	{x,sol,solR},
	If[!QuinticEquationQ[poly,var],
		Message[QuinticSolve::notQE,TraditionalForm@poly];
		Return[Null]
	];
	solR=Solve[poly==0,var];
	If[solR[[1,1,2,0]]=!=Root,Return[solR]];
	sol=SolveQuinticEqn[poly==0,var];
	If[sol===$Failed||sol[[1,1,2,0]]===Root,
		Echo["快速根式变换失败, 尝试使用扩展算法.","Method: "],
		Return[sol]
	];
	sol=Simplify[QuinticRootToRadicals/@(solR[[All, 1, 2]])];
	If[sol[[1,0]]==Root,
		Message[QuinticSolve::noSol,TraditionalForm@poly];
		Return[solR]
	];
	List/@Thread[var->sol]
];
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
SolvableGroupQ[e_,x_]:=QuinticByRadicalsQ[e,x]//First;
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
SolveQuinticEqn[a_==b_,x_]:=Module[
	{temp},
	temp=QuinticByRadicalsQ[a-b,x];
	If[temp[[1]],
		If[temp[[2]]===$Failed,
			Return[Solve[a==b,x]],If[(#===Integer||#===Rational)&[Head[temp[[2]]]],
			tosolve[a-b,x,temp[[2]]],
			Message[QuinticSolve::noRad];
			Return[$Failed]]],Message[QuinticSolve::noRad];
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
QuinticRootToRadicals[root_Root] := Block[
	{a, b, c, d, e, f, h, p, q, r, s, t, u, v, w, x, z, g, F, A, B, G, H, L, M, P, Q, R, S},
	If[!TrueQ[Element[root, Algebraics]], Return[root]];
	With[{m = MinimalPolynomial[root, z]},
		If[!PolynomialQ[m, z] || Exponent[m, z] != 5,Return[root]];
		{f, e, d, c, b, a} = CoefficientList[m, z]];
	p = (5 a c - 2 b^2)/(5 a^2);
	q = (25 a^2 d - 15 a b c + 4 b^3)/(25 a^3);
	r = (125 a^3 e - 50 a^2 b d + 15 a b^2 c - 3 b^4)/(125 a^4);
	s = (3125 a^4 f - 625 a^3 b e + 125 a^2 b^2 d - 25 a b^3 c +
		4 b^5)/(3125 a^5);
	G = Select[
		Solve[{(p^2 + 12 r + 4 t) Discriminant[
			z^5 + p z^3 + q z^2 + r z + s,
			z] == (2 t^3 +
			8 t^2 r + (2 p q^2 - 6 p^2 r + 24 r^2 - 50 q s) t - 2 q^4 +
			13 p q^2 r - 16 (p^2 - 4 r) r^2 - 5 q (3 p^2 + 40 r) s +
			125 p s^2)^2,
			4 r^2 + 2 q (p q + 5 s + 2 u) + 5 x ==
				t^2 + 2 p^2 (3 r + t) + 2 p v,
			3 p^4 (2 r + t) + 5 p s (50 s + 9 u) +
				3 q^2 (18 p r + 5 p t + 4 v) +
				q (-20 s (7 r + 3 t) + 6 r u + p w) +
				2 (40 r^3 + 16 r^2 t + t^3 + 25 s w + 10 r x) ==
				14 q^4 + 28 p r v +
					p^2 (52 r^2 + 36 r t + q (3 p q + 41 s + 3 u) - 3 p v + 3 x),
			q^4 (30 r - 4 t) + t^4 + p^4 (22 r^2 - 6 q s + 4 r t) +
				q^2 (50 s^2 - 155 s u - 29 r v) +
				p^3 (-4 q^2 (4 r + t) + 9 s u + 4 r v) +
				p (q^3 (-132 s + 8 u) - 5 s (110 r s - 5 s t + 28 r u) +
					16 r^2 v + q (105 s v + 8 r w) -
					3 q^2 (14 r^2 + 5 r t - 3 x)) +
				p^2 (4 q^4 - 68 r^3 + 16 r^2 t +
					q (404 r s + 79 s t - 17 r u) - 4 q^2 v - 15 s w -
					19 r x) ==
				16 r^4 + 3 q^3 w + 20 r s (4 q t + 5 w) +
					4 r^2 (5 q s - 17 q u - 15 x) + 25 s (5 s v + 9 q x),
			625 s^3 (10 s + u) + q^5 (858 s + 20 u) +
				p^5 (198 s^2 + 5 q^2 (5 r + t) - 15 r v) +
				q^2 (5 s (2140 r s + 365 s t + 43 r u) - 12 r^2 v) +
				q^4 (-34 r^2 - 43 r t + 22 x) +
				8 r^2 (120 r^3 + 64 r^2 t + 25 s w + 30 r x) +
				p^3 (q^3 (181 s - 5 u) + s (-810 r s + 355 s t - 147 r u) +
					168 r^2 v - q (212 s v + 11 r w) -
					q^2 (22 r (13 r + 5 t) + 5 x)) +
				p^4 (-5 q^4 + q (-491 r s - 200 s t + 15 r u) + 5 q^2 v +
					18 s w + r (4 r (91 r + 50 t) + 15 x)) +
				p^2 (5 q^4 (19 r + 3 t) +
					2 q r (2060 r s + 864 s t - 45 r u) + 325 s^2 v +
					q^2 (3005 s^2 + 351 s u - 83 r v) + 3 q^3 w + 290 q s x -
					2 r (544 r^3 + 216 r^2 t + 265 s w + 76 r x)) ==
				15 p^6 r (2 r + t) + 2 (t^5 + 750 r s^2 v) +
					q^3 (620 s v + 41 r w) +
					q (2640 r^2 s t + 8 r^3 (780 s - 19 u) + 2375 s^2 w +
						700 r s x) +
					p (12 q^6 + 20 r s (-45 r s + 10 s t + 21 r u) +
						q^3 (4095 r s + 752 s t + 43 r u) - 10 q^4 v + 176 r^3 v +
						5 q s (635 s (5 s + u) - 312 r v) - 124 q r^2 w +
						1375 s^2 x - q^2 (612 r^3 + 220 r^2 t + 110 s w - 27 r x)),
			Element[t, Rationals]}, {t, u, v, w, x}],
		FreeQ[ConditionalExpression], 1];
	If[G == {}, Return[root], G /. Rule -> Set];
	g = -3 p^2 (-v + q^2) + 20 v r - 50 u s + 125 s^2 + 3 q^2 (-7 r + t) +
		p^3 (4 r + 3 t) + p (16 r^2 - q (u - 40 s) + 12 r t);
	h = 366 v q^3 - 402 q^5 - 748 u q^2 r + 440 w r^2 - 448 q r^3 -
		12 p^5 s - 275 w q s + 2100 v r s - 1925 q^2 r s - 4875 u s^2 -
		1875 s^3 + x (-65 p^2 q - 550 q r + 875 p s) + 524 q r^2 t -
		1040 q^2 s t + p^4 q (158 r + 85 t) +
		p^3 (85 v q - 85 q^3 + 4 u r - 1462 r s - 418 s t) +
		p (41 w q^2 - 298 v q r - 56 u r^2 + 5 q s (419 u + 35 s) +
			10 r s (290 r + 159 t) + q^3 (896 r + 419 t)) -
		p^2 (58 w r + 520 v s + q (73 u q - 142 r^2 + 159 q s + 440 r t));
	F = Sqrt[5 (40 x p - 120 w q + p^2 (-24 v + 40 q^2) + 100 v r +
		332 q^2 r - 300 u s + 125 s^2 + p^3 (-80 r - 24 t) + 24 q^2 t +
		p (88 u q + 160 r^2 - 480 q s + 100 r t))];
	A = Sqrt[5/2 (g + h/F)];
	B = 1/(A F) 5 (42 q^5 + 12 p^5 s + 3 p^4 q (14 r + 5 t) +
		q^2 (550 r s + 515 s t - 182 r u) - 6 q^3 v +
		p^3 (-15 q^3 + 492 r s + 213 s t - 4 r u + 15 q v) -
		50 s (25 s^2 - 60 s u + 22 r v) - 40 r^2 w + 650 q s w +
		p^2 (-3 q^2 (52 s + 9 u) + 195 s v - 22 r w +
			q (358 r^2 + 50 r t - 35 x)) -
		8 q r (29 r^2 + 23 r t + 25 x) +
		p (q^3 (-246 r + t) + 5 q s (565 s - 54 u) -
			4 r (350 r s + 235 s t - 24 r u) + 68 q r v + 19 q^2 w -
			250 s x));
	H = -1750 w q + p^2 (-600 v + 500 q^2) + 2500 v r - 7500 u s +
		3125 s^2 + q^2 (-700 r - 1150 t) + p^3 (-2000 r - 600 t) +
		p (1000 x + 1700 u q + 4000 r^2 - 6375 q s + 2500 r t);
	L = -25 x p - 9 v p^2 - 25 w q - 7 u p q - 7 p^2 q^2 - 60 v r +
		50 p^3 r + 128 q^2 r - 308 p r^2 + 525 u s - 145 p q s - 1000 s^2 -
		p^3 t + 11 q^2 t - 96 p r t;
	M = -125 x p + 67 v p^2 + 75 w q - 109 u p q - 79 p^2 q^2 - 420 v r +
		210 p^3 r + 496 q^2 r - 676 p r^2 + 1175 u s - 415 p q s -
		750 s^2 + 63 p^3 t + 27 q^2 t - 412 p r t; {A, B, F} =
		Select[{{A, B, F}, {-A, -B, F}, {B, -A, -F}, {-B, A, -F}},
			Apply[25 (2 u - p q - 5 s) + (L #1 + M #2)/g + H/#3 != 0 &], 1][[1]];
	P = 1/5 (5/4 (25 (2 u - p q - 5 s) + (L A + M B)/g + H/F))^(1/5);
	Q = -((4 p^2 q + 2 (36 q r + 7 q t - 5 w) +	p (-45 s + 4 u + F))/(10 P F));
	R = 1/(500 P^2) (-25 q + (25 (-40 r^2 - 35 q s + 2 p^2 (10 r + t) -
		22 q u + 2 p (q^2 + v) - 10 x))/F +
		1/g 2 (-105 q s A - 140 q s B + 4 r t (3 A + 4 B) +
			2 p q^2 (7 A + 11 B) + r^2 (76 A + 68 B) -
			2 p^2 (29 r A + 3 A t + 17 r B + 9 t B) + 23 q A u +
			14 q B u - 2 p (2 A + 11 B) v + 35 A x + 5 B x));
	S = 1/(500 P^3) (1/
		g (-80 r s A + 30 s A t + 60 r s B + 40 s t B +
		14 q^3 (A + 3 B) - 16 r g + 3 t g +
		2 p^2 (26 s A + 18 s B + g) + 8 r A u + 44 r B u -
		2 q (p (26 r A + 3 A t + 33 r B + 14 t B) + (A + 18 B) v) +
		p (-8 A w + 6 B w)) + (5 (8 p^3 q - 2 p q (13 r + t) +
		p^2 (-20 s + 8 u) +
		5 (14 q^3 + 10 r s - 5 s t - 10 r u - 2 q v)))/F);
	Select[{P + Q + R + S,
		(-1)^(4/5) P - (-1)^(1/5) Q + (-1)^(2/5) R - (-1)^(3/5)S,
		-(-1)^(3/5) P + (-1)^(2/5) Q + (-1)^(4/5) R - (-1)^(1/5)S,
		+(-1)^(2/5) P - (-1)^(3/5) Q - (-1)^(1/5) R + (-1)^(4/5)S,
		-(-1)^(1/5) P + (-1)^(4/5) Q - (-1)^(3/5) R + (-1)^(2/5)S
	} - b/(5 a), !TrueQ[# != root] &, 1][[1]]
];
End[];
SetAttributes[
	{MetacyclicGroup,GaloisGroup,QuinticSolve,SolvableGroupQ},
	{Protected,ReadProtected}
];