(* ::Package:: *)
(* ::Title:: *)
(*Tschirnhaus(契恩豪斯变换)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template*)
(**)
(*Author: 酱紫君*)
(*Creation Date:2012-12-27*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["Tschirnhaus`"];
PrincipalTransformEqn::usage = "";
BringJerrardTransformEqn::usage = "";
CanonicalTransformEqn::usage = "";
PrincipalTransform::usage = "";
BringJerrardTransform::usage = "";
CanonicalTransform::usage = "";
QuinticSolveHermite::usage = "";
HypergeometricSolve::usage = "";
Tschirnhaus2::usage = "";
Tschirnhaus3::usage = "";
Tschirnhaus4::usage = "";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
Tschirnhaus::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Tschirnhaus$Version="V1.0";
Tschirnhaus$LastUpdate="2017-12-29";
(* ::Subsubsection:: *)
(*TransformsEqn*)
Psi[q_,x_,n_Integer]:=Psi[q,x,n]=-((n*Coefficient[q,x,5-n]+Sum[Psi[q,x,n-j]*Coefficient[q,x,5-j],{j,n-1}])/Coefficient[q,x,5]);
PrincipalTransformEqn[(p_)==0,x_,y_]:=Module[
	{alpha,beta,xi},
	{alpha,beta}={alpha,beta}/.Last[
		Solve[{5*(xi^2+alpha*xi+beta)==0, Expand[5*(xi^2+alpha*xi+beta)^2]==0}/.xi^(n_.)->(1/5)*Psi[p,x,n],{alpha,beta}
		]
	];
	{Evaluate[#1^2+alpha*#1+beta]&,
		y^5-Sum[(y^(5-j)*Collect[(xi^2+alpha*xi+beta)^j+4*beta^j,xi])/j/.xi^(n_.)->Psi[p,x,n],{j,3,5}]==0
	}
]/;MatchQ[CoefficientList[p,x],{_,_,_,_,_?(#1=!=0&),_}];
BringJerrardTransformEqn[(p_)==0,y_,z_]:=Module[
	{alpha,beta,gamma,delta,epsilon,kappa,lambda,mu,nu,psi,xi,zeta,a,b,c,g,h},
	psi[t_]:=Expand[5*t]/.xi^(n_.)->(1/5)*Psi[p,y,n];
	{a,b,c}=(Psi[p,y,#1]&)/@{3,4,5};
	g=5*a*xi^3-5*b*xi^2-a^2;h=5*a*xi^4-5*c*xi^2-a*b;
	{lambda,mu,nu}=psi/@{g^2,2*g*h,h^2};
	kappa=-(mu/(2*lambda))+Sqrt[mu^2/(4*lambda^2)-nu/lambda];
	delta=Solve[psi[(zeta*xi+kappa*g+h)^3]==0,zeta][[1,1,2]];
	alpha=5*a;beta=5*a*kappa;gamma=-5*b*kappa-5*c;
	epsilon=(-a^2)*kappa-a*b;
	{Evaluate[alpha*#1^4+beta*#1^3+gamma*#1^2+delta*#1+epsilon]&,
		z^5-Sum[(z^(5-j)*Collect[psi[(delta*xi+kappa*g+h)^j],xi])/j,{j,4,5}]==0
	}
]/;MatchQ[CoefficientList[p,y],{_,_,_,0,0,_}];
CanonicalTransformEqn[z_^5+e_. z_+f_==0,z_,t_]:={#/(-e)^(1/4)&,t^5-t+f/(-e)^(5/4)==0};
(* ::Subsubsection:: *)
(*Transforms*)
TschirnhausTransform::notPT="`1` 不满足契恩豪斯主变换的使用条件!";
TschirnhausTransform::notBJ="`1`不满足布林-杰拉德变换的使用条件!";
TschirnhausTransform::notCT="`1`不满足规范变换的使用条件!";
PrincipalTransform5[p_?PolynomialQ,x_,y_]:=Block[
	{mQ,trans,eqn},
	mQ=!MatchQ[CoefficientList[p,x],{_,_,_,_,_?(#=!=0&),_}];
	If[mQ,Return@Message[TschirnhausTransform::notPT,p]];
	{trans,eqn}=Quiet@PrincipalTransformEqn[p==0,x,y]//Chop;
	Echo[TraditionalForm[y==trans[x]],"Traceback:"];
	Quiet@Simplify[First@eqn,Reals,TimeConstraint -> 0.1]
];
BringJerrardTransform[p_?PolynomialQ,x_,y_]:=Block[
	{mQ,trans,eqn},
	mQ=!MatchQ[CoefficientList[p,x],{_,_,_,0,0,_}];
	If[mQ,Return@Message[TschirnhausTransform::notBJ,p]];
	{trans,eqn}=Quiet@BringJerrardTransformEqn[p==0,x,y]//Chop;
	Echo[TraditionalForm[y==trans[x]],"Traceback:"];
	Quiet@Simplify[First@eqn,Reals,TimeConstraint -> 0.1]
];
CanonicalTransform5[p_?PolynomialQ,x_,y_]:=Block[
	{mQ,trans,eqn},
	mQ=!MatchQ[CoefficientList[p,x],{_,_,0,0,0,_}];
	If[mQ,Return@Message[TschirnhausTransform::notCT,p]];
	{trans,eqn}=Quiet@CanonicalTransformEqn[p==0,x,y]//Chop;
	Echo[TraditionalForm[y==trans[x]],"Traceback:"];
	Quiet@Simplify[First@eqn,Reals,TimeConstraint -> 0.1]
];
(* ::Subsubsection:: *)
(*Tschirnhaus*)
TschirnhausTransform::done="`1` 已经是布林-杰拉德正规式!";

Tschirnhaus2[poly_,var_]:=Block[
	{x=var,all1,all2,t},
	If[MatchQ[CoefficientList[poly,var],{_,-1,1}],Message[TschirnhausTransform::done,poly];
	Return@poly];
	all1=x/.Solve[poly==0,x];
	all2=x/.Solve[x^2-x+t==0,x];
	x^2-x+t/.First@Solve[First@all1==First@all2,t]];
Tschirnhaus3[poly_,var_]:=Block[
	{x=var,all1,all2,t,tt},
	If[MatchQ[CoefficientList[poly,var],{_,-1,0,1}],Message[TschirnhausTransform::done,poly];
	Return@poly];
	all1=x/.Solve[1 x^3+3 x^2+2 x+4==0,x];
	all2=x/.Solve[x^3-x+t==0,x];
	tt=Flatten[Solve[#,t]&/@RootReduce@Thread[First@all1==all2]];
	x^3-x+t/.First@tt//RootReduce//ToRadicals
];
Tschirnhaus4[poly_,var_]:=Block[
	{res,elim,x=var,y,m,n},
	If[MatchQ[CoefficientList[poly,var],{_,-1,0,0,1}],Message[TschirnhausTransform::done,poly];
	Return@poly];
	res=Resultant[poly,y-(x^2+m x+n),x];
	elim=Solve[Thread[CoefficientList[res,y][[3;;4]]==0],{m,n}];
	{f,e}=CoefficientList[res/.Last@elim//Simplify,y][[1;;2]];
	Echo[y==FullSimplify[((x^2+m x+n)/.Last@elim)/(-e)^(1/3)]//TraditionalForm,"Traceback: "];
	y^4-y+f/(-e)^(4/3)
];
(* ::Subsubsection:: *)
(*HermiteSolve*)
HermiteSolve[rho_,t_]:=Module[
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
(*MeijerGSolve*)
MeijerGSolve[n_Integer /; n > 1, t_] := Append[
	Table[
		Exp[-((2*Pi*I*j)/(n - 1))] -(t*Sqrt[n/(n - 1)^3]*Inactive[MeijerG][{Append[Range[n - 1]/n,
			(n - 2)/(n - 1)], {}}, {Range[0, n - 2]/(n - 1),
			{-(1/(n - 1))}}, -((n^(n/(n - 1))*t*Exp[(2*Pi*I*j)/(n - 1)])/(n - 1)),
			1/(n - 1)])/(2*Pi)^(n - 3/2),
		{j, 0, n - 2}],
	(Sqrt[n/(n - 1)^3]*t*Inactive[MeijerG][{Range[0, n - 1]/n, {}}, {{0},Range[-1, n - 3]/(n - 1)},
		(-n^n)*(t/(n - 1))^(n - 1)])/Sqrt[2*Pi]
];
(* ::Subsubsection:: *)
(*Hypergeometric*)
HypergeometricSolve[2,1]=1/2-1/2 HypergeometricPFQ[{-(1/2)},{},-4 t];
HypergeometricSolve[2,2]=1/2+1/2 HypergeometricPFQ[{-(1/2)},{},-4 t];
HypergeometricSolve[3,1]=Hypergeometric2F1[1/6,-(1/6),1/2,(27 t^2)/4]
	+1/2 t Hypergeometric2F1[2/3,1/3,3/2,(27 t^2)/4];
HypergeometricSolve[3,2]=-Hypergeometric2F1[1/6,-(1/6),1/2,(27 t^2)/4]
	+1/2 t Hypergeometric2F1[2/3,1/3,3/2,(27 t^2)/4];
HypergeometricSolve[3,3]=-(t Hypergeometric2F1[2/3,1/3,3/2,(27 t^2)/4]);
HypergeometricSolve[4,1]=-(t HypergeometricPFQ[{1/4,1/2,3/4},{2/3,4/3},-((256 t^3)/27)]);
HypergeometricSolve[4,2]=HypergeometricPFQ[{-(1/12),1/6,5/12},{1/3,2/3},-((256 t^3)/27)]
	+1/3 t HypergeometricPFQ[{1/4,1/2,3/4},{2/3,4/3},-((256 t^3)/27)]
	-2/9 t^2 HypergeometricPFQ[{7/12,5/6,13/12},{4/3,5/3},-((256 t^3)/27)];
HypergeometricSolve[4,3]=(-1)^(2/3) HypergeometricPFQ[{-(1/12),1/6,5/12},{1/3,2/3},-((256 t^3)/27)]
	+1/3 t HypergeometricPFQ[{1/4,1/2,3/4},{2/3,4/3},-((256 t^3)/27)]
	+2/9 (-1)^(1/3) t^2 HypergeometricPFQ[{7/12,5/6,13/12},{4/3,5/3},-((256 t^3)/27)];
HypergeometricSolve[4,4]=-((-1)^(1/3) HypergeometricPFQ[{-(1/12),1/6,5/12},{1/3,2/3},-((256 t^3)/27)])
	+1/3 t HypergeometricPFQ[{1/4,1/2,3/4},{2/3,4/3},-((256 t^3)/27)]
	-2/9 (-1)^(2/3) t^2 HypergeometricPFQ[{7/12,5/6,13/12},{4/3,5/3},-((256 t^3)/27)];
HypergeometricSolve[5,1]=t HypergeometricPFQ[{1/5,2/5,3/5,4/5},{1/2,3/4,5/4},-((3125 t^4)/256)];
HypergeometricSolve[5,2]=-((4 (-1)^(1/4) Gamma[3/4] HypergeometricPFQ[{-(1/20),3/20,7/20,11/20},{1/4,1/2,3/4},-((3125 t^4)/256)])/Gamma[-(1/4)])
	-1/4 t HypergeometricPFQ[{1/5,2/5,3/5,4/5},{1/2,3/4,5/4},-((3125 t^4)/256)]
	+(25 (-1)^(3/4) t^2 Gamma[5/4] HypergeometricPFQ[{9/20,13/20,17/20,21/20},{3/4,5/4,3/2},-((3125 t^4)/256)])/(128 Gamma[9/4])
	+1/32 (5 I) t^3 HypergeometricPFQ[{7/10,9/10,11/10,13/10},{5/4,3/2,7/4},-((3125 t^4)/256)];
HypergeometricSolve[5,3]=-((4 (-1)^(3/4) Gamma[3/4] HypergeometricPFQ[{-(1/20),3/20,7/20,11/20},{1/4,1/2,3/4},-((3125 t^4)/256)])/Gamma[-(1/4)])
	-1/4 t HypergeometricPFQ[{1/5,2/5,3/5,4/5},{1/2,3/4,5/4},-((3125 t^4)/256)]
	+(25 (-1)^(1/4) t^2 Gamma[5/4] HypergeometricPFQ[{9/20,13/20,17/20,21/20},{3/4,5/4,3/2},-((3125 t^4)/256)])/(128 Gamma[9/4])
	-1/32 (5 I) t^3 HypergeometricPFQ[{7/10,9/10,11/10,13/10},{5/4,3/2,7/4},-((3125 t^4)/256)];
HypergeometricSolve[5,4]=(4 (-1)^(1/4) Gamma[3/4] HypergeometricPFQ[{-(1/20),3/20,7/20,11/20},{1/4,1/2,3/4},-((3125 t^4)/256)])/Gamma[-(1/4)]
	-1/4 t HypergeometricPFQ[{1/5,2/5,3/5,4/5},{1/2,3/4,5/4},-((3125 t^4)/256)]
	-(25 (-1)^(3/4) t^2 Gamma[5/4] HypergeometricPFQ[{9/20,13/20,17/20,21/20},{3/4,5/4,3/2},-((3125 t^4)/256)])/(128 Gamma[9/4])
	+1/32 (5 I) t^3 HypergeometricPFQ[{7/10,9/10,11/10,13/10},{5/4,3/2,7/4},-((3125 t^4)/256)];
HypergeometricSolve[5,5]=(4 (-1)^(3/4) Gamma[3/4] HypergeometricPFQ[{-(1/20),3/20,7/20,11/20},{1/4,1/2,3/4},-((3125 t^4)/256)])/Gamma[-(1/4)]
	-1/4 t HypergeometricPFQ[{1/5,2/5,3/5,4/5},{1/2,3/4,5/4},-((3125 t^4)/256)]
	-(25 (-1)^(1/4) t^2 Gamma[5/4] HypergeometricPFQ[{9/20,13/20,17/20,21/20},{3/4,5/4,3/2},-((3125 t^4)/256)])/(128 Gamma[9/4])
	-1/32 (5 I) t^3 HypergeometricPFQ[{7/10,9/10,11/10,13/10},{5/4,3/2,7/4},-((3125 t^4)/256)];
HypergeometricPostProcess[f_]:=Collect[f,_HypergeometricPFQ]/.{a_ F_HypergeometricPFQ:>With[{r=Rationalize[Chop[N[a]]]},r F/;Precision[r]===\[Infinity]]};
HypergeometricSolve[n_Integer,k_Integer]:=Module[
	{coef,i},
	coef=Refine[FunctionExpand[SeriesCoefficient[Root[#1^n-#1-t&,1],{t,0,k}]],k>=0];
	HypergeometricPostProcess[Sum[coef t^i,{i,0,\[Infinity]}]]
];

(* ::Subsection::Closed:: *)
(*附加设置*)
End[];
SetAttributes[
	{ },
	{Protected,ReadProtected}
];
EndPackage[];
