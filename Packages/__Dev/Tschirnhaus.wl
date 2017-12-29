(* ::Package:: *)
(* ::Title:: *)
(*Tschirnhaus(样板包)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template*)
(**)
(*Author: 酱紫君*)
(*Creation Date:2012-12-28*)
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
(*功能块 2*)



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
PrincipalTransform[p_?PolynomialQ,x_,y_]:=Block[
	{mQ,trans,eqn},
	mQ=!MatchQ[CoefficientList[p,x],{_,_,_,_,_?(#=!=0&),_}];
	If[mQ,Return@Message[TschirnhausTransform::notPT,p]];
	{trans,eqn}=Quiet@PrincipalTransformEqn[p==0,x,y]//Chop;
	Echo[TraditionalForm[x==trans[y]],"Traceback:"];
	First@eqn
];
BringJerrardTransform[p_?PolynomialQ,x_,y_]:=Block[
	{mQ,trans,eqn},
	mQ=!MatchQ[CoefficientList[p,x],{_,_,_,0,0,_}];
	If[mQ,Return@Message[TschirnhausTransform::notBJ,p]];
	{trans,eqn}=Quiet@BringJerrardTransformEqn[p==0,x,y]//Chop;
	Echo[TraditionalForm[x==trans[y]],"Traceback:"];
	First@eqn
];
CanonicalTransform[p_?PolynomialQ,x_,y_]:=Block[
	{mQ,trans,eqn},
	mQ=!MatchQ[CoefficientList[p,x],{_,_,0,0,0,_}];
	If[mQ,Return@Message[TschirnhausTransform::notCT,p]];
	{trans,eqn}=Quiet@CanonicalTransformEqn[p==0,x,y]//Chop;
	Echo[TraditionalForm[x==trans[y]],"Traceback:"];
	First@eqn
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
	Table[Exp[-((2*Pi*I*j)/(n - 1))] -(t*Sqrt[n/(n - 1)^3]*Inactive[MeijerG][{Append[Range[n - 1]/n,
			(n - 2)/(n - 1)], {}}, {Range[0, n - 2]/(n - 1),
			{-(1/(n - 1))}}, -((n^(n/(n - 1))*t*Exp[(2*Pi*I*j)/(n - 1)])/(n - 1)),
		1/(n - 1)])/(2*Pi)^(n - 3/2),{j, 0, n - 2}],
		(Sqrt[n/(n - 1)^3]*t*Inactive[MeijerG][{Range[0, n - 1]/n, {}}, {{0}, Range[-1, n - 3]/(n - 1)},
			(-n^n)*(t/(n - 1))^(n - 1)])/Sqrt[2*Pi]
];
(* ::Subsubsection:: *)
(*功能块 2*)



(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];
