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
(*功能块 1*)
ExampleFunction[1]="我就是个示例函数,什么功能都没有";

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
