(* ::Package:: *)
(* ::Title:: *)
(*ExFactorial(特殊阶乘包)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template(v1.3)*)
(**)
(*Author: GalAster*)
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
BeginPackage["ExFactorial`"];
MultiFactorial::usage = "MultiFactorial[x,k] x 的 k阶阶乘.";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
ExFactorial::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
ExFactorial$Version="V1.0";
ExFactorial$LastUpdate="2018-01-01";
Unprotect[Sum];
(* ::Subsubsection:: *)
(*MultiFactorial*)
MultiFactorialInt[n_,k_]:=With[{q=Quotient[n+k-1,k]},k^q q! Binomial[n/k,q]];
MultiFactorialInt2[n_,k_]:=With[{q=Quotient[n-1,k]},k^(q+1) Gamma[n/k+1]/Gamma[n/k-q]];
MultiFactorialInt3[n_,k_]:=FactorialPower[n,Quotient[n-1,k]+1,k];
MultiFactorialFast[z_,k_]:=k^((z-1)/k)Gamma[z/k+1]/Gamma[1/k+1];
ReciprocalFactorialSumConstant[n_]:=1/n Exp[1/n](n+Sum[n^(k/n) Gamma[k/n,0,1/n],{k,n-1}]);
Rec[p_]:=Total@Table[E^(2Pi I j n/p),{j,p}]/p;
Rec2[p_]:=Sum[E^(2Pi I j n/p),{j,p}]/p;
ep=Table[Rec@#/.{n->n+i},{i,#-1,0,-1}]&;
mf=Table[Simplify[MultiFactorialInt[# n-i,#],n\[Element]Integers]/.n->(n+i)/#,{i,#-1,0,-1}]&;
MultiFactorial::noInt=" `1` 不是一个正整数!";
Options[MultiFactorial]={Simplify->True,Complex->False,Fast->False};
MultiFactorial[x_,k_,OptionsPattern[]]:=Block[{},
	If[!IntegerQ@k,Message[MultiFactorial::noInt,k]];
	If[IntegerQ@x,Return@MultiFactorialInt[x,k]];
	If[!OptionValue[Complex],Return@MultiFactorialInt[x,k]];
	If[OptionValue[Fast],Return@MultiFactorialFast[x,k]];
	If[!OptionValue[Simplify],Evaluate[Re[ep[k].mf[k]]/.n->x]];
	real=Re@Simplify[ep[k].mf[k]//FunctionExpand,n\[Element]Integers]//ComplexExpand;
	Return[Simplify[real,n\[Element]Reals]/.n->x];
];
Sum[1/MultiFactorial[n_,k_],{n_,0,Infinity}]:=ReciprocalFactorialSumConstant[k];
(* ::Subsubsection:: *)
(*功能块 2*)
ExampleFunction[2]="我就是个示例函数,什么功能都没有";


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;
Protected[Sum];
SetAttributes[
	{ },
	{Protected,ReadProtected}
];
EndPackage[];
