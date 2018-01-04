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
MultiFactorial::usage = "MultiFactorial[x,k] x 的 k阶 阶乘.";
HyperFactorial::usage = "超阶乘函数 HyperFactorial[n].";
SuperFactorial::usage = "叠阶乘函数 SuperFactorial[n].";
SubFactorial::usage = "n个元素的错排数为SubFactorial[n].";
(*AlternatingFactorial*)
RisingFactorial::usage = "上阶乘函数 RisingFactorial[n].";
FallingFactorial::usage = "下阶乘函数 FallingFactorial[n].";
EulerFactorial::usage = "EulerFactorial[n] 欧拉阶乘函数.";
HadamardFactorial::usage = "HadamardFactorial[n] 阿达玛阶乘函数.";
LuschnyFactorial::usage = "LuschnyFactorial[n] 阶乘.";
Primorial::usage = "Primorial[n] 第n个素数阶乘.";
ExpFactorial::usage = "ExpFactorial[n] 第n个指数阶乘.";
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
Unprotect[Sum,FunctionExpand];
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
(*Hyper/Super/Sub Factorial*)

(* ::Subsubsection:: *)
(*Hyper/Super/Sub*)
Unprotect[FunctionExpand,AlternatingFactorial];
SetAttributes[
	{
		HyperFactorial,SuperFactorial,
		SubFactorial,
		EulerFactorial,HadamardFactorial,LuschnyFactorial,
		ExpFactorial,Primorial
	},
	{NumericFunction,Listable}
];
HyperFactorial[n_?NumericQ]:=Hyperfactorial[n];
SuperFactorial[n_?NumericQ]:=BarnesG[n+2];
FunctionExpand[HyperFactorial[z_]]:=Hyperfactorial[z];
FunctionExpand[SuperFactorial[z_]]:=BarnesG[z+2];
(*Todo: 微分,级数,格式化*)
(*AlternatingFactorial*)
(*Format[AlternatingFactorial[z_],TraditionalForm]:=RowBox[{"AF(",MakeBoxes[z,TraditionalForm],")"}]*)
SubFactorial[n_?NumericQ]:= Subfactorial[n];
FunctionExpand[SubFactorial[z_]]:=Subfactorial[z];
(*Todo: 重载,微分,级数,格式化*)
RisingFactorial[x_?NumericQ,n_?NumericQ]:=Pochhammer[x,n];
FallingFactorial[x_?NumericQ, n_?NumericQ] := (-1)^n Pochhammer[-x, n];
FunctionExpand@RisingFactorial[x_,n_]:=Pochhammer[x,n]
FunctionExpand@FallingFactorial[x_, n_] := (-1)^n Pochhammer[-x, n];
(*Todo: 微分,级数,格式化*)
EulerFactorial[x_?NumericQ]:=Gamma[x+1];
HadamardFactorial[x_?NumericQ]:=(PolyGamma[1/2-x/2]-PolyGamma[-x/2])/(2Gamma[-x]);
LuschnyFactorial[x_?NumericQ]:=(1/2+x (PolyGamma[1-x/2]-PolyGamma[1/2-x/2])/2)/(-x)!;
FunctionExpand@EulerFactorial[x_]:=Gamma[x+1];
FunctionExpand@HadamardFactorial[x_]:=(PolyGamma[1/2-x/2]-PolyGamma[-x/2])/(2Gamma[-x]);
FunctionExpand@LuschnyFactorial[x_]:=(1/2+x (PolyGamma[1-x/2]-PolyGamma[1/2-x/2])/2)/(-x)!;
(*Todo: 微分,级数,格式化*)
PosIntQ=IntegerQ@#&&#>0&;
ExpFactorial[0]:=1;
ExpFactorial[n_?PosIntQ]:=ExpFactorial[n]=n^(ExpFactorial[n-1]);
Primorial[0]:=1;
Primorial[1]:=2;
Primorial[n_?PosIntQ]:=Primorial[n]=Prime[n]Primorial[n-1];
(*Todo: 微分,级数,格式化*)

(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;
Protected[Sum,FunctionExpand];
SetAttributes[
	{ },
	{Protected,ReadProtected}
];
EndPackage[];
