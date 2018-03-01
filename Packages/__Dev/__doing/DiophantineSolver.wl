(* ::Package:: *)
(* ::Title:: *)
(*Example(样板包)*)
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
BeginPackage["DiophantineSolver`"];
AndrewAllanSolver::usage = "";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
DiophantineSolver::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
DiophantineSolver$Version="V1.0";
DiophantineSolver$LastUpdate="2016-11-11";
(* ::Subsubsection:: *)
(*AndrewAllan*)
Begin["AndrewAllan`"];
ECplusN[p1_,p2_,para_]:=Re@N@EllipticExp[EllipticLog[p1,para]+EllipticLog[p2,para],para];
ECplus[p1_,p2_,para_]:=Block[
	{A,B,k,x,y,x1,y1,x2,y2},
	{A,B}=para;{x1,y1}=p1;{x2,y2}=p2;
	{x,-y}/.If[p1==p2,
		Solve[{
			y^2==x(x^2+A x+B),
			(y-y1)/(x-x1)==(3x1^2+2 A x1+B)/(2y1)
		},{x,y}]//First,
		Solve[{
			y^2==x(x^2+A x+B),
			(y-y1)/(x-x1)==(y1-y2)/(x1-x2),
			x!=x1,x!=x2
		},{x,y}]//First
	]
];
Solver[n_Integer]:=Module[
	{
		x,y,a,b,c,A,B,
		para,sol,ji,iv,sp,nl,eci,fii
	},
	Echo[{
		x==-4(a+b+2c)(n+3)/((2+n) a+(1+n)b-c),
		y==4(a-b)(2 n^2+11n+15)/((2+n) a+(1+n)b-c)
	}//FullSimplify,"等价映射: "];
	para={A,B}={4n^2+12n-3,32(n+3)};
	sol=Solve[{Echo[y^2==x(x^2+A x+B),"等价椭圆曲线: "],y>0,x!=0},{x,y},Integers];
	ji=DeleteCases[{x,y}/.sol,{x,ConditionalExpression[_,_]}];
	If[ji=={},Return[$Failed]];
	iv=Interval[
		{(3-12n-4n^2)/2-(2n+5)Sqrt[4n^2+4n-15]/2,-2(n+3)(n+Sqrt[n^2-4])},
		{-2(n+3)(n-Sqrt[n^2-4]),-4(n+3)/(n+2)}
	]//N;
	sp=RandomChoice@ji;
	nl=NestWhileList[ECplusN[sp,#,para]&,sp,!IntervalMemberQ[iv,First@#]&];
	eci=Rule@@@Transpose[{{x,y},Nest[ECplus[sp,#,para]&,sp,Length@nl-1]}];
	fii=Solve[{
		a/(a+b+c)==(8n+24-x+y)/(24+8 n-6 x-2 n x),
		b/(a+b+c)==(8n+24-x-y)/(24+8 n-6 x-2 n x),
		c/(a+b+c)==(4 n+12+2 x+n x)/(3 x+n x-4 n-12)
	}/.eci,{a,b,c},Integers];
	fii/.{ConditionalExpression[x_,_]:>First@x}//First
];
End[];
AndrewAllanSolver[n_]:=AndrewAllan`Solver[n];



(* ::Subsubsection:: *)
(*功能块 2*)
ExampleFunction[2]="我就是个示例函数,什么功能都没有";


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;
SetAttributes[
	{ },
	{Protected,ReadProtected}
];
EndPackage[];
