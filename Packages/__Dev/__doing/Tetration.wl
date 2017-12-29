(* ::Package:: *)
(* ::Title:: *)
(*Tetration(样板包)*)
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
BeginPackage["Tetration`"];
Tetrate::usage = "这里应该填这个函数的说明,如果要换行用\"\r\"\r就像这样";
TetraLog::usage = "这里应该填这个函数的说明,如果要换行用\"\r\"\r就像这样";
TetraRoot::usage = "这里应该填这个函数的说明,如果要换行用\"\r\"\r就像这样";
TetraD::usage = "这里应该填这个函数的说明,如果要换行用\"\r\"\r就像这样";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
Tetration$Version="V1.0";
Tetration$Environment="V11.0+";
Tetration$LastUpdate="2016-11-11";
Tetration::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
(* ::Subsubsection:: *)
(*功能块 1*)
Log4Prepare[n_,x_]:=Log4Prepare[n,x]=
Block[{mat,$MachinePrecision=32},
	mat=Table[k^j/k!-If[j==k,Log[x]^-k,0],{j,0,n-1},{k,1,n}];
	{x,LinearSolve[N[mat,$MachinePrecision],UnitVector[n,1]]}
];

TetrationEvaluate[v_List,y_?NumericQ]:=
Block[{SlogCrit,TetCrit},
	SlogCrit[zc_]:=-1+Sum[v[[2,k]]*zc^k/k!,{k,1,Length[v[[2]]]}];
	TetCrit[yc_]:=FindRoot[SlogCrit[z]==yc,{z,1}][[1,2]];
	If[y>-1,
		Nest[Power[v[[1]],#]&,TetCrit[y-Ceiling[y]],Ceiling[y]],
		Nest[Log[v[[1]],#]&,TetCrit[y-Ceiling[y]],-Ceiling[y]]
	]
]

Options[Tetrate]={MaxIterations->10};
Tetrate[x_,y_,OptionsPattern[]]:=TetrationEvaluate[Log4Prepare[OptionValue[MaxIterations],x],y]
Tetrate[z_,Infinity]:= ProductLog[-Log[z]]/-Log[z];

Log4Evaluate[v_List,z_?NumericQ]:=
Block[{SlogCrit},
	SlogCrit[zc_]:=-1+Sum[v[[2,k]]*zc^k/k!,{k,1,Length[v[[2]]]}];
	Which[
		z<=0,	SlogCrit[v[[1]]^z]-1,
		0<z<=1,	SlogCrit[z],
		z>1,	Block[{i=-1},SlogCrit[NestWhile[Log[v[[1]],#]&,z,(i++;#>1)&]]+i]
	]
]

Options[TetraLog]={MaxIterations->10};
TetraLog[x_,y_,OptionsPattern[]]:=
Block[{ans},
	ans=Log4Evaluate[Log4Prepare[OptionValue[MaxIterations],x],y];
	If[Chop@FractionalPart[ans]==0,Round[ans],ans]
]

Bisection[f_, int_, tol_, niter_] := 
Block[{m = tol + 1, prev, ym, yl = f[Last@int]},
	NestWhile[(
		prev = m;
		m = Total@#/2;
		ym = f[m];
		If[ym*yl > 0,
			yl = ym;{First@#, m},
			{m, Last@#}
		]
	) &,
	int,ym != 0 && Abs[m - prev] > tol &, 2, niter]
]
Options[TetraRoot]={MaxIterations->10};
TetraRoot[a_,y_,OptionsPattern[]]:=
Block[{ff,ans},
	ff=Tetrate[a,#]-y&;
	ans=Quiet@Bisection[ff,{0.001,10},10^-OptionValue[MaxIterations],250];
	If[First@ans<0.01,Message[General::ovfl];$Failed,Last@ans]
];


(* ::Subsubsection:: *)
(*功能块 2*)
(*Gives the k-th numerical derivative of f(x) at x=c:*)
TetraND[f_,x_,c_]:=TetraND[f,x,c,1]
TetraND[f_,x_,c_,k_]:=TetraND[f,x,c,k,0.0001]
TetraND[f_,x_,c_,0,h_]:=(f/.x->c)
TetraND[f_,x_,c_,k_,h_]/;k!=0:=(TetraND[f,x,c+h,k-1,h]-TetraND[f,x,c,k-1,h])/h
TetraNDImage=ArrayPlot[Transpose@Partition[ToCharacterCode[#//Compress][[1;;49]],7],Mesh->True,ColorFunction->ColorData["TemperatureMap"],ImageSize->36]&;

Format[TetraNDFormat[___], OutputForm] := "TetraND[<>]"
Format[TetraNDFormat[___], InputForm] := "TetraND[<>]"
TetraNDFormat/:MakeBoxes[obj:TetraNDFormat[ass_], form:StandardForm|TraditionalForm] := 
Module[{above, below}, 
	above = {
		{BoxForm`SummaryItem[{"Function: ", ass["Function"]}],SpanFromLeft}, 
		{BoxForm`SummaryItem[{"Order: ", ass["Order"]}],BoxForm`SummaryItem[{"error: ", ass["error"]}]}}; 
	below = {}; 
	BoxForm`ArrangeSummaryBox[TetraND, obj,ass["icon"], above, below, form, 
	"Interpretable" -> Automatic]
]; 

SetAttributes[TetraD,HoldFirst]
Options[TetraD]={MaxIterations->7};
TetraD[expr_,t_,OptionsPattern[]]:=TetraD[expr,{t,1}]
TetraD[expr_,{t_,n_},OptionsPattern[]]:=
Block[{x,err,fun},
	err=N[10^-Sqrt@OptionValue[MaxIterations]];
	fun=Evaluate@TetraND[expr,t,#,n,err]&;
	TetraNDFormat[Association[
		"Function" ->Hold@expr, 
		"Order" ->n,
		"error" ->err,
		"icon"->TetraNDImage@expr,
		Function->fun]
	]
]
TetraNDFormat[asc_?AssociationQ][prop_] := Lookup[asc, prop]
TetraNDFormat[asc_?AssociationQ][n_?NumericQ] := Lookup[asc,Function][n]
(* ::Subsubsection:: *)
(*功能块 2*)



(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];
