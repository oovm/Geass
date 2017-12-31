(* ::Package:: *)
(* ::Title:: *)
(*ExCompile(编译函数包)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template*)
(**)
(*Author:GalAster*)
(*Creation Date:2016-12-19*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*编译函数包,用于储存编译函数的源代码*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["ExCompile`"];
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
ExCompile$Version="V0.1";
ExCompile$Environment="V11.0+";
ExCompile$LastUpdate="2016-12-19";
ExCompile::usage = "编译函数包,用于储存编译函数的源代码";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
(* ::Subsubsection:: *)
(*功能块 1*)
ExampleFunction[1]="我就是个示例函数,什么功能都没有";



(* ::Subsubsection:: *)
(*功能块 2*)
ExampleFunction[2]="我就是个示例函数,什么功能都没有";


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];
