(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: Main *)
(* :Context: Main` *)
(* :Author: GalAster *)
(* :Date: 2016-01-11 *)

(* :Package Version: 0.2 *)
(* :Update: 2016-11-14 *)
(* :Mathematica Version: 11.0+ *)
(* :Copyright:该软件包遵从CC协议:BY+NA+NC(署名、非商业性使用、相同方式共享） *)
(* :Keywords: *)
(* :Discussion: *)



BeginPackage["Main`"];
Begin["`Private`"];
PlayList[n_]:=Block[{foo,up},
  foo[p_]:=Range[5(-1+p)p,p(4+5p),p];
  up=Floor[-(2/5)+1/5Sqrt[4+5n]];
  Drop[Flatten@{foo/@Range[up],Range[5up(up+1),n,up+1]},1]];

Venn[n_,ineqs_:{}]:=
    Block[{i,r=.6,R=1,v,grouprules,x,y,x1,x2,y1,y2,ve},
      v=Table[Circle[r{Cos[#],Sin[#]}&[2Pi(i-1)/n],R],{i,n}];
      {x1,x2}={Min[#],Max[#]}&[Flatten@Replace[v,Circle[{xx_,yy_},rr_]:>{xx-rr,xx+rr},{1}]];
      {y1,y2}={Min[#],Max[#]}&[Flatten@Replace[v,Circle[{xx_,yy_},rr_]:>{yy-rr,yy+rr},{1}]];
      ve[x_,y_,i_]:=v[[i]]/.Circle[{xx_,yy_},rr_]:>(x-xx)^2+(y-yy)^2<rr^2;
      grouprules[x_,y_]=ineqs/.Table[With[{is=i},Subscript[_,is]:>ve[x,y,is]],{i,n}];
      Show[If[MatchQ[ineqs,{}|False],{},
        RegionPlot[grouprules[x,y],{x,x1,x2},{y,y1,y2},Axes->False]],Graphics[v],
        PlotLabel->TraditionalForm[Replace[ineqs,{}|False->\[EmptySet]]],Frame->False]];




A = ConstantArray[0, {7, 7}];
DynamicBlock[{pt = {0, 0}},
  Dynamic@EventHandler[
    ArrayPlot[A, ImageSize -> 512, Mesh -> True,
      MeshStyle ->
          Black], {{"MouseDown",
      1} :> (A[[Ceiling[7 - MousePosition["Graphics"][[2]]],
        Ceiling[MousePosition["Graphics"][[1]]]]] = 1), {"MouseDown",
      2} :> (A[[Ceiling[7 - MousePosition["Graphics"][[2]]],
        Ceiling[MousePosition["Graphics"][[1]]]]] = 0)}]]
Dynamic[B = {Length[A[[1]]], FromDigits[#, 2] & /@ A}]
NonogramList = B
ListToMatrix[list_] := IntegerDigits[list[[2]], 2, list[[1]]]
ArrayPlot[NonogramMatrix = ListToMatrix@NonogramList]
SplitNM = Map[Split, NonogramMatrix];
SplitMN = Map[Split, Transpose[NonogramMatrix]];
ListLift =
    Map[Length,
      Table[Select[SplitNM[[i]], MemberQ[#, 1] &], {i, 1,
        Length[SplitNM]}], {2}]
ListAbove =
    Map[Length,
      Table[Select[SplitMN[[i]], MemberQ[#, 1] &], {i, 1,
        Length[SplitMN]}], {2}]


End[];
Protect[DigitalCycle,TriPainting];

EndPackage[]