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
(*Author:GalAster*)
(*Creation Date:2016-07-12*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["MathConfession`"];
MathLove::usage = "MathLove[]随机给你一种表白方式.";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
ExForm$Version="V0.2";
ExForm$Environment="V11.0+";
ExForm$LastUpdate="2016-11-12";
ExForm::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
MathLove[]:=MathLove[RandomInteger[10]];
Rose[x_,theta_]:=Block[{phi=(Pi/2)*Exp[-(theta/(8*Pi))],
  X=1-(1/2)*((5/4)*(1-Mod[18/5*theta,2*Pi]/Pi)^2-1/4)^2,y,r},
  y=(3913/2000-5x+16x^2/5)x^2*Sin[phi];
  r=X*(x*Sin[phi]+y*Cos[phi]);{r*Sin[theta],r*Cos[theta],
    X*(x*Cos[phi]-y*Sin[phi])}];
MathLove[1]:=Block[{rose,stem,text},
  rose=ParametricPlot3D[
    Evaluate[Rose[x,theta]],{x,0,1},{theta,-2Pi,15Pi},
    Mesh->None,PerformanceGoal->"Speed",
    PlotStyle->RGBColor[246/255,1,157/255],PlotPoints->100,
    Boxed->False,Axes->False,
    PlotRange->{{-1,1},{-1,1},{-1.6,1}}];
  stem=Graphics3D[{Green,Cylinder[{{0,0,-0.05},{0,0,-10}},0.1]}];
  text=Text[TraditionalForm[(49/50)*x*((x*(400*x*(16*x-25)+3913)*(Cos[Pi/E^(1/4)]-1))/4000+Cos[Pi/(2*E^(1/4))])]];
  Print[text];Show[rose,stem]];
FlowerR[u_,v_,c_]=v^(1.6)(0.+(0.01+Abs[Sin[c u/2]])^(1/3));
Jazmin[u_,v_,c_]=Evaluate[{FlowerR[u,v,c]Sin[u]*Cos[v],FlowerR[u,v,c]Cos[u]*Cos[v],3.5Abs[Cos[v]]^(0.7)(2+Sin[c u/2]^2)}];
MathLove[2]=ParametricPlot3D[Evaluate[Jazmin[u,v,5]],{u,-Pi,Pi},{v,0,3.4},PlotPoints->50,
  BoxRatios->Automatic,PlotRange->All,Boxed->False,Axes->None,Mesh->None,
  ColorFunction->(ColorData["SunsetColors"][0.9(1-0.9#5)]&)];
MathLove[3]=ParametricPlot3D[Evaluate[Jazmin[u,v,7]],{u,-Pi,Pi},{v,0,Pi},PlotPoints->30,
  BoxRatios->Automatic,PlotRange->All,Boxed->False,Axes->None,Mesh->None,
  ColorFunction->(ColorData["SolarColors"][1.5(#5^1.3)]&)];
MathLove[4]=ParametricPlot3D[Evaluate[Jazmin[u,v,9]],{u,-Pi,Pi},{v,0,3.4},PlotPoints->50,
  BoxRatios->Automatic,PlotRange->All,Boxed->False,Axes->None,Mesh->None,
  ColorFunction->(ColorData["SunsetColors"][0.9(1-0.9#5)]&)];
MathLove[5]:=Block[{vals,funs},{vals,funs}=
    NDEigensystem[{-Laplacian[u[x,y],{x,y}],DirichletCondition[u[x,y]==0,x==0]},
      u[x,y],{x,y}\[Element]RegionSymmetricDifference[Disk[{0,0},5],Disk[{0,0},3]],1];
Quiet@Plot3D[funs,{x,-5,5},{y,-5,5},PlotRange->All,PlotTheme->"Marketing",
  PlotLegends->Placed["-\!\(\*TemplateBox[{RowBox[{\"u\",\"(\",\n\
        RowBox[{\"x\",\",\",\"y\"}],\")\"}],RowBox[{\"{\",\n\
        RowBox[{\"x\",\",\",\"y\"}],\"}\"}]},\n\
        \"Laplacian\"]\),3<\!\(\*SuperscriptBox[\(x\),\
        \(2\)]\)+\!\(\*SuperscriptBox[\(y\),\(2\)]\)<5",Below]]];
MathLove[6]:=ContourPlot[(-1+x^2+y^2)^3==x^2 y^3,{x,-1.4,1.4},{y,-1.3,1.5},
  PlotLabel->TraditionalForm[(-1+x^2+y^2)^3==x^2 y^3]];
MathLove[7]:=RegionPlot[x^2+((-5y)/4+Sqrt[Abs[x]])^2<1,{x,-1.3,1.3},{y,-1.1,1.5},
  PlotLabel->TraditionalForm[x^2+((-5y)/4+Sqrt[Abs[x]])^2<1]];
MathLove[8]:=MatrixPlot[Table[(x+y)/((y^2+x^2-2)^3+y^2x^3),{x,-2.5,2,0.01},{y,-2.25,2.25,0.01}],
  Frame->False,PlotLabel->TraditionalForm[(x+y)/((y^2+x^2-2)^3+y^2x^3)]];


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];