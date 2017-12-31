(* ::Package:: *)
(* ::Title:: *)
(*ExMatrix(特殊矩阵包)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template*)
(**)
(*Author:GalAster*)
(*Creation Date:2016-11-22*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["ExMatrix`"];
SpiralMatrix::usage = "给出一个n×n的螺旋矩阵";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
ExMatrix$Version="V0.1";
ExMatrix$Environment="V11.0+";
ExMatrix$LastUpdate="2016-11-22";
ExMatrix::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)



(* ::Subsubsection:: *)
(*特殊矩阵*)
SpiralMatrix[n_?OddQ]:=Permute[Range[n^2],Accumulate@Take[Join[{n^2+1}/2,
  Flatten@Table[(-1)^ji,{j,n},{i,{-1,n}},{j}]],n^2]]~Partition~n;
SpiralMatrix[n_]:=SpiralMatrix[n+1][[1;;-2,2;;-1]];
MagicMatrix[n_]:=MagicSquare`Magic[n];
JacobianMatrix[f_List?VectorQ,x_List]:=Outer[D,f,x]/;Equal@@(Dimensions/@{f,x}) ;
JacobianDeterminant[f_List?VectorQ,x_List]:=Det[JacobianMatrix[f,x]]/;Equal@@(Dimensions/@{f,x});


(* ::Subsubsection:: *)
(*未分类代码*)
(*本程序包中Path的格式为双重表{{x,y}},而非Mathematica中使用的列表{a,b,c}*)
Options[CiclePath]={Radius->0.25,FontSize->Scaled[1/20]};
CiclePath[matrix_,path_,OptionsPattern[]]:=
    Graphics[{MapIndexed[{Circle[#2,OptionValue[Radius]],Text[#,#2]}&,matrix,{2}],
      Arrow[#,OptionValue[Radius]]&/@Partition[path,2,1]},BaseStyle->{FontSize->OptionValue[FontSize]}];
MinPathF[mat_][i_,j_]:=MinPathF[mat][i,j]=
    mat[[i,j]]+Piecewise[{
      {Min[MinPathF[mat][i+1,j],MinPathF[mat][i,j+1]],
        i<Length[mat]&&j<Length[mat[[i]]]},
      {MinPathF[mat][i+1,j],i<Length[mat]},
      {MinPathF[mat][i,j+1],j<Length[mat[[i]]]}},0];
nextF[mat_][{i_,j_}]:=If[i<Length[mat]&&j<Length[mat[[i]]],
  If[MinPathF[mat][i+1,j]<MinPathF[mat][i,j+1],{i+1,j},{i,j+1}],
  If[i<Length[mat],{i+1,j},If[j<Length[mat[[i]]],{i,j+1},{}]]];
(*LRC=LowerRightCorner 右下角*)
Options[MatrixPathLRC]={Return->Graphics,Style->{Dividers->All,Spacings->{1.5,1.5}}};
MatrixPathLRC[mat_,Start_:{1,1},OptionsPattern[]]:=Switch[OptionValue[Return],
  Path,Most@NestWhileList[nextF[mat],Start,!#==={}&],
  Graphics,Grid[grid,#]&@Join[{Background->{Automatic,Automatic,
    Thread[MatrixPathLRC[mat,Start,Return->Path]->Red]}},OptionValue[Style]]];


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];