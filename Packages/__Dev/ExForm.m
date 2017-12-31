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
(*Creation Date:2016-12-06*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["ExForm`"];
LispForm::usage = "以Lisp形式显示Mathematica表达式";
TriangleForm::usage = "将一个三角式的多重表显示出来,比如杨辉三角";
ColorForm::usage = "给数字染色";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
ExForm$Version="V0.1";
ExForm$Environment="V11.0+";
ExForm$LastUpdate="2016-12-06";
ExForm::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)



(* ::Subsubsection:: *)
(*Lisp形式*)
Attributes[LispForm]=HoldAll;
LispForm[exp_]:=Block[{ml,str,aaa,bbb,ccc,ddd,eee},
  ml=ImportString@ExportString[FullForm[Hold@exp],"MathML"];
  str=Cases[ml,_String,-1];
  aaa={#-1,#}&@@@Position[str,"["];
  bbb=SequenceCases[str,{a_,"["}:>{"[",a}];
  ccc=Rule@@@Partition[Flatten[Transpose/@Transpose[{aaa,bbb}]],2];
  ddd=Insert[ReplacePart[str,ccc]," ",{#+1}&@@@Position[str,"["]];
  eee=ToLowerCase@Insert[ddd," ",Position[ddd,"]"]];
  StringJoin[(eee/.{"["->"(","]"->")",","->" ","hold"->""})[[4;;-3]]]];
trans=If[Head@WolframLanguageData[ToString@#,"Translations"]===Missing,Nothing,
  ToString@#->Entity["WritingScript","SimplifiedChinese::zzc7y"]/.WolframLanguageData[ToString@#,"Translations"]]&;
LispForm[exp_,"匿天算"]:=Block[{ml,str,tra,trap,aaa,bbb,ccc,ddd,eee},
  ml=ImportString@ExportString[FullForm[Hold@exp],"MathML"];
  str=Cases[ml,_String,-1];
  tra=trans/@DeleteCases[Quiet@Union[ToExpression/@str],Hold];
  trap=tra~Join~{"["->"阴","]"->"阳",","->" ","Hold"->"匿天演算式"};
  aaa={#-1,#}&@@@Position[str,"["];
  bbb=SequenceCases[str,{a_,"["}:>{"[",a}];
  ccc=Rule@@@Partition[Flatten[Transpose/@Transpose[{aaa,bbb}]],2];
  ddd=Insert[ReplacePart[str,ccc]," ",{#+1}&@@@Position[str,"["]];
  eee=Insert[ddd," ",Position[ddd,"]"]];
  StringJoin[eee/.trap]];



(* ::Subsubsection:: *)
(*三角形式排列*)
Options[TriangleForm]={ColorFunction->ColorDataFunction["Black","Gradients",{0,1},If[#1<0,#1,Black]&]};
TriangleForm[triArray_List,OptionsPattern[]]:=Block[{n=Length[triArray]},
  Graphics[MapIndexed[Text[Style[#1,Large,OptionValue[ColorFunction]
  [(Min[triArray]-#1)/Subtract@@MinMax[triArray]]],
    {Sqrt[3]*(n-1+#2.{-1,2}),3*(n-First[#2]+1)}/2]&,triArray,{2}]]];



(* ::Subsubsection:: *)
(*数字上色代码*)
Options[ColorForm]={Form->StandardForm,Color->"TemperatureMap"};
ColorForm[expr_,OptionsPattern[]]:=With[{colored=DisplayForm[ToBoxes[expr,OptionValue[Form]]/.s_String:>
    With[{x=Quiet@ToExpression@s},RowBox@List@StringReplace[ToBoxes@s,
      (ToString[#]->"\*\n"<>ToString@ToBoxes@Style[#,ColorData[OptionValue[Color]]
      [If[NumberQ[OptionValue[Color]],#,#/10]]])&/@Range[0,9]]/;MatchQ[x,_Real|_Integer]]]},Interpretation[colored,expr]];


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];