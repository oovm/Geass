(* ::Package:: *)
(* ::Title:: *)
(*ExNumber(特殊数论包)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template*)
(**)
(*Author:GalAster*)
(*Creation Date:2016-11-11*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["ExNumber`"];
MultPrime::usage = "PlusPrime[n]生成n以内所有可以由两个素数相乘得到的整数";
PlusPrime::usage = "PlusPrime[n]生成n以内所有可以由两个素数相加得到的整数";
ManyPrime::usage = "ManyPrime[n]生成n以内所有可以由s个素数相乘得到的整数";
DisplaySum::usage = "DisplaySum[f[n],{n,a,b}]显示这个级数的和";
ImproperSum::usage = "ImproperSum[f[n]]尝试各种手段对f[n]进行无穷求和";
RTCount::usage="RTCount[max]对小于max的整数可构成的直角三角形计数\r
    RTCount[max,Return->True]返回具体的每个整数的计数\r
    RTCount[min,max]返回区间[min,max]中的计数.";
SumProdPartitions::usage="SumProdPartitions[n]给出整数n的积和分解\r
    SumProdPartitions[n,Show->False],不显示分解出的1.";
SumProdNumber::usage="SumProdNumber[max]给出小于max的整数的最小积和数集合\r
    SumProdNumber[max,s],s代表搜索深度,太小可能会导致丢解,默认为6,可设为Infinite,但是速度会变得很慢";
DigitReplacePrime::usage="DigitReplacePrime[n,m,p]在n位数搜索交换m位的p元质数组.";
BaileyP::usage="BaileyP[1,16,8,{4,0,0,-2,-1,-1,0,0}]//Activate
    %//FullSimplify";

(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
ExNumber$Version="V1.0";
ExNumber$Environment="V11.0+";
ExNumber$LastUpdate="2016-12-19";
ExNumber::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)




(* ::Subsubsection::Closed:: *)
(*特殊取模函数*)
Unprotect[Mod,PowerMod];
Mod[x_,y_]:=1/;x==1||y==1;
PowerMod[x_,y_,z_]:=1/;x==1||z==1;
(*内置取模函数覆写优化,效果是递归截断*)
(*ET=ExponentialTower*)
ModEt[{a_},n_]:=Mod[a,n];
ModEt[{a_,b_},n_]:=PowerMod[a,b,n];
ModEt[list_?VectorQ,n_]:=Block[
  {a=First@list,bc=Rest@list,g,d,f},
  g=GCD[a,n];d=a/g;f=n/g;
  If[a==1||n==1,Return[1]];
  If[g==1,Return[PowerMod[a,ModEt[bc,EulerPhi[n]],n]]];
  Mod[g Mod[PowerMod[g,Mod[(ModEt[bc,EulerPhi[f]]-1),
    EulerPhi[f]],f]PowerMod[d,ModEt[bc,EulerPhi[f]],f],f],n]];
ModBi::win="错误的输入,请保证所有数都是正数且a>b";
ModBi[r_?IntegerQ,t_,n_]:=Block[
  {{a,b}={r,Min[t,r-t]}},
  If[0<=r<=t,None,Message[ModBi::win]];
  If[PrimeQ@n,Return@LucasTheorem[a,b,n]];
  If[SquareFreeQ@n,Return@SquareFreeMod[a,b,n]];
  AndrewMod[a,b,n]];
LucasTheorem[a_,b_,p_?PrimeQ]:=Block[
  {pn=IntegerDigits[#,p]&/@{a,b},sn},
  sn=PadLeft[#,Max[Length/@pn]]&/@pn;
  sn=PadLeft[#,Max[Length/@pn]]&/@pn;
  Mod[Times@@Mod[Binomial@@@Transpose@sn,p],p]];
(*(*卢卡斯定理正确性检验*)
a=Table[Mod[Binomial[i,j],13],{i,1,100},{j,1,100}];
b=Table[LucasTheorem[i,j,13],{i,1,100},{j,1,100}];
a\[Equal]b*)
SquareFreeMod[a_,b_,p_?SquareFreeQ]:=Block[
  {pp=FactorInteger[p][[All,1]],xx},
  xx=LucasTheorem[a,b,#]&/@pp;
  ChineseRemainder[xx,pp]];
(*(*无平方因子正确性验证*)
a=Table[SquareFreeMod[i,50,230],{i,50,200}];
b=Table[Mod[Binomial[i,50],230],{i,50,200}];
a\[Equal]b*)
ModFa[n_,p_]:=Fold[Mod[#1 #2,p]&,Range[n]];



(* ::Subsubsection::Closed:: *)
(*数盘*)
fontstyle=((Translate[#1,{-4.5,-10}]&)/@First[First[ImportString[ExportString[
  Style[#1,FontSize->24,FontFamily->"Arial"],"PDF"],
  "PDF","TextMode"->"Outlines"]]]&)/@Join[{"."},CharacterRange["0","9"]];
DigitalCycle[num_,digits_:5000,start_:Pi/4,fontsize_:0.0655]:=Block[{list},
  list=Insert[#[[1]],-1,1+#[[2]]]&@RealDigits[num,10,digits];
  Graphics[MapIndexed[With[{angle=(-(#2[[1]]-2)+Switch[#2[[1]],1,-0.1,2,0,_,0.6])*fontsize},
    With[{scale=(1-1.5*fontsize)^(-angle/(2*Pi))},GeometricTransformation[fontstyle[[#1+2]],
      ScalingTransform[{1,1}*0.1*fontsize*scale]/*TranslationTransform[{0,scale}]/*
          RotationTransform[start+angle]]]]&,list],PlotRange->{{-1.1,1.1},{-1.1,1.1}}]];
(*另一种数盘*)
clusterSector[gap_][{{xmin_,xmax_},y_},rest___]:=
    Block[{ngap=Min[(xmax-xmin)/2,gap]},{EdgeForm[White],
      ChartElementData["Sector"][{{xmin+ngap,xmax-ngap},y},rest]}];
iCoord[{i_,j_},bin_:60]:=Through[{Cos,Sin}[Pi/2-\[Pi]/5i-(\[Pi]/5)/bin(j-1)-0.025]];
iCurve[{x_,y_},rad_:15,bin_:60,colorf_:ColorData[35]]:=
    Block[{s,t,range,c1,c2},{s,t}=iCoord[#,bin]&/@{x,y};
    {c1,c2}=colorf/@{x[[1]],y[[1]]};
    range=Range[0,1,.1];
    Line[BezierFunction[rad{s,{0,0}+.4Normalize[(s+t)],t}]/@range,
      VertexColors->(Blend[{c1,c2},#]&/@range)]];
DigitalSector[num_,digits_:1000,style_:35,fontsize_:30]:=
    Block[{digit,count,cdigits,curves},
      digit=First@RealDigits[num,10,digits];
      count=Association[Thread[Range[0,9]->Table[1,10]]];
      cdigits=Partition[{#,count[#]++}&/@digit,2,1];
      curves=iCurve[#,15.5,Max[cdigits],ColorData[style]]&/@cdigits;
      Show[{PieChart[Table[1,10],
        SectorOrigin->{{Pi/2,"Clockwise"},16},PerformanceGoal->"Speed",
        ChartElementFunction->clusterSector[0.02],
        ChartLabels->Placed[Table[Rotate[Style[i,15,White,FontFamily->"Arials"],-(18+36i)Degree],
          {i,0,9}],{1/2,1.8}],ChartStyle->style,Background->Black],
        Graphics[{{Opacity[.4],curves},Text[Style[ToString[num,StandardForm],
          White,fontsize,Bold],{0,0}]}]}]];




(* ::Subsubsection::Closed:: *)
(*未分类函数*)
MultPrime[n_]:=Union@@Table[p*TakeWhile[Prime[Range[PrimePi[n]]],p*#1<n&],{p,TakeWhile[l,#1<Sqrt[n]&]}];
PlusPrime[n_]:=Union@@Table[p+TakeWhile[Prime[Range[PrimePi[n]]],p*#1<n&],{p,TakeWhile[l,#1<Sqrt[n]&]}];
ManyPrime[n_,s_]:=Select[Range[n],PrimeOmega[#1]==s&];
DisplaySum[a_,{n_,n1_,n2_},opts:OptionsPattern[]]/;n1<=n2:=
    Block[{nf=Min[n1+OptionValue["Terms"]-1,n2]},Row[{Defer[Sum[a,{n,n1,n2}]],
      Composition[Defer,Plus]@@Append[Table[a,{n,n1,nf}],If[n2===\[Infinity],"\[CenterEllipsis]",Nothing]],Sum[a,{n,n1,n2}]},"="]];
Summation[fun_]:=Sum[fun,{n,1,Infinity},Regularization->#]&/@{"None","Abel","Euler","Cesaro","Dirichlet","Borel"};
RamanujanSummation[fun_]:=Block[{f},
  f=Function[Evaluate@Variables[Level[fun,{-1}]],Evaluate@fun];
  -(f[0]/2)+I*Integrate[(f[I*t]-f[(-I)*t])/(E^(2*Pi*t)-1),{t,0,Infinity}]];
ImproperSum[function_]:=Block[{ans,name},
  ans=Join[Summation[function],{RamanujanSummation[function]}];
  name={"Cauchy","Abel","Euler","Cesaro","Dirichlet","Borel","Ramanujan"};
  TableForm@Transpose[{name,If[Head@#===Sum,"Undefinited",#]&/@ans}]];

RTCount::eq="你输入的最大值比最小值大,请进行正确的输入.";
Options[RTCount]={Return->False};
RTCount[max_?IntegerQ,OptionsPattern[]]:=Block[
  {CountArray=ConstantArray[0,max]},
  Do[If[CoprimeQ[a^2-b^2,2 a b,a^2+b^2],CountArray[[2a k(a+b)]]++],
    {a,1,Sqrt[max/2]},
    {b,If[OddQ@a,2,1],Min[(max-2 a^2)/(2 a),a-1],2},
    {k,1,max/(2 a (a+b))}];
  Return[If[OptionValue[Return],CountArray,Tally@CountArray]]];
RTCount[min_?IntegerQ,max_?IntegerQ,OptionsPattern[]]:=Block[
  {a=RTCount[max,Return->True],
    i=PadRight[RTCount[min-1,Return->True],max]},
  If[min>=max,Return[Message[RTCount::eq]]];
  Return[If[OptionValue[Return],a-i,Tally@Take[a-i,max-min]]]];
Options[PrimePartitions]={Method->PowerTuple};
PrimePowerTuple[max_,rule_,offset_]:=Block[{sifter},
  sifter[l_,x_]:=Union@@(#+Array[Prime,PrimePi[(max-#)^(1/x)]]^x&/@l);
  Fold[sifter,{offset},Sort[rule,Greater]]];
Options[SumProdPartitions]={Show->True};
SumProdPartitions[n_,OptionsPattern[]]:=
    Block[{div=Take[Divisors[n],Ceiling[Length[Divisors[n]]/2]],ans},
      ans=Drop[Transpose[Append[{div},n/div]],1];
      If[OptionValue[Show],PadRight[#,n-Total@#+2,1]&/@ans,ans]];
SumProdNumber[max_?IntegerQ,bound_:6(*Infinity不丢解,但是太慢了*)]:=
    Block[{ans=ConstantArray[10^bound,2max],factor,p,k},
      ans[[1]]=0;factor[0]=2;
      Do[Table[factor[i]=Range[factor[i-1],Floor[(2max/Product[factor[j],{j,i-1}])^(1/(n+1-i))]],{i,n}];
      p=Product[factor[i],{i,n}];
      k=n-Sum[factor[i],{i,n}]+p;
      MapThread[If[ans[[#]]>#2,ans[[#]]=#2]&,Flatten/@{k,p}];
        ,{n,2,Floor[Log[2,max]]}];
      Union[Take[ans,max]]];


(* ::Subsubsection::Closed:: *)
(*DRP=DigitReplacePrime 数位替换素数组*)
Begin["`DRP`"];
base[k_]:=Array[a,k];
space[k_,i_]:=Subsets[Range[k-1],{i}];
remain[k_,i_]:=Select[IntegerDigits/@Range[10^(k-2-i),10^(k-1-i)-1],Mod[Total[#],3]!=0&];
remplace[k_,i_]:=Complement[Range[k-1],#]&/@space[k,i];
tab[k_,i_]:=Table[base[k]/.(Thread[Rule[base[k][[#]]&/@o,m]]),{o,remplace[k,i]},{m,remain[k,i]}]//Flatten[#,1]&;
lastnum[n_,k_]:=Table[tab[n,k]/.a[n]:>z,{z,{1,3,7,9}}]//Flatten[#,1]&;
d=Table[a[_]->i,{i,0,9}];
f=DeleteCases[#,a_/;First[a]==0]&;
end[n_,k_]:=Map[FromDigits,#,{2}]&@(f/@Outer[ReplaceAll,lastnum[n,k],d,1]);
End[] ;
DigitReplacePrime[n_,m_,p_]:=Select[#,PrimeQ]&/@Sort[Select[DRP`end[n,m],Total@Boole@PrimeQ@#==p&]];




(* ::Subsubsection::Closed:: *)
(*快速平方判定*)
SqrtQ[num_,power_:2]:=Round@Surd[N@num,power]^power==num;
SqrtQSelect[(list_)?VectorQ,power_:2]:=
    Intersection[Array[#^power&,
      Ceiling[Surd[Max[list],power]]-Floor[Surd[Min[list],power]]+1,
      Floor[Surd[Min[list],power]]],list];

BaileyP[s_,b_,n_,A_]:=Block[{k,echo},
  echo=Evaluate[1/b^k  Plus@@(A/Array[(n k+#)&,n])];
  Echo[Inactivate@Sum[echo,{k,0,Infinity}],"和式展开为: "]];


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[]