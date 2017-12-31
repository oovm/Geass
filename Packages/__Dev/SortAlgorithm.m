(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: SortAlgorithm *)
(* :Context: SortAlgorithm` *)
(* :Author: GalAster *)
(* :Date: 2016-10-05 *)

(* :Package Version: 1.4 *)
(* :Update: 2016-11-13 *)
(* :Mathematica Version: 11.0+ *)
(* :Copyright:该软件包遵从CC协议:BY+NA+NC(署名、非商业性使用、相同方式共享） *)
(* :Keywords: *)
(* :Discussion: *)



BeginPackage["BiGridGenerator`SortAlgorithm`"];
ShellSort::usage = "ShellSort[List]给出List的希尔排序过程追踪.";
BubbleSort::usage = "BubbleSort[List]给出List的冒泡排序过程追踪.";
InsertionSort::usage = "InsertionSort[List]给出List的插入排序过程追踪.";
CocktailSort::usage = "CocktailSort[List]给出List的鸡尾酒排序过程追踪.";
BogoSort::usage = "BogoSort[List]给出List的量子猴排过程追踪.";
QuickSort::usage = "QuickSort[List]给出List的快速排序过程追踪.";
SortPlay::usage = "SortPlay[{List}]使用柱状图播放排序过程";
SortDraw::usage = "SortDraw[{List}]使用状态矩阵显示排序过程";
SortShow::usage = "SortShow[{List}]使用线路图显示排序过程";
SortPlot::usage = "SortPlot[{List}]使用对应点状图显示排序过程";
BeadSortStep::usage = "BeadSortStep[Matrix]给出状态矩阵Matrix的珠排的逐步过程.";
BeadSort::usage = "BeadSort[List]给出List的珠排过程追踪.";
BeadPlay::usage = "BeadPlay[List]播放List的状态矩阵的排序过程";
BatcherNet::usage = "BatcherNet[n]显示对n个输入排序时的合并交换排序网络";
InsertionNet::usage = "InsertionNet[n]显示对n个输入排序时的插入排序排序网络";
OddEvenTranspositionNet::usage = "OddEvenTranspositionNet[n]显示对n个输入排序时的奇偶归并排序网络";
PairwiseNet::usage = "PairwiseNet[n]显示对n个输入排序时的成对比较排序网络";
OptimalNet::usage = "OptimalNet[n],n<=10时输出完美排序网络,10<n<=16输出理想排序网络,n>16时套用最佳的Batcher排序网络.";
NetEfficiency::usage = "NetEfficiency[net]分析排序网络net的工作效率.";
NetShow::usage = "NetShow[n,net,input]大小为n的排序网络,选用排序网络算法net,input为待排序数据.";



Begin["`Private`"];
ShellSort[start_]:=Block[{liShell,SSort},
  liShell={};liShell=Append[liShell,start];
  SSort[lst_]:=Block[{list=lst,incr,temp,i,j},
    incr=Round[Length[list]/2];
    While[incr>0,For[i=incr+1,i<Length[list]+1,i++,temp=list[[i]];j=i;
    While[(j>=(incr+1))&&(list[[j-incr]]>temp),
      list[[j]]=list[[j-incr]];j=j-incr;];
    list[[j]]=temp;liShell=Append[liShell,list];];
    If[incr==2,incr=1,incr=Round[incr/2.2]]]];
  SSort[start];liShell];
BubbleSort[start_]:=Block[{liBubble,BuSort},
  liBubble={};liBubble=Append[liBubble,start];
  BuSort[array_]:=Block[{arr=array,swapped=False,n=Length[array]},
    Do[If[arr[[i]]>arr[[i+1]],temp=arr[[i]];
    arr[[i]]=arr[[i+1]];arr[[i+1]]=temp;swapped=True;
    liBubble=Append[liBubble,arr];],{i,1,n-1}];
    While[swapped==True,swapped=False;
    Do[If[arr[[i]]>arr[[i+1]],temp=arr[[i]];
    arr[[i]]=arr[[i+1]];arr[[i+1]]=temp;swapped=True;
    liBubble=Append[liBubble,arr];],{i,1,n-1}]]];
  BuSort[start];liBubble];
InsertionSort[start_]:=Block[{liInsert,ISort},
  liInsert={};liInsert=Append[liInsert,start];
  ISort[array_]:=Block[{arr=array},
    For[i=2,i<=Length[arr],i++,value=arr[[i]];j=i-1;
    While[j>=1&&arr[[j]]>value,arr[[j+1]]=arr[[j]];j--;];
    arr[[j+1]]=value;
    liInsert=Append[liInsert,arr]]];
  ISort[start];liInsert];
CocktailSort[start_]:=Block[{liCocktail,CSort},
  liCocktail={};liCocktail=Append[liCocktail,start];
  CSort[array2_]:=Block[{arr=array2},swapped=True;
  While[swapped==True,swapped:=False;
  For[i=1,i<Length[arr]-1,i++,
    If[arr[[i]]>arr[[i+1]],temp=arr[[i]];
    arr[[i]]=arr[[i+1]];arr[[i+1]]=temp;swapped=True;
    liCocktail=Append[liCocktail,arr];]];
  If[swapped==False,Break[]];
  swapped:=False;
  For[i=Length[arr]-1,i>0,i--,
    If[arr[[i]]>arr[[i+1]],temp=arr[[i]];
    arr[[i]]=arr[[i+1]];arr[[i+1]]=temp;swapped:=True;
    liCocktail=Append[liCocktail,arr];]];]];
  CSort[start];liCocktail];
BogoSort[start_]:=Block[{l,time,ans,res},
  l=Length@start;time=RandomVariate@PoissonDistribution[l l!];
  res=DeleteCases[Table[RandomSample[start],{t,0,
    RandomVariate@PoissonDistribution[l^2]}],Sort@start]~Join~{Sort@start};
  ans=Sort@Union@RandomInteger[time,Ceiling@Sqrt@Length@res];
  Print["我们观测了"<>ToString[time]<>"个平行宇宙后得到了最终的结果,接下来我们输出编号"<>
      ToString[ans]<>"等"<>ToString[Length[res]]<>"个宇宙中的观测结果:"];res];
QuickSort[s_]:=Block[{head,tail,qsort,t0},
  head[{x_,xs___}]:=Select[{xs},#<=x&];
  tail[{x_,xs___}]:=Select[{xs},#>x&];
  qsort[{}]={};
  qsort[l:{x_,___}]:=
      Block[{lh,lt},(Sow@{l,lh=head@l,x,lt=tail@l};
      Join[qsort@lh,{x},qsort@lt])];
  t0=(Reap@qsort@s)[[2,1]]/.{l_,h_,x_,t_}:>Thread[l->Flatten@{h,x,t}];
  t0=t0//.HoldPattern[x_->x_]|{}->Sequence[];
  FoldList[#1/.#2&,s,t0]];
Options[SortPlay]={ChartStyle->"DarkRainbow"};
SortPlay[startlist_,OptionsPattern[]]:=
    Animate[BarChart[startlist[[n]],
      ChartStyle->OptionValue[ChartStyle],ImageSize->Large],{n,1,
      Length@startlist,1},AnimationRunning->False,
      DisplayAllSteps->True];
Options[SortDraw]={ColorFunction->"TemperatureMap",AspectRatio->Automatic};
SortDraw[startlist_,OptionsPattern[]]:=
    ArrayPlot[startlist,ColorFunction->OptionValue[ColorFunction],
      AspectRatio->OptionValue[AspectRatio]];
Options[SortShow]={AspectRatio->Automatic};
SortShow[startlist_,OptionsPattern[]]:=Block[{n},
  n=Length[startlist[[1]]];
  Graphics[MapIndexed[{Thickness[0.1/n],Hue[Last[#2]/n],Line[#1],Black,
    If[n<20,Function[s,Text[First[#2],s]]/@#1,{}]}&,
    Transpose[MapIndexed[{First[#2],#1}&,startlist,{2}]]],
    AspectRatio->OptionValue[AspectRatio],ImageSize->Full]];
Options[SortPlot]={Joined->False,ColorFunction->"Rainbow"};
SortPlot[startlist_,OptionsPattern[]]:=Block[{p},
  p=Range@Length[startlist[[1]]];
  Animate[ListPlot[(Transpose@{p,#}&/@startlist)[[n]],
    ImageSize->Large,ColorFunction->OptionValue[ColorFunction],
    Joined->OptionValue[Joined]],
    {n,1,Length@ShellSort[Start],1},AnimationRunning->False,
    DisplayAllSteps->True]];
BeadSortStep[m_List,behavior_]:=Transpose[(Flatten[#1//.Switch[behavior,
      "延迟匀速",{{x___,1,0,y___}:>{x,{0,1},y},{x___,0,-1,y___}:>{x,{-1,0},y}},
      "延迟光速",{{x___,1,z:Longest[(0)..],y___}:>{x,{z,1},y},{x___,z:Longest[(0)..],-1,y___}:>{x,{-1,z},y}},
      "瞬时匀速",{{x___,u:Longest[(1)..],0,y___}:>{x,{0,u},y},{x___,0,u:Longest[(-1)..],y___}:>{x,{u,0},y}},
      "瞬时光速",{{x___,1,0,y___}:>{x,0,1,y},{x___,0,-1,y___}:>{x,-1,0,y}}]]&)/@Transpose[m]];
BeadSortStep[m_List]:=Transpose[(Flatten[#1//.{{x___,1,0,y___}:>{x,{0,1},y},{x___,0,-1,y___}:>{x,{-1,0},y}}]&)/@Transpose[m]];
BeadSort[start_,behavior_:"延迟匀速"]:=Block[{BeadArray},
  BeadArray=(PadRight[Table[1,{i,#1}],Length@start]&)/@start;
  Map[Total,FixedPointList[BeadSortStep[#,behavior]&,BeadArray],{2}]];
BeadPlay[start_,behavior_:"延迟匀速"]:=Block[{BeadArray},
  BeadArray=(PadRight[Table[1,{i,#1}],Length@start]&)/@start;
  ListAnimate[ArrayPlot/@FixedPointList[BeadSortStep[#,behavior]&,BeadArray],
    AnimationRunning->False,DisplayAllSteps->True,AnimationRepetitions->1]];
PairSort[list_,{i_,j_}]:=Block[{t=list},If[t[[i]]>t[[j]],{t[[i]],t[[j]]}={t[[j]],t[[i]]}];t];
MultiPairSort[list_,pairs_]:=Fold[PairSort,list,pairs];
ApplySorting[net_,list_]:=Fold[MultiPairSort,list,net];
ApplySortingList[net_,list_]:=FoldList[MultiPairSort,list,net];
NetworkGraphics[net_,seq_]:=Block[{n=Max[net],net1=Flatten[net,1],len},
  len=Length[net1];Graphics[{Map[Reverse,If[seq===None,Table[Line[{{0,y},{len+1,y}}],{y,n}],
    MapIndexed[{Thickness[.1/Length[First[seq]]],ColorData["Rainbow"][#[[1]]/n],
      Line[{{#[[2,1]],#2[[1]]},{#[[2,2]],#2[[1]]}}]}&,
      Map[Function[x,With[{lengths=Length/@Split[x]},{#[[1]],{#[[3]]-#[[2]],#[[3]]}}&/@
          Thread[{Map[First,Split[x]],lengths,Accumulate[lengths]}]]],
        Transpose[seq]],{-3}]]],{GrayLevel[.4],If[seq=!=None,Thickness[.1/Length[First[seq]]],{}],
    MapIndexed[Line[{{#2[[1]],#1[[1]]},{#2[[1]],#1[[2]]}}]&,net1,{-2}]}},PlotRange->All,AspectRatio->1/3,ImageSize->Full]];
BatcherNet[n_Integer]/;n>=2:=Block[{q,r,d,p,res={}},Table[d=2^t;p=2^t;r=0;q=2^(Ceiling[Log[2,n]]-1);ExchangeLoop[n],{t,Ceiling[Log[2,n]]-1,0,-1}];DeleteCases[res,Null,\[Infinity]]];
ExchangeLoop[n_Integer]:=(res=Append[res,Table[If[BitAnd[i,2^t]==r,{i+1,i+d+1}],{i,0,n-d-1}]];If[q!=p,d=q-p;q=q/2;r=p;ExchangeLoop[n]]);
InsertionNet[n_]:=With[{m=Log[2,n]},(Join[#1,Rest[Reverse[#1]]]&)[Rest[Flatten[Transpose[
  {Table[{i,i+1},{k,1,n,2},{i,2,k,2}],Table[{i,i+1},{k,1,n,2},{i,1,k,2}]}],1]]]]/;IntegerQ[Log[2,n]];
OddEvenTranspositionNet[n_]:=Flatten[Table[{Table[{i,i+1},{i,1,n-1,2}],Table[{i,i+1},{i,2,n-2,2}]},{n/2}],1]/;IntegerQ[Log[2,n]];
PairwiseNet[1]:={};
PairwiseNet[n_]/;IntegerQ[Log[2,n]]&&n>=2:=
    Block[{res},Join[{Table[{2i-1,2i},{i,1,n/2}]},
      With[{s=PairwiseNet[n/2]},Join[Map[2#-1&,s,{2}],Map[2#&,s,{2}]]],
      Table[Table[{2j,2(j+n/2^i)-1},{j,1,n/2-n/2^i}],{i,2,Log[2,n]}]]];
OptimalNet[9]={{{1,2},{4,5},{7,8}},{{2,3},{5,6},{8,9}},{{1,
  2},{4,5},{7,8},{3,6}},{{1,4},{2,5},{6,9}},{{4,
  7},{5,8},{3,6}},{{1,4},{2,5},{6,8},{3,7}},{{2,
  4},{5,7}},{{3,5},{6,7}},{{3,4}}};
OptimalNet[10]={{{5,10},{4,9},{3,8},{2,7},{1,6}},{{2,5},{7,
  10},{1,4},{6,9}},{{1,3},{4,7},{8,10}},{{1,2},{3,
  5},{6,8},{9,10}},{{2,3},{5,7},{8,9},{4,6}},{{3,
  6},{7,9},{2,4},{5,8}},{{3,4},{7,8}},{{4,5},{6,
  7}},{{5,6}}};
(*Thanks to @Stephen Wolfram*)
(*We just know the OptimalSortNetwork under 10*)
(*https://arxiv.org/pdf/1405.5754v3.pdf*)
OptimalNet[11]={{{1,2},{3,4},{5,6},{7,8},{9,10}},{{2,4},{6,
  8},{1,3},{5,7},{9,11}},{{2,3},{6,7},{10,11},{1,
  5},{4,8}},{{2,6},{7,11},{5,9}},{{6,10},{3,7},{1,
  5},{4,9}},{{2,6},{7,11},{3,4},{9,10}},{{2,5},{8,
  11},{4,6},{7,9}},{{3,5},{8,10},{6,7}},{{4,5},{8,
  9}}};
OptimalNet[12]={{{1,2},{3,4},{5,6},{7,8},{9,10},{11,12}},{{2,
  4},{6,8},{10,12},{1,3},{5,7},{9,11}},{{2,3},{6,
  7},{10,11},{1,5},{8,12}},{{2,6},{7,11},{4,8},{5,
  9}},{{6,10},{3,7},{1,5},{8,12},{4,9}},{{2,6},{7,
  11},{3,4},{9,10}},{{2,5},{8,11},{4,6},{7,9}},{{3,
  5},{8,10},{6,7}},{{4,5},{8,9}}};
OptimalNet[13]={{{2,8},{10,12},{4,5},{6,9},{1,13},{3,7}},{{1,
  2},{3,4},{5,7},{9,12},{8,13},{6,10}},{{1,3},{4,
  8},{11,12},{2,5},{7,13}},{{8,9},{12,13},{5,10},{7,
  11}},{{4,5},{6,7},{9,10},{11,12},{2,8}},{{3,7},{10,
  12},{2,4},{5,8},{9,11},{1,6}},{{3,6},{7,9},{10,
  11}},{{2,3},{4,6},{8,9},{5,7}},{{3,4},{5,6},{7,
  8},{9,10}},{{4,5},{6,7}}};
OptimalNet[14]={{{1,2},{3,4},{5,6},{7,8},{9,10},{11,12},{13,
  14}},{{1,3},{5,7},{9,11},{2,4},{6,8},{10,12}},{{1,
  5},{9,13},{2,6},{10,14},{3,7},{4,8}},{{1,9},{2,
  10},{3,11},{4,12},{5,13},{6,14}},{{6,11},{7,10},{4,
  13},{8,12},{2,3},{5,9}},{{2,5},{8,14},{3,9},{6,
  7},{10,11}},{{3,5},{12,14},{4,9},{8,13}},{{7,9},{11,
  13},{4,6},{8,10}},{{4,5},{6,7},{8,9},{10,11},{12,
  13}},{{7,8},{9,10}}};
OptimalNet[15]={{{1,2},{3,4},{5,6},{7,8},{9,10},{11,12},{13,
  14}},{{1,3},{5,7},{9,11},{13,15},{2,4},{6,8},{10,
  12}},{{1,5},{9,13},{2,6},{10,14},{3,7},{11,15},{4,
  8}},{{1,9},{2,10},{3,11},{4,12},{5,13},{6,14},{7,
  15}},{{6,11},{7,10},{4,13},{14,15},{8,12},{2,3},{5,
  9}},{{2,5},{8,14},{3,9},{12,15},{6,7},{10,11}},{{3,
  5},{12,14},{4,9},{8,13}},{{7,9},{11,13},{4,6},{8,
  10}},{{4,5},{6,7},{8,9},{10,11},{12,13}},{{7,8},{9,
  10}}};
OptimalNet[16]={{{1,2},{3,4},{5,6},{7,8},{9,10},{11,12},{13,
  14},{15,16}},{{1,3},{5,7},{9,11},{13,15},{2,4},{6,
  8},{10,12},{14,16}},{{1,5},{9,13},{2,6},{10,14},{3,
  7},{11,15},{4,8},{12,16}},{{1,9},{2,10},{3,11},{4,
  12},{5,13},{6,14},{7,15},{8,16}},{{6,11},{7,10},{4,
  13},{14,15},{8,12},{2,3},{5,9}},{{2,5},{8,14},{3,
  9},{12,15},{6,7},{10,11}},{{3,5},{12,14},{4,9},{8,
  13}},{{7,9},{11,13},{4,6},{8,10}},{{4,5},{6,7},{8,
  9},{10,11},{12,13}},{{7,8},{9,10}}};
OptimalNet[n_]:=BatcherNet[n];
NetEfficiency[net_]:={Length@Flatten[net,1],Max[(Transpose@Tally@Flatten@net)[[2]]]};
NetShow::undef="尚未发现大于10的完美排序网络,若n>16时仍输出Batcher网络.";
NetShow[nn_,net_:OptimalNet,input_:None]:=Block[{n,s},
  If[nn>10&&net===OptimalNet,Message[NetShow::undef]];
  n=Switch[net,OddEvenTranspositionNet,2^Round[Log[2,nn]],
    InsertionNet,2^Round[Log[2,nn]],
    PairwiseNet,2^Round[Log[2,nn]],
    BatcherNet,nn,
    OptimalNet,nn];
  s=net[n];
  If[input===None,NetworkGraphics[s,None],
    NetworkGraphics[s,Quiet@ApplySortingList[List/@Flatten[s,1],input]]]];
End[];
SetAttributes[ToExpression@Names["BiGridGenerator`SortAlgorithm`*"],{Protected,ReadProtected,Locked}];
EndPackage[]
