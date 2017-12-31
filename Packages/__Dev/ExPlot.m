(* ::Package:: *)
(* ::Title:: *)
(*ExPlot(绘图增强包)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template*)
(**)
(*Author:GalAster*)
(*Creation Date:2016-05-12*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["ExPlot`"];
InfinitePlot::usage = "一种把无穷函数映射到单位正方形的变换,用法基本同Plot\r
  InfinitePlot[Method->List],使用点绘图,对于波动大的函数效果较好,但是速度较慢.";
CPlanePlot::usage = "CPlanePlot[Function,{Xmin,Xmax,dX},{Ymin,Ymax,dY}]给出一个函数的复平面图像.\r
  其中Function必须是个纯函数.如Sin或#^2&等等,后面几个参数分别表示绘图区域和步长\r\r
  CPlanePlot[Function,{Xmin,Xmax,dX},{Ymin,Ymax,dY},Options]\r
  本函数继承了ArrayPlot,所以Options里可以填ArrayPlot的选项,不过除了ColorFunction选项外都支持不良:\r
  CPlanePlot[Tan,{-4,4,0.02},{-4,4,0.02},Frame->False,ColorFunction->\"Rainbow\"]";
CMapPlot::usage = "CMapPlot[Function,{Xmin,Xmax,dX},{Ymin,Ymax,dY}]给出一个函数的复平面映射.\r
  其中Function必须是个纯函数.如Sin或#^2&等等,后面几个参数分别表示绘图区域和步长\r\r
  CMapPlot[Function,{Xmin,Xmax,dX},{Ymin,Ymax,dY},Options]\r
  本函数继承了ListLinePlot,所以Options里可以填ListLinePlot的选项,选项基本上支持良好:\r
  CMapPlot[Tan,{0,Pi,Pi/40},{-1,1,2/20}]\r
  CMapPlot[ArcTan,{0,Pi,Pi/40},{0,2,2/20},PlotTheme->\"Detailed\",ColorFunction->\"Rainbow\",PlotLabel->\"z->ArcTan[z]\"]";
C3DPlot::usage = "C3DPlot[f(z),{z,Zmin,Zmax},Options]给出一个复函数的模投影,从复二维投影到实三维.\r
  本函数继承了Plot3D,所以Options里可以填Plot3D的选项,选项基本上支持良好:\r
  C3DPlot[Gamma[z],{z,-3.5-3.5I,3.5+5.5I},PlotRange->{0,4}]";
PiecewisePlot::usage="PiecewisePlot可以有效绘制分段函数以及反常函数,选项基本和Plot相同.";
Periodization::usage="Periodization[f(x),{x,a,b}]周期化一个单变量函数.";
Gray3DPlot::usage = ".";
WavePlot::usage = ".";
GEBPlot::usage = ".";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
ExPlot$Version="V0.5";
ExPlot$Environment="V11.0+";
ExPlot$LastUpdate="2016-10-17";
ExPlot::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)



(* ::Subsubsection:: *)
(*无穷->单位框的映射*)
InfinitePlot[list_]:=Block[{funs,func,tick,axis,coor},
  funs=ArcTan[Function[x,#][Tan[x]]]&/@list;
  func=Plot[Evaluate@funs,{x,-Pi/2,Pi/2},AspectRatio->1,Axes->False,PlotLegends->list,PerformanceGoal->"Quality",MaxRecursion->15];
  tick=N@ConstantArray[Pi/2,12]-((Pi/2^#)&/@Range[12]);
  axis={{{0,0},{Pi/2.0,0}}}~Join~({{#,0.02},{#,-0.02}}&/@tick);
  coor=Graphics[Line/@((#.RotationMatrix[Pi/2])~Join~#)&@(axis~Join~-axis)];
  Show[func,coor,ImageSize->Large]];
InfinitePlot[list_,Method->List]:=
    Block[{funs,xxx,data,func,tick,axis,coor},
      funs=ArcTan[Function[x,#][Tan[x]]]&/@list;
      xxx=Subdivide[-Pi/2,-Pi/4.0,800]~Join~Subdivide[-Pi/4,Pi/4.0,400]~Join~Subdivide[Pi/4,Pi/2.0,800];
      data=Transpose[{xxx,Function[x,#]/@xxx}]&/@funs;
      func=ListLinePlot[data,AspectRatio->1,Axes->False,PlotLegends->list,PlotRangeClipping->False];
      tick=N@ConstantArray[Pi/2,12]-((Pi/2^#)&/@Range[12]);
      axis={{{0,0},{Pi/2.0,0}}}~Join~({{#,0.02},{#,-0.02}}&/@tick);
      coor=Graphics[Line/@((#.RotationMatrix[Pi/2])~Join~#)&@(axis~Join~-axis)];
      Show[func,coor,ImageSize->Large]];



(* ::Subsubsection:: *)
(*大小切换绘图*)
FunQ={ConstantArray[#1,Length@{##2}],{##2}}&@@@NestList[RotateLeft,#,Length@#-1]&;
FunGE[{a_,b_}]:=If[Inner[GreaterEqual,a,b,And],a[[1]],I];
FunLE[{a_,b_}]:=If[Inner[LessEqual,a,b,And],a[[1]],I];
MaxPlot[funcs_,range_,ops___]:=Plot[Evaluate[FunGE/@(FunQ@funcs)],range,ops];
MinPlot[funcs_,range_,ops___]:=Plot[Evaluate[FunLE/@(FunQ@funcs)],range,ops];
MaxPlot3D[funcs_,range_,ops___]:=Plot3D[Evaluate[FunGE/@(FunQ@funcs)],range,ops];
MinPlot3D[funcs_,range_,ops___]:=Plot3D[Evaluate[FunLE/@(FunQ@funcs)],range,ops];
(*Thanks to @Apple*)



(* ::Subsubsection:: *)
(*复平面绘图*)
CPlanePlot[f_,{x0_,x1_,dx_},{y0_,y1_,dy_},ops:OptionsPattern[ArrayPlot]]:=
    Block[{fun,data,RE,IM,ABS},
      fun=Table[f[x+I y+.0],{x,x0,x1,dx},{y,y0,y1,dy}];
      data=2ArcTan[#@fun]/Pi&/@{Re,Im,Abs};
      {RE,IM,ABS}=ArrayPlot[#,FilterRules[{ops,ColorFunction->"TemperatureMap"},Options[ArrayPlot]]]&/@data;
      GraphicsGrid[{{RE,ABS,SpanFromLeft},{IM,SpanFromAbove,SpanFromBoth}},ImageSize->Large]];
Options[CMapPlot]={AspectRatio->Automatic};
CMapPlot[f_,{x0_,x1_,dx_},{y0_,y1_,dy_},ops:OptionsPattern[{CMapPlot,ListLinePlot}]]:=
    Block[{x,y,tx,ty,llpx,llpy},
      tx=Table[{Re[f[x+I y+.0]],Im[f[x+I y+.0]]},{x,x0,x1,dx},{y,y0,y1,dy}];
      ty=Table[{Re[f[x+I y+.0]],Im[f[x+I y+.0]]},{y,y0,y1,dy},{x,x0,x1,dx}];
      {llpx,llpy}=Quiet@ListLinePlot[#,FilterRules[{ops,InterpolationOrder->2},Options[ListLinePlot]]]&/@{tx,ty};
      Show[llpx,llpy,ImageSize->Large,AspectRatio->OptionValue[AspectRatio]]];
C3DPlot[f_,range_,ops:OptionsPattern[Plot3D]]:=
    Block[{rangerealvar,rangeimagvar,g},
      g[r_,i_]:=(f/.range[[1]]:>r+I i);
      Plot3D[Abs[g[rangerealvar,rangeimagvar]],
        {rangerealvar,Re[range[[2]]],Re[range[[3]]]},
        {rangeimagvar,Im[range[[2]]],Im[range[[3]]]},
        Evaluate@FilterRules[{ops,ColorFunction->(Hue[Mod[Arg[g[#1,#2]]/(2*Pi)+1,1]]&),
          ColorFunctionScaling->False},Options[Plot3D]]]];



(* ::Subsubsection:: *)
(*分段函数绘图*)
PiecewisePlot::limindet="Limit `` is not numeric or infinite at ``";
PiecewisePlot::nonpw="Function `` is not a Piecewise function or did not expand to one";
(*2015-7-13  Get From
http://mathematica.stackexchange.com/questions/39445/plot-a-piecewise-function-with-black-and-white-disks-marking-discontinuities/39466#39466*)
PiecewisePlot`debug::debug="``";
PiecewisePlot`debug::plot="``";
PiecewisePlot`debug::annotation="``";
PiecewisePlot`debug::limit="``";
PiecewisePlot`debug=Hold[PiecewisePlot`debug::debug,PiecewisePlot`debug::plot,PiecewisePlot`debug::annotation,PiecewisePlot`debug::limit];
Off@@PiecewisePlot`debug;
Options[PiecewisePlot]=Join[{"DotSize"->Automatic,"EmptyDotStyle"->Automatic,"FilledDotStyle"->Automatic,"AsymptoteStyle"->Automatic,"BaseDotSize"->Offset[{2,2}],"AdditionalPoints"->{},
(*addition pts to annotate*)"PiecewiseExpand"->Automatic,(*which fns.to expand*)"ContinuousEndpoints"->Automatic},(*eval.formula,not limit*)Options[Plot]];
Options[EmptyDot]=Options[FilledDot]=Options[Asymptote]=Options[PiecewisePlot`plot]=Options[PiecewisePlot`init]=Options[PiecewisePlot];
(*graphics elements*)
EmptyDot[pt_,opts:OptionsPattern[]]/;OptionValue["EmptyDotStyle"]===None:={};
FilledDot[pt_,opts:OptionsPattern[]]/;OptionValue["FilledDotStyle"]===None:={};
Asymptote[pt_,opts:OptionsPattern[]]/;OptionValue["AsymptoteStyle"]===None:={};
EmptyDot[pt_,opts:OptionsPattern[]]:={White,OptionValue["EmptyDotStyle"]/.Automatic->{},Disk[pt,OptionValue["DotSize"]/.Automatic->OptionValue["BaseDotSize"]]};
FilledDot[pt_,opts:OptionsPattern[]]:={OptionValue["FilledDotStyle"]/.Automatic->{},Disk[pt,OptionValue["DotSize"]/.Automatic->OptionValue["BaseDotSize"]]};
Asymptote[x0_,opts:OptionsPattern[]]:={Dashing[Large],OptionValue["AsymptoteStyle"]/.Automatic->{},Line[Thread[{x0,OptionValue[PlotRange][[2]]}]]};
PiecewisePlot`$inequality=Greater|Less|LessEqual|GreaterEqual;
PiecewisePlot`$discontinuousAuto=Ceiling|Floor|Round|Sign;
PiecewisePlot`$discontinuousAll=Ceiling|Floor|Round|Sign|(*Min|Max|Clip|*)UnitStep|IntegerPart|(*FractionalPart|*)Mod|Quotient|UnitBox|UnitTriangle|SquareWave
(*|TriangleWave|SawtoothWave*)(*|BernsteinBasis|BSplineBasis|Abs|If|Which|Switch*);
PiecewisePlot`$discontinuous=Ceiling|Floor|Round|Sign;
(*auxiliary functions*)
(*causes Conditional solutions to expand to all possibilities;
(arises from trig eq,and C[1]-- perhaps C[2],etc?*)
PiecewisePlot`expand[cond_Or,var_]:=PiecewisePlot`expand[#,var]&/@cond;
PiecewisePlot`expand[cond_,var_]:=Reduce[cond,var,Backsubstitution->True];
PiecewisePlot`solve[eq_,var_]/;MemberQ[eq,PiecewisePlot`$discontinuous,Infinity,Heads->True]:=PiecewisePlot`solve[#==C[1]&&C[1]\[Element]Integers&&And@@Cases[eq,Except[_Equal]],var]&/@
    Cases[eq,PiecewisePlot`$discontinuous[e_]:>e,Infinity];
PiecewisePlot`solve[eq_,var_]:={var->(var/.#)}&/@List@ToRules@PiecewisePlot`expand[Reduce[eq,var,Reals,Backsubstitution->True],var]/.{False->{}};
(*limit routines for handling discontinuous functions,which Limit fails to do*)
Needs["NumericalCalculus`"];
PiecewisePlot`nlimit[f_?NumericQ,var_->x0_,dir_]:=f;
PiecewisePlot`nlimit[f_,var_->x0_,dir_]:=NLimit[f,var->x0,dir];
PiecewisePlot`limit[f_,var_->x0_,dir_]/;MemberQ[Numerator[f],PiecewisePlot`$discontinuous,Infinity,Heads->True]:=Block[{y0,f0},f0=f//.(disc:PiecewisePlot`$discontinuous)[z_]/;
    FreeQ[z,PiecewisePlot`$discontinuous]:>disc[With[{dz=Abs[D[z,var]/.var->N@x0]},Mean[{z/.var->N@x0,z/.var->x0-0.1 Last[dir]/Max[1,dz]}]]];
Message[PiecewisePlot`debug::limit,{f0,f,var->x0,dir}];
Quiet[Check[y0=PiecewisePlot`nlimit[f0,var->x0,dir],Check[y0=Limit[f0,var->x0,dir],If[!NumericQ[y0],y0=Indeterminate]]],{Power::infy,Infinity::indet,NLimit::noise}];y0];
PiecewisePlot`limit[f_,var_->x0_,dir_]:=Block[{y0},Quiet[Check[y0=f/.var->x0,Check[y0=Limit[f,var->x0,dir],If[!NumericQ[y0],y0=Indeterminate]]],{Power::infy,Infinity::indet}];y0];
PiecewisePlot`$reverseIneq={Less->Greater,Greater->Less,LessEqual->GreaterEqual};
PiecewisePlot`reverseIneq[(rel:PiecewisePlot`$inequality)[args__]]:=(rel/.PiecewisePlot`$reverseIneq)@@Reverse@{args};
PiecewisePlot`inDomain[]:=LessEqual@@PiecewisePlot`domain[[{2,1,3}]];
PiecewisePlot`inDomain[dom_]:=LessEqual@@dom[[{2,1,3}]];
(*annotatedPoints-- returns list of abscissas to be "annotated" with dots/asymptotes boundaryPoints-- returns list of boundaries numbers between pieces interiorPoints-- returns list of points where the denominator is zero*)
PiecewisePlot`annotatedPoints[allpieces_,domain_,additionalpoints_]:=
    DeleteDuplicates@Flatten@Join[PiecewisePlot`boundaryPoints[allpieces,domain],
      PiecewisePlot`interiorPoints[allpieces,domain],additionalpoints];
PiecewisePlot`boundaryPoints[allpieces_,domain:{var_,_,_}]:=With[{conditions=DeleteDuplicates[Equal@@@Flatten[Last/@allpieces/.
    {HoldPattern@Inequality[a_,rel1_,b_,rel2_,c_]:>{PiecewisePlot`reverseIneq[rel1[a,b]],rel2[b,c]},(rel:PiecewisePlot`$inequality)[a_,b_,c_]:>{PiecewisePlot`reverseIneq[rel[a,b]],rel[b,c]}}]]},
  Message[PiecewisePlot`debug::annotation,conditions];
var/.Flatten[(*deletes no soln {}'s*)PiecewisePlot`solve[#&&PiecewisePlot`inDomain[domain],var]&/@conditions,1]/.var->{} (*no BPs in domain*)];
PiecewisePlot`interiorPoints[allpieces_,domain:{var_,_,_}]:=
    MapThread[Function[{formula,condition},
      Flatten[{With[{solns=PiecewisePlot`solve[Denominator[formula,Trig->True]==0&&(condition/.{LessEqual->Less,GreaterEqual->Greater})&&LessEqual@@PiecewisePlot`domain[[{2,1,3}]],PiecewisePlot`var]},
        PiecewisePlot`var/.solns/.PiecewisePlot`var->{}],If[MemberQ[Numerator[formula],PiecewisePlot`$discontinuous,Infinity,Heads->True],
        With[{solns=PiecewisePlot`solve[Numerator[formula]==0&&(condition/.{LessEqual->Less,GreaterEqual->Greater})&&LessEqual@@PiecewisePlot`domain[[{2,1,3}]],PiecewisePlot`var]},
          PiecewisePlot`var/.solns/.PiecewisePlot`var->{}],{}]}]],Transpose@allpieces];
(*sowAnnotations-Sows irregular points,tagged with three ids;
"filled"\[Rule]{x,y};
"empty"\[Rule]{x,y};
"asymptote"\[Rule]x;*)
PiecewisePlot`sowAnnotations[allpieces_,domain:{var_,a_,b_},{}]:={};
PiecewisePlot`sowAnnotations[allpieces_,domain:{var_,a_,b_},points_List]:=(Message[PiecewisePlot`debug::annotation,"sowAnn"->{allpieces,points}];
PiecewisePlot`sowAnnotations[allpieces,domain,##]&@@@Partition[{If[First[#]==a,Indeterminate,a]}~Join~#~Join~{If[Last[#]==b,Indeterminate,b]},3,1]&@SortBy[points,N]);
PiecewisePlot`sowAnnotations[allpieces_,domain:{var_,_,_},xminus_,x0_?NumericQ,xplus_]:=Block[{y0,yplus,yminus,f0,fminus,fplus},f0=First[Pick@@MapAt[#/.var->x0&/@#&,Transpose@allpieces,2]/.{}->{Indeterminate}];
Quiet[y0=f0/.var->N@x0,{Power::infy,Infinity::indet}];
If[xminus=!=Indeterminate,(*xminus\[NotEqual]left endpoint*)fminus=First[Pick@@MapAt[#/.var->Mean[{xminus,x0}]&/@#&,Transpose@allpieces,2]/.{}->{Indeterminate}];
yminus=PiecewisePlot`limit[fminus,var->x0,Direction->1];];
If[xplus=!=Indeterminate,(*xplus\[NotEqual]right endpoint*)fplus=First[Pick@@MapAt[#/.var->Mean[{x0,xplus}]&/@#&,Transpose@allpieces,2]/.{}->{Indeterminate}];
yplus=PiecewisePlot`limit[fplus,var->x0,Direction->-1];];
If[Abs[yminus]==Infinity||Abs[yplus]==Infinity,Sow[x0,"asymptote"]];
If[NumericQ[y0],Sow[{x0,y0},"filled"]];
Message[PiecewisePlot`debug::annotation,{{x0,y0,f0},{xminus,yminus,fminus},{xplus,yplus,fplus}}];
Sow[{x0,#},"empty"]&/@DeleteDuplicates@DeleteCases[Select[{yminus,yplus},NumericQ],y0]];
(*initialization of context variables*)
PiecewisePlot`init[f:HoldPattern@Piecewise[pieces_,default_],domain:{var_,_,_},opts:OptionsPattern[]]:=(PiecewisePlot`domain=SetPrecision[domain,Infinity];
PiecewisePlot`var=var;
PiecewisePlot`allpieces=If[default=!=Indeterminate,Append[pieces,(*add True case to pieces*)
  {default,If[Head[#]===Not,Reduce[#],#]&@Simplify[Not[Or@@(Last/@pieces)]]}],pieces]/.{formula_,HoldPattern@Or[e__]}:>Sequence@@({formula,#}&/@List[e]);
PiecewisePlot`$discontinuous=OptionValue["PiecewiseExpand"]/.{Automatic->PiecewisePlot`$discontinuousAuto,All->PiecewisePlot`$discontinuousAll,None->{}};
Message[PiecewisePlot`debug::debug,"f"->f]);
(*The main plotting function*)
PiecewisePlot`plot[f:HoldPattern@Piecewise[pieces_,default_],domain:{var_,a_,b_},opts:OptionsPattern[]]:=
    Block[{PiecewisePlot`var,PiecewisePlot`domain,PiecewisePlot`allpieces,PiecewisePlot`$discontinuous},
    (*INITIALIZATION:PiecewisePlot`var;
    PiecewisePlot`domain;
    PiecewisePlot`allpieces;
    PiecewisePlot`$discontinuous*)
PiecewisePlot`init[f,domain,opts];
Message[PiecewisePlot`debug::plot,"allpieces"->PiecewisePlot`allpieces];
(*POINTS OF INTEREST*)
With[{annotatedpoints=PiecewisePlot`annotatedPoints[PiecewisePlot`allpieces,PiecewisePlot`domain,
  OptionValue["AdditionalPoints"]],plotopts=FilterRules[{opts},Cases[Options[Plot],Except[Exclusions->_]]]},
  Message[PiecewisePlot`debug::plot,"annotatedpoints"->annotatedpoints];
(*ANNOTATIONS*)
With[{annotations=Last@Reap[PiecewisePlot`sowAnnotations[PiecewisePlot`allpieces,PiecewisePlot`domain,annotatedpoints],
  {"asymptote","empty","filled"}]},Message[PiecewisePlot`debug::plot,Thread[{"asymptote","empty","filled"}->annotations]];
(*PROCESS PLOT*)
With[{exclusions=Join[If[OptionValue[Exclusions]===None,{},Flatten[{OptionValue[Exclusions]}]],
  PiecewisePlot`var==#&/@Flatten[First@annotations]](*can't we use annotatedpoints?*)},
  With[{curves=Plot[f,domain,Evaluate@Join[{Exclusions->exclusions},plotopts]]},
    Show[curves,Graphics[{ColorData[1][1],EdgeForm[ColorData[1][1]],OptionValue[PlotStyle]/.Automatic->{},
      MapThread[Map,{{Asymptote[#,PlotRange->PlotRange[curves],opts]&,EmptyDot[#,opts]&,FilledDot[#,opts]&},
        If[Depth[#]>2,First[#],#]&/@annotations}]}]]]]]]];
(*The user-interface*)
PiecewisePlot[f:HoldPattern@Piecewise[pieces_,default_],domain_,opts:OptionsPattern[]]:=PiecewisePlot`plot[f,domain,opts];
(*tries to expand f as a Piecewise function*)
PiecewisePlot`pweMethods={"Simplification"->False,"EliminateConditions"->False,"RefineConditions"->False,"ValueSimplifier"->None};
PiecewisePlot[f_,domain:{var_,a_,b_},opts:OptionsPattern[]]:=
    Block[{PiecewisePlot`graphics},(*restrict var in PiecewiseExpand/Reduce*)
      With[{a0=If[#<a,#,#-1/2]&@Floor[a],b0=If[#>b,#,#+1/2]&@Ceiling[b]},
        With[{pwf=Assuming[a0<var<b0,PiecewiseExpand[f/.dis:PiecewisePlot`$discontinuousAll[_]:>Piecewise[Map[{#[[1,-1]],Replace[#[[2;;]],
          cond_/;!FreeQ[cond,C[_]]:>(Reduce[#,var,DeleteDuplicates@Cases[#,C[_],Infinity]]&/@
              LogicalExpand[cond/.HoldPattern[And[e__?(FreeQ[#,var]&)]]:>Reduce[And[e]]])]}&,
          List@@Reduce[dis==C[1]&&C[1]\[Element]Integers&&a0<var<b0,{C[1],x},Backsubstitution->True]],Indeterminate],
          Method->PiecewisePlot`pweMethods]]},If[Head[pwf]===Piecewise,
          PiecewisePlot`graphics=PiecewisePlot`plot[pwf,domain,opts],
          PiecewisePlot`graphics=PiecewisePlot`plot[Piecewise[{{f,-Infinity<var<Infinity}},Indeterminate],domain,opts]]]];
      PiecewisePlot`graphics];
Periodization[func_,{val_Symbol,min_?NumberQ,max_?NumberQ}]:=func/.(val:>Mod[val-min,max-min]+min);



(* ::Subsubsection:: *)
(*作用于图片的绘图函数*)
Gray3DPlot[img_,points_:200]:=Block[{gray},
  gray=Reverse@ImageData@RemoveAlphaChannel@ColorConvert[img,"Grayscale"];
  ListPlot3D[gray,ColorFunction->GrayLevel,MaxPlotPoints->points,Boxed->False,Axes->False,Mesh->None]];
Wave3DPlot[img_,mf_:5,md_:80,ops___]:=Block[{in,gray},
  in=ImageResize[MeanFilter[img,mf],{200,md}];
  gray=Reverse@ImageData@RemoveAlphaChannel@ColorConvert[in,"Grayscale"];
  ListPlot3D[gray,Mesh->{0,md},PlotStyle->Opacity[0],Boxed->False,Axes->False,ops]];
Options[letter]={ImageSize->100};
letter[s_,OptionsPattern[]]:=Binarize@Graphics[
  {EdgeForm[None],FaceForm[Black],
    First[First[ImportString[ExportString[Style[s,FontSize->100],"PDF"],
      "PDF","TextMode"->"Outlines"]]]},AspectRatio->1,
  ImageSize->OptionValue[ImageSize]];



(* ::Subsubsection:: *)
(*其他绘图*)
GEBPlot[str_String,res_Integer:100]:=Block[{X,Y,Z},
  {X,Y,Z}=(ImageData@letter[#,ImageSize->res])&/@StringPartition[str,1];
  Quiet@RegionPlot3D[X[[Round[i],Round[j]]]==0&&Y[[Round[i],Round[k]]]==0&&
      Z[[Round[j],Round[k]]]==0,{i,1,res},{j,1,res},{k,1,res},
    Boxed->False,Axes->False,Mesh->None,PlotPoints->res/10]];
DigitsPlot[x_,num_:100,dig_:10,ops___]:=ArrayPlot[Partition[RealDigits[x,dig,num][[1]],dig],Mesh->True,ops];



(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];