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
(*Creation Date:2016-03-15*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["ExRandom`"];
RandomExample::usage = "RandomExample[]随机给出一个Mathematica的巧妙范例!";
AutoBiography::usage = "Biography[Name]可以随机生成Name的自传哦!";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
ExRandom$Version="V0.1";
ExRandom$Environment="V11.0+";
ExRandom$LastUpdate="2016-11-22";
ExRandom::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)



(* ::Subsubsection:: *)
(*随机巧妙范例*)
RandomExample[]:=
    Block[{dir,file,inputs,output,cap,i=0,j=1,in},
      dir=DirectoryName[FindFile["ExamplePages/CreateMolecularGraphs.nb"]];
      file=RandomChoice[FileNames["*",dir]];
      output=Import[file,{"Cells","Output"}][[1]];
      cap=CellLabel/.Options[output];
      If[!StringQ[cap],Return[$Failed]];
      cap=ToExpression[StringReplace[cap,"Out["~~x__~~"]"~~__:>x]];
      inputs=Import[file,{"Cells","Input"}];
      CellPrint[TextCell[StringReplace[file,__~~"ExamplePages":>"ExamplePages"],"Subsubsection"]];
      CellPrint[Reap[While[i<cap&&j<=Length[inputs],
        in=CellLabel/.Options[inputs[[j]]];
        If[StringQ[in],i=ToExpression[StringReplace[in,"In["~~x__~~"]"~~__:>x]]];
        Sow[inputs[[j++]]]]][[-1,1]]];
      CellPrint[output];];



(* ::Subsubsection:: *)
(*随机头像 Gravatar*)
Gravatar::novpn="数据库请求失败,你可能需要VPN,或者你要求的数据量太过巨大,你可以使用TimeConstraint选项增加请求时长.";
Options[Gravatar]={Method->"Standard",TimeConstraint->5,ImageSize->256};
Gravatar[mail_String,OptionsPattern[]]:=Switch[OptionValue[Method],
  "Standard",TimeConstrained[GravatarLinker[mail,OptionValue[ImageSize]],OptionValue[TimeConstraint],Message[Gravatar::novpn]],
  "Pixels",IdenticonPixels[mail],
  "Cells",IdenticonCells[mail]];
IdenticonPixels[id_String]:=Block[{hash,color,orient,cells,tm,q},
  hash=IntegerDigits[Hash[id,"MD5"],8,36];
  color=RGBColor[hash[[1;;3]]/7];
  orient=If[OddQ[hash[[4]]],{Left,Bottom},{Bottom,Left}];
  cells=MapIndexed[If[OddQ[#1],color,White]&,Partition[hash,6],{2}];
  q=Image[cells];
  Magnify[ImageAssemble[{{q,ImageReflect[q,orient[[1]]]},
    {ImageReflect[q,orient[[2]]],ImageReflect[ImageReflect[q,Top],Left]}}],4]];
IdenticonCells[id_String]:=Block[{hash,color,orient,cells,tm,q},
  hash=IntegerDigits[Hash[id,"MD5"],8,36];
  color=RGBColor[hash[[1;;3]]/7];
  orient=If[OddQ[hash[[4]]],{ReflectionMatrix[{1,0}],ReflectionMatrix[{0,1}]},{RotationTransform[Pi/2],RotationTransform[3 Pi/2]}];
  cells=MapIndexed[If[OddQ[#1],{2,#2[[1]]},Nothing]&,hash];
  tm=TriangulateMesh[BoundaryMeshRegion[{{0,0},{1,0},{1,1},{0,1}},Line[{1,2,3,4,1}]],MaxCellMeasure->1/26,MeshQualityGoal->1];
  q=MeshPrimitives[tm,cells];
  Graphics[{color,EdgeForm[color],q,Translate[GeometricTransformation[q,orient[[1]]],{2,0}],
    Translate[GeometricTransformation[q,RotationTransform[Pi]],{2,0}],Translate[GeometricTransformation[q,orient[[2]]],{0,0}]}]];
GravatarLinker[email_,size_: 256]:=Block[{emailparts,randN,input,inputhash,img},
  inputhash=IntegerString[Hash[ToLowerCase[email],"MD5"],16,32];
  Import["http://www.gravatar.com/avatar/"<>inputhash<>"?s="<>ToString[size]<>"&d=identicon&r=PG"]];



(* ::Subsubsection:: *)
(*随机游走研究*)
Options[CoverTime] = {Out -> "Mean"};
CoverTime[input_, i_, OptionsPattern[]] := 
	Block[{mat, st, dmp},
  	Switch[Head@input,
   		Graph,
   			mat = AdjacencyMatrix@input;
   			st = UnitVector[Length@mat, i];
   			dmp = DiscreteMarkovProcess[st, mat],
  		 List,
   			If[MatrixQ[input], Message["Not Matrix"]];
 		  	st = UnitVector[Length@input, i];
  		 	dmp = DiscreteMarkovProcess[st, mat = input]
 	];
	CoverTimeAll[mat, i, dmp, OptionValue[Out]]
]

CoverTimeAll[mat_, i_, dmp_, opt_] :=
	Block[{k, road, sign, pdf, ex, ans},
   	road = Subsets[DeleteCases[Range@Length@mat, i]][[2 ;; -1]];
   	sign = If[Length[#]~Mod~2 == 1, 1, -1] & /@ road;
   	ans = Switch[opt,
		"PDF",
			pdf = PDF[FirstPassageTimeDistribution[dmp, #], k] & /@ road;
			Total[sign*pdf] // PiecewiseExpand,
		"Mean",
			ex = Mean@FirstPassageTimeDistribution[dmp, #] & /@ road;
			Total[sign*ex]
		];
	Return[ans];
];



ExRandomWalk[max_,"2DGridSelfAvoiding"]:=Block[{i=0,pts={{0,0}},moves={{-1,0},{0,1},{1,0},{0,-1}}},
  While[i<max&&Not@(And@@(MemberQ[pts,#]&/@Table[pts[[-1]]+moves[[i]],{i,1,4}])),i++;
  AppendTo[pts,RandomChoice[Select[Table[pts[[-1]]+moves[[i]],{i,1,4}],Not@MemberQ[pts,#]&]]]];
  TemporalData[Transpose@#,{Range@Length@#}]&@pts];
ExRandomWalk[max_,"3DGridSelfAvoiding"]:=Block[{notvisitedQ,SARW,pts},
  notvisitedQ[_]:=True;
  SARW={#1,Select[Flatten[Outer[Plus,{#1},{{1,0,0},{-1,0,0},{0,1,0},{0,-1,0},{0,0,1},{0,0,-1}},1],1],notvisitedQ]}&;
  pts=NestWhileList[SARW[notvisitedQ[#1[[1]]]=False;
  RandomChoice[#1[[2]]]]&,{{0,0,0},{{1,0,0},{-1,0,0},{0,1,0},{0,-1,0},{0,0,1},{0,0,-1}}},#1[[2]]=!={}&,1,max-1];
  TemporalData[Transpose@#,{Range@Length@#}]&@(First/@pts)];

SetAttributes[RandomWalkPlot,HoldAll];
RandomWalkPlot[ExRandomWalk[max_,"2DGridSelfAvoiding"]]:=
    Block[{pts=Transpose[ExRandomWalk[max,"2DGridSelfAvoiding"]["ValueList"]]},
      Graphics[Line@pts,Epilog->{PointSize[Large],RGBColor[.6,.74,.36],Point[{0,0}],RGBColor[.9,.42,.17],
        Point[Last@pts],PointSize[Medium],Black,Table[Point[pts[[i]]],{i,2,Length[pts]-1}]},PlotRange->All]];
RandomWalkPlot[ExRandomWalk[max_,"3DGridSelfAvoiding"]]:=
    Block[{pts=Transpose[ExRandomWalk[max,"3DGridSelfAvoiding"]["ValueList"]]},
      Graphics3D[{Thick,Gray,Line[pts],RGBColor[0.6,0.74,0.36],Sphere[pts[[1]],1],RGBColor[0.9,0.42,0.17],Sphere[pts[[-1]],1]},
        PlotLabel->Style[If[Length[pts]<max,StringJoin[ToString[Length[pts]-1],"步后卡住了!!!"],StringJoin[ToString[max-1],"步后逃逸!"]],"Label",12],
        PlotRange->All,Boxed->False]];










(* ::Subsubsection:: *)
(*随机图案*)
InvMollweide[{x_,y_}]:=With[{theta=ArcSin[y]},{Pi(x)/(2Cos[theta]),ArcSin[(2theta+Sin[2theta])/Pi]}];
(*随机宇宙背景辐射*)
RandomCBR[res_:64]:=Block[{Alms,fieldN,dat,im},
  Do[Alms[l,m]=(Random[NormalDistribution[]]+I Random[NormalDistribution[]])/Sqrt[(l+2)(l+1)];
  Alms[l,-m]=(-1)^m Conjugate@Alms[l,m];,{l,0,48},{m,0,l}];
  Do[Alms[l,0]=(Random[NormalDistribution[]])/Sqrt[(l+2)(l+1)];,{l,0,48}];
  fieldN=Compile[{\[Theta],\[Phi]},Evaluate@Sum[Alms[l,m]SphericalHarmonicY[l,m,\[Theta],\[Phi]],{l,0,48},{m,-l,l}]];
  dat=ParallelTable[fieldN[\[Theta],\[Phi]],{\[Theta],0.0,Pi,Pi/res},{\[Phi],0.0,2Pi,Pi/res}];
  im=Re[dat]//Image//ImageAdjust//Colorize[#,ColorFunction->"LightTemperatureMap"]&;
  ImageTransformation[im,InvMollweide,DataRange->{{-Pi,Pi},{-Pi/2,Pi/2}},PlotRange->{{-2,2},{-1,1}},Padding->White]];
(*随机生成类似鹅卵石的图案*)
RandomPebble[n_,sc_:0.95]:=With[{data=MapIndexed[Flatten[{##1}]&,RandomReal[1,{n,2}]]},
  Normal[ListDensityPlot[data,InterpolationOrder->0,
    ColorFunction->Hue,Mesh->All,
    Background->Lighter[Hue[RandomReal[]],.75],Frame->False,
    ImageSize->400]]/.Polygon[l_,v_]:>Scale[{Hue[RandomReal[]],
    FilledCurve[BSplineCurve[l,SplineClosed->True,SplineDegree->3]]},sc]];







(* ::Subsubsection:: *)
(*随机列表划分*)
RandomPartition[n_,p_?IntegerQ]:=Block[{r},r=RandomSample[Range[1,n],p-1]//Sort;
AppendTo[r,n];Prepend[r//Differences,r[[1]]]];
RandomPartition[n_,V_?VectorQ]:=Block[{r,s},r=RandomInteger[V,n];
s=Select[Accumulate@r,#<n&]~Join~{n};Append[Differences@s,s[[1]]]];




(* ::Subsubsection:: *)
(*待处理*)
(*https://zhuanlan.zhihu.com/p/21258963*)
百家姓=StringPartition["王李张刘陈杨黄赵周吴徐孙马胡朱郭何罗高林郑梁谢唐许冯宋韩邓彭曹曾田于萧潘袁董叶杜丁蒋程余吕魏蔡苏任卢沈姜姚钟崔陆谭汪石傅贾范金方韦\
夏廖侯白孟邹秦尹江熊薛邱阎段雷季史陶毛贺龙万顾关郝孔向龚邵钱武扬黎汤戴严文常牛莫洪米康温代赖施覃安",1];
复姓={"欧阳","上官","司马","东方","诸葛","令狐","南宫","慕容","公孙","司徒","皇甫","夏候","万俟","宇文","轩辕","东郭","南门","西门","尉迟"};
通用字=StringPartition["天地日月星辰金木水火土风云雨声雷电山丘陵平原台江河川池湖海洋冰沙泽深涵春夏秋冬立分至谷梧桐银赤红丹朱黄绿碧青蓝紫黑白墨苍初旭光\
荣宝玉藏陶艺家宙代纪世期时宇界系统阶带和清温润森林亭楼阁欢迎宜宾客友恩华飞升圣少子心真人学敬敏精湛景影喜一二三四五六七八九十百千万兆里元鑫甘\
乐方圆周维望诺冠亚季殿尤其迪化靖瑞睿易奕熙然司令群奇遇繁衍",1];
男用字=StringPartition["高大岩石勇猛刚强雄威豪震壮阔宏伟杰建国庆筑基健康毅启明亮道德忠仁义礼智信诚谦贤良俊通达广长泰昌盛富贵吉祥洪烈照鸿浩波涛浪源潮渊\
隐逸超越特勤东南西北中甲乙丙丁戊己庚辛壬癸阳顶立辉耀军兵将帅武力本主钢铁锋峰钟鼎铭印玄太远上中近古继承松柏根柱梁成龙虎豹彪骏鹏鹤斌郎公士夫生\
汉伯叔之胜利赢权益人民团结正定安兴举直凯旋征旅守卫福禄寿奋斗跃进步同志向全恒衡鸣",1];
女用字=StringPartition["晓小妹姑娘妃姬后好妙妮娃爱媛婵娟娇媚婉婷妍嫣贝璧环琪璇玑珍珠玲珑佩琼瑶瑜琳珊晶莹花草兰英灵芝萱芸茗芬芳蓓蕾翠丽萍莲荷莉蔷薇茜莎\
蓉菱梅杏杨柳桂椿榆枫凤雉燕鸽莺鹃蝶锦绣绫罗绮衣裳慧惠香穗颖芒彩艳素秀美悦雯露霞雪虹涟溪思绪念想盼希梦境淑贞端庄宁静倩巧楚琴笛竹棋书诗文语音韵\
歌曲如若晴曦馨欣颜优雅怡暖满可佳嘉",1];



种族表 = {人类, 暗夜精灵, 矮人, 侏儒, 德莱尼, 狼人, 牛头人, 巨魔, 亡灵, 血精灵, 地精, 熊猫人, 恐惧魔王, 深渊领主, 艾瑞达, 泰坦, 龙族, 野猪人, 鹰身人, 娜迦, 鱼人, 其拉虫族, 虚灵, 戈隆, 食人魔, 阿努比萨斯, 维库, 托维尔, 锦鱼人, 魔古族, 蜥蜴人, 纳鲁};
等级表 = Range@100;
武器表 = {提洛许 - 世纪梦魇, 戈拉德 - 巨龙之暮, 巨龙之怒, 影之哀伤, 瓦兰奈尔 - 远古王者之锤, 灵弦长弓, 宇宙灌注者,毁灭, 瓦解法杖, 无尽之刃, 迁跃切割者, 索利达尔 - 群星之怒, 埃辛诺斯战刃, 安杜尼蘇斯 - 靈魂的收割者,埃提耶什 - 守护者的传说之杖, 雷霆之怒 - 逐风者的祝福之剑, 萨弗拉斯 - 炎魔拉格纳罗斯之手, 霜之哀伤, 灰烬使者};
必杀技表 = {回到过去, 魂之挽歌, 恩赐解脱, 梦境缠绕, 星体禁锢, 灵魂超度, 黄泉颤抖, 永恒冰壁, 浴火重生, 时光倒流,海妖之歌, 灵魂之矛, 幻化之锋, 月之祝福, 月之暗面, 月神之箭, 群星坠落, 神之力量, 圆月之舞};
战斗力表 = Range@(10^6);
死亡时间表 = Range[500, 1500];
死亡事件表 = {太过鬼畜, 偷看基友洗澡, 太过脑残, 金坷垃洗脑, 看B站视频, 码代码};

AutoBiography[姓名_] := Block[{choose},
  SeedRandom[Hash[姓名, "CRC32"]];
  choose[表_] := 表[[RandomInteger[{1, Length[表]}]]];
  {种族, 等级, 武器, 必杀技, 战斗力, 死亡时间, 死亡事件} = choose /@ {种族表, 等级表, 武器表, 必杀技表, 战斗力表, 死亡时间表, 死亡事件表};
  Print[ToString@姓名 ~~ "是一个" ~~ ToString@等级 ~~ "级的" ~~ ToString@种族 ~~ ",成名武器是" ~~ ToString@武器 ~~ ",必杀技是" ~~ ToString@必杀技 ~~ "," ~~ ToString@死亡时间 ~~ "年死于" ~~ ToString@死亡事件 ~~ "."]]






(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];