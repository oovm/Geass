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
(*Creation Date:2016-02-17*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["ExPainting`"];
TriPainting::usage = "
TriPainting[image]可以得到图像image的三角划分\r
TriPainting[image,n]指定生成时的划分数量为n,默认值1000";
PloyPainting::usage = "
PloyPainting[image]可以得到图像image的多边形划分\r
PloyPainting[image,n]指定生成时的划分数量为n,默认值1000\r
PloyPainting[image,{n,m,mesh}],指定畸变度为m,默认值1,网格宽度,默认为0.";
GlassPainting::usage = "
GlassPainting[image]可以得到图像image的玻璃化效果";
PointPainting::usage = "
PointPainting[image]可以得到图像image的点绘风格划分\r
PointPainting[image,n]指定生成时的点的数量为n,默认值10000";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
ExPainting$Version="V0.6";
ExPainting$Environment="V11.0+";
ExPainting$LastUpdate="2016-11-27";
ExPainting::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)



(* ::Subsubsection:: *)
(*多边形画风*)
TriPainting[i_,n_:1000]:=
    Block[{x,y,pt,pts},{x,y}=ImageDimensions[i];
    pt=Reverse/@RandomChoice[Flatten@ImageData@GradientFilter[i,2]->Tuples@{Range[y,1,-1],Range[x]},n];
    pts=Join[pt,{{0,0},{x,0},{x,y},{0,y}}];
    Graphics[With[{col=RGBColor@ImageValue[i,Mean@@#]},{EdgeForm@col,col,#}]&/@MeshPrimitives[DelaunayMesh@pts,2]]];
PloyPainting[img_,n_:1000]:=
    Block[{x,y,gr,pt},{x,y}=ImageDimensions[img];
    gr=ListDensityPlot[
      Transpose@{RandomReal[x,n],RandomReal[y,n],RandomReal[1,n]},
      InterpolationOrder->0,Frame->False,Mesh->All,
      AspectRatio->Automatic];
    pt=Polygon[#[[1]]]&/@Cases[Normal@gr,_Polygon,\[Infinity]];
    Graphics[With[{col=RGBColor@ImageValue[img,Mean@@#]},
      {EdgeForm@col,col,#}]&/@pt,AspectRatio->ImageAspectRatio[img]]];
(*PloyPainting[图片,{精细度,畸变度,网格}]*)
PloyPainting[img_,{n_,m_:1,mesh_:None}]:=
    Block[{im,pts,dat},im=ImageAdjust[ImageResize[img,n]];
    dat=Apply[RGBColor,Flatten[Transpose[Reverse[ImageData[im]]],1],{1}];
    pts=Flatten[Table[{x,y}+RandomReal[m{-1,1}],{x,1,n},{y,1,n}],1];
    ListDensityPlot[Flatten/@Transpose[{pts,Range[1,n^2]}],
      InterpolationOrder->0,Mesh->mesh,
      MeshStyle->Thickness[Small],Frame->False,
      ColorFunction->dat]];
Dither[pts_,dith_]:=pts+.25 dith RandomReal[{-1,1},{Length@pts,2}];
GlassPainting[img_,m_:1000,n_:5]:=
    Block[{bounds,num,seeds,vrnMesh,polygons},
      bounds=Transpose@{{0,0},ImageDimensions@img};
      num=ImageValuePositions[EdgeDetect[img],White];
      seeds=RandomSample[num,Min[m,Length@num]];
      vrnMesh=VoronoiMesh[Dither[seeds,.01],bounds];
      polygons=Table[{EdgeForm[Black],
        FaceForm[RGBColor[PixelValue[img,Mean@@pol]]],pol},{pol,
        MeshPrimitives[Nest[VoronoiMesh[Mean@@@MeshPrimitives[#,2],bounds]&,vrnMesh,n],2]}];
      Graphics@polygons];
PointPainting[img_,n_:10000]:=
    Block[{etf,sdf,map,mapdata,data,w,h,ch,spots},
      etf=EntropyFilter[img,12]//ImageAdjust;
      sdf=ColorConvert[StandardDeviationFilter[img,5],"GrayScale"]//ImageAdjust;
      map=ImageAdd[sdf,etf]//ImageAdjust;
      mapdata=ImageData[map];
      data=ImageData[img];
      {w,h}=ImageDimensions[img];
      ch=RandomChoice[(Flatten[mapdata]+0.1)^1.7->Join@@Table[{i,j},{i,h},{j,w}],n];
      spots=Reverse@SortBy[{data[[#1,#2]],{#2,-#1},15(1.1-mapdata[[#1,#2]])^1.8}&@@@ch,Last];
      Graphics[{RGBColor[#1],Disk[#2,#3]}&@@@spots,Background->GrayLevel[0.75],PlotRange->{{1,w},{1,-h}}]];
TranslateObject[p_,{x_,y_}]:=Map[{x,y}+#&,p,{2}];
HouseHexPolygon[s_,0]:=
    Polygon[s*{{1/2,-1},{3/2,0},{3/2,1},{-1/2,1},{-3/2,0},{-3/2,-1}}];
HouseHexPolygon[s_,1]:=
    Polygon[s*{{1,1/2},{0,3/2},{-1,
      3/2},{-1,-1/2},{0,-3/2},{1,-3/2}}];
HouseHexGrid[s_,imax_,jmax_]:=
    Block[{imod,jmod,k,m},Flatten[Table[imod=Mod[2i,5];
    jmod=Mod[2i+3,5];
    k=Range[0,Floor[(jmax-imod)/5]];
    m=Range[0,Floor[(jmax-jmod)/5]];
    {Table[TranslateObject[HouseHexPolygon[s,0],s*{i+1/2,j}],{j,jmod+1/2+5*m}],
      Table[TranslateObject[HouseHexPolygon[s,1],s*{i,j}],{j,imod+5*k}]},{i,0,imax}],2]];
HouseHexGridPerturbed[s_,h_,v_,r_]:=
    Block[{poly=Map[Round[#,10.^-10]&,HouseHexGrid[N[s],h,v],{2}],pts,len,rules},
      pts=DeleteDuplicates[Flatten[poly[[All,1]],1]];
      len=Length[pts];
      rules=Dispatch[Thread[pts->Range[len]]];
      GraphicsComplex[pts+RandomReal[{-r,r},{len,2}],
        RandomSample[poly]/.rules]];
(*HexPainting[图像,粒度,纹理]*)
HexPainting[image_,s_,t_:0]:=
    Block[{d,dim,g,centroids,colours,rr,gg,bb,greys},
      d=Reverse[ImageData[image]];dim=Dimensions[d];
      g=HouseHexGridPerturbed[s,Floor[dim[[2]]/s-3],Floor[dim[[1]]/s-3],0];
      centroids=Apply[Mean[g[[1,#]]]&,g[[2]],1];
      colours=Apply[RGBColor,Extract[d,Map[Reverse,Round[centroids+s/2]]],1];
      greys=colours/.RGBColor[rr_,gg_,bb_]->0.299rr+0.587gg+0.114bb;
      g[[2]]=Transpose[{colours,g[[2]]}][[Ordering[greys]]];
      If[t==1,Graphics[{EdgeForm[{Thickness[0.0003],Black}],
        GraphicsComplex[g[[1]],g[[2]]]}],
        Graphics[{GraphicsComplex[g[[1]],g[[2]]]}]]];



(* ::Subsubsection:: *)
(*未分类代码*)

ShapedFunction=Compile[{{v,_Real},{kernel,_Real,2}},v*kernel,
  RuntimeAttributes->{Listable},Parallelization->True,
  CompilationTarget->"C",RuntimeOptions->"Speed"];
ShapedPixels[img_,kernel_]:=
    With[{dim=ImageDimensions[img]},
      ImageCrop[Image[Join@@Transpose[Join@@@
          Transpose[ShapedFunction[ImageData[ImageResize[img,
            Ceiling[dim/Reverse[Dimensions[kernel]]]]],kernel],
            {1,2,5,4,3}],{1,3,2,4}]],dim]];
ShapedPainting[pic_,mat___]:=Manipulate[
  ShapedPixels[pic,ArrayPad[matrix[r],padding]],
  {{r,0,"遮罩半径"},0,20,1},{{padding,0,"遮罩间隙"},0,10,1},
  {{matrix,DiskMatrix,"遮罩矩阵"},{DiskMatrix,DiamondMatrix,
    BoxMatrix,IdentityMatrix,CrossMatrix,mat}}];
(*测试代码 ShapedPainting[pic, Rescale@GaussianMatrix[#] &]*)
QRPainting[aim_,text_:"https://github.com/GalAster/BiGridGenerator"]:=
    Block[{img,d,tgi,f,cg,dat,color},
      img=ImageData@ColorConvert[BarcodeImage[text,{"QR","H"},50],"LAB"];
      d=Length@img;
      tgi=ImageAdjust@ImageResize[aim,Most@Dimensions@img];
      color[Lmat_,A_,B_]:=Map[{.8#+.2,A,B}&,Lmat,{2}];
      f[0,{_?(#>=.9&),_,_}]:=color[1-DiskMatrix[0,3],0,0];
      f[1,{_?(#<.1&),_,_}]:=color[DiskMatrix[0,3],0,0];
      f[x_,{L_,A_,B_}]:=color[Partition[Insert[RandomSample[1-UnitStep[Range@8-Floor[L*10]+x]],x,5],3],A,B];
      f[x_List,LAB_List]:=f@@@Thread@{x,LAB};
      cg[m_]:=ArrayFlatten@Map[ConstantArray[#,{3,3}]&,m,{2}];
      dat=ArrayFlatten[f[img[[;;,;;,1]],ImageData[ColorConvert[tgi,"LAB"]][[;;,;;,;;3]]]];
      (dat[[Span[24#1],Span[24#2]]]=cg[img[[Span[8#1],Span[8#2]]]])&@@@{{1,1},{1,-1},{-1,1}};
      dat[[22;;3d-21,19;;21]]=Transpose[dat[[19;;21,22;;3d-21]]=cg[{{#,0,0}&/@Mod[Range[d-14],2]}]];
      Image[dat,ColorSpace->"LAB",ImageSize->Large]];


(*见鬼,写完才发现Mathematica没有YUV的色彩空间...
Solve[{y,u,v}=={0.299*r+0.587*g+0.114*b,-0.169r-0.331g+0.5b+128,0.5r-0.419g-0.081b+128},{b,g,r}];
YUV2RGB=Function[{y,u,v},Evaluate@Flatten[{r,g,b}/.%]];
CYR=77;CYG=150;CYB=29;
CUR=-43;CUG=-85;CUB=128;
CVR=128;CVG=-107;CVB=-21;
{{CYR,CYG,CYB},{CUR,CUG,CUB},{CVR,CVG,CVB}}
y=BitShiftRight[CYR*r+CYG*g+CYB*b,8];
u=BitShiftRight[CUR*r+CUG*g+CUB*b,8];
v=BitShiftRight[CVR*r+CVG*g+CVB*b,8];
RGB2YUV=Function[{r,g,b},BitShiftRight[{29b+150g+77r,128b-85g-43r,-21b-107g+128r},8]+{0,128,128}];
Image[Nest[Floor@Apply[YUV2RGB,Apply[RGB2YUV,#,{2}],{2}]&,st,20],"Byte"]*)
(*手撸JPEG的压缩算法,只能压缩灰度图,而且还真心TM慢
DCT[x_?MatrixQ]:=Transpose[N@FourierDCTMatrix[8]].x.FourierDCTMatrix[8]
IDCT[x_?MatrixQ]:=Transpose[FourierDCTMatrix[8,3]].x.FourierDCTMatrix[8,3]
jpeg[img_]:=ImageAssemble[Map[Image[Threshold[DCT[ImageData[#]],{"LargestValues",8}]]&,ImagePartition[ImageSubtract[img],8],{2}]]
ImageAssemble[Map[Image[IDCT[ImageData[#]]]&,ImagePartition[jpeg[img],8],{2}]];

RGB2RGB[r_, g_, b_, other___] := Block[{y, u, v},
  y = BitShiftRight[77*r + 150*g + 29*b, 8];
  u = BitShiftRight[-43*r - 85*g + 128*b, 8] - 1;
  v = BitShiftRight[128*r - 107*g - 21*b, 8] - 1;
  {BitShiftRight[65536*y + 91881*v, 16],
   BitShiftRight[65536*y - 22553*u - 46802*v, 16],
   BitShiftRight[65536*y + 116130*u, 16],
   other}]

RGB2RGBCompile = Compile[{{r, _Integer}, {g, _Integer}, {b, _Integer}},
  Evaluate@RGB2RGB[r, g, b],
  RuntimeAttributes -> {Listable}, Parallelization -> True,
  RuntimeOptions -> "Speed"
  ]

*)
(*伪造的劣化绿绿算法,再编译加速下好了
RGB2RGB=Function[{r,g,b},{RandomReal[{0.9,1.0}]r,RandomReal[{0.99,1.01}]g,RandomReal[{0.9,1.0}]b}];*)
RGB2RGB=Compile[{{r,_Integer},{g,_Integer},{b,_Integer}},{RandomReal[{0.9,1.0}]r,RandomReal[{0.99,1.01}]g,RandomReal[{0.9,1.0}]b}];
TiebaPainting[image_,qua_:0.5,time_:20]:=Block[{img,w},
  img=image;
  w=ImageDimensions[img][[1]];
  img=ImageResize[img,201];
  img=Nest[Image[Apply[RGB2RGB,ImageData[#,"Byte"],{2}],"Byte"]&,img,time];
  Do[Export["baidu.jpeg",img,"CompressionLevel"->(1-qua)];
  img=Import["baidu.jpeg"]
    ,{n,1,time}];
  img=ImageResize[img,w]];
(*本函数全是bug,一点都不好用*)
ExpandPainting[img_,neigh_:20,samp_:1000]:=Block[{dims,canvas,mask},
  dims=ImageDimensions[img];
  canvas=ImageCrop[starryNight,2*dims,Padding->White];
  mask=ImageCrop[ConstantImage[Black,dims],2*dims,Padding->White];
  Inpaint[canvas,mask,Method->{"TextureSynthesis","NeighborCount"->30,"MaxSamples"->1000}]];
(*LineWebPainting[图像,细腻度:100]*)
LineWebPainting[img_,k_:100]:=Block[{radon,lhalf,inverseDualRadon,lines},
  If[k === 0, Return[img]];
  radon=Radon[ColorNegate@ColorConvert[img,"Grayscale"]];
  {w,h}=ImageDimensions[radon];
  lhalf=Table[N@Sin[\[Pi] i/h],{i,0,h-1},{j,0,w-1}];
  inverseDualRadon=Image@Chop@InverseFourier[lhalf Fourier[ImageData[radon]]];
  lines=ImageApply[With[{p=Clip[k #,{0,1}]},RandomChoice[{1-p,p}->{0,1}]]&,inverseDualRadon];
  ColorNegate@ImageAdjust[InverseRadon[lines,ImageDimensions[img],Method->None],0,{0,k}]];
ImageStandUp[img_]:=Block[{lines=ImageLines@DeleteSmallComponents[EdgeDetect[img,Method->{"Canny","StraightEdges"->0.4}]],list},
  ImageRotate[img,Mean@Select[list=Mod[-#,Pi/2,-Pi/4]&/@ArcTan@@@Subtract@@@lines,
    Chop[First@Commonest@Round[list,1/1000]-#,1/1000]===0&],Background->Transparent]];
ImageStandUp[img_,"TryAll"]:=Block[{imgrote},imgrote=ImageRotate[img,#,"SameRatioCropping"]&/@
    Range[-Pi/2,Pi/2,Pi/6];TableForm[Function[{a,b},a->b]@@@Transpose@{imgrote,ImageStandUp/@imgrote}]];

(*Mondrian[大小,复杂度,比例]*)
Mondrian[p_,complex_:6,ratio_:0.7]:=Block[{splitx,splity,f,cols},
  splitx[Rectangle[{x0_,y0_},{x1_,y1_}]]:=
      Block[{a=RandomInteger[{x0+1,x1-1}]},{Rectangle[{x0,y0},{a,y1}],Rectangle[{a,y0},{x1,y1}]}];
  splity[Rectangle[{x0_,y0_},{x1_,y1_}]]:=
      Block[{a=RandomInteger[{y0+1,y1-1}]},{Rectangle[{x0,y0},{x1,a}],Rectangle[{x0,a},{x1,y1}]}];
  f=ReplaceAll[r:Rectangle[{x0_,y0_},{x1_,y1_}]:>RandomChoice[{(x1-x0)^2,(y1-y0)^2,5}->{splitx,splity,Identity}]@r];
  cols=MapThread[Darker,{{Black,White,Yellow,Red,Blue},{0,0.1,0.1,0.15,0.3}}];
  Graphics[{EdgeForm@Thickness[0.012],{FaceForm@RandomChoice@cols,#}&/@
      Flatten@Nest[f,Rectangle[{0,0},{p,p}],complex]},AspectRatio->ratio]];


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];