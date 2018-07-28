"https://www.wolfram.com/language/11/image-and-signal-processing/visualize-and-synthesize-stereograms.html?product=mathematica";


StereogramShift[maxshift_][pattern_Image, depth_Image] := Block[
	{array, trafo, shifted, width, height},
	{width, height} = {1 / 2, 1} ImageDimensions[pattern];
	array = maxshift Transpose@ImageData[depth, DataReversed -> True];
	trafo = Function[position , (position + {Extract[array, Ceiling[position]], 0})];
	shifted = ImageTransformation[pattern, trafo,
		Resampling -> "Cubic", Padding -> "Periodic",
		DataRange -> All, PlotRange -> {{0, width}, {0, height}}
	];
	ImageAssemble[{{ImageTake[pattern, All, {-width, -1}], shifted}}]
];
CreateStereogram[pattern_Image, depth_Image, maxshift_ : 24] := Module[
	{W, H, w, h, depthMap, patternStrip, ips},
	{w, h} = ImageDimensions[pattern];
	{W, H} = ImageDimensions[depth];
	depthMap = ImageCrop[
		ImageClip@ColorConvert[depth, "Grayscale"], {Ceiling[W, w], Full}];
	patternStrip = ImageCrop[
		ImageAssemble@
			Transpose@{ConstantArray[pattern, {Ceiling[H / h]}]}, {w, H}];
	ips = Map[
		Function[ strip , ImageTake[strip, All, {w / 2 + 1, -1}]],
		FoldList[
			StereogramShift[maxshift],
			patternStrip,
			Flatten@ImagePartition[depthMap, {1 / 2, 1} ImageDimensions[patternStrip]]
		]
	];
	ImageCrop[ImageAssemble@List@ips, {W, H}]
];


StereogramDecode[img_Image, shift_Integer] := With[
	{ics = ImageDisplacements[{ImageTake[img, All, {shift + 1, -1}], ImageTake[img, All, {1, -shift - 1}]}]},
	Image[Function[depth, 1600depth / (160 + depth)][Map[Norm, RandomChoice@ics, {2}]]] // ImageAdjust
];