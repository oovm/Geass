RiemannSurfacePlot3D::usage = 
"RiemannSurfacePlot3D[w == f[z], reim[w[z]], {z, w}] plots a Riemann surface of w as  the real or imaginary part reim of w over the complex z-plane.\r
 RiemannSurfacePlot3D[w == f[z], {\[Zeta]1, \[Zeta]2, \[Zeta]3}, {z, w}] plots a Riemann surface of w as {\[Zeta]1, \[Zeta]2, \[Zeta]3} \r
 along the Cartesian coordinate axes where \[Zeta]1, \[Zeta]2, \[Zeta]3 can be Re[z], Im[z], Re[w], Im[w] or a linear combination of them.";
Coloring::usage = 
"Coloring 是 RiemannSurfacePlot3D 的一个选项. The coloring should be specified as an expression containing
Re[z], Im[z], Re[w], Im[w] that for numerical values evaluates to a coloring directive.";
BranchPointOffset::usage = 
"BranchPointOffset 是 RiemannSurfacePlot3D 的一个选项 and determines how far away from a branch point the numerical solution of the differential equation should start.";
StitchPatches::usage = 
"StitchPatches 是 RiemannSurfacePlot3D 的一个选项. and determines if the individual patches should be joined.";
LogSheets::usage = 
"LogSheets 是 RiemannSurfacePlot3D 的一个选项. and determines which sheets to use for logarithms
and product logarithms. The default is {-1, 0, 1}, meaning the main sheet and the sheets immediately below and above the main sheet.";
NDSolveOptions::usage = 
"NDSolveOptions 是 RiemannSurfacePlot3D 的一个选项. and allows propagating options to NDSolve
for the solution of the coupled system of nonlinear differential equations.";
Begin["`Private`"];
ArcTrigFunctionQ[f_] := MatchQ[f, ArcSin | ArcCos | ArcTan | ArcCot | ArcSec | ArcCsc |
			                      ArcSinh | ArcCosh | ArcTanh | ArcCoth | ArcSech | ArcCsch]
canonicalizeFunction[w_] :=  
 w //. f_?ArcTrigFunctionQ[x_] :> (Evaluate[TrigToExp[f[#]]]&[x]) //.
       ProductLog[_Integer, arg_] :> ProductLog[arg]
multiValuedTerms[w_, z_] := 
Union[Cases[canonicalizeFunction[w], 
		    Log[_?(MemberQ[#, z, {0, \[Infinity]}]&)] | 
            ProductLog[_?(MemberQ[#, z, {0, \[Infinity]}]&)] | 
            ProductLog[_Integer, _?(MemberQ[#, z, {0, \[Infinity]}]&)] |
		    Power[_?(MemberQ[#, z, {0, \[Infinity]}]&), 
                  _?(Not[(* some simplification effort *)IntegerQ[Together //@ #]]&)] |           
			Root[_?(MemberQ[#, z, {0, \[Infinity]}]&), __], {0, \[Infinity]}]]
branchPoints[w_, z_] := 
Block[{mvt = multiValuedTerms[w, z], bcsPre, bps},
       bcsPre = DeleteCases[Flatten[branchPointsF[#, z]& /@ mvt], z | _DirectedInfinity, \[Infinity]]; 
       bps = If[And @@ (NumericQ /@ bcsPre), bcsPre, $Failed];
       If[debugQ, Print[Style[Row[{Text["Branch points: "], bps}], Darker[Green]]]];
       bps] // Quiet
branchPointsF[Log[f_], z_] := z /. {ToRules[Reduce[f == 0 || 1/f == 0, z]]}
branchPointsF[ProductLog[f_], z_] := z /. {ToRules[Reduce[f == 0 || 1/f == 0 || f == -1/E, z]]}
branchPointsF[Power[f_, _], z_] := z /. {ToRules[Reduce[f == 0 || 1/f == 0, z]]}
branchPointsF[Root[f_, _], z_] := 
Block[{w, p}, p = f[w]; (z /. {ToRules[Reduce[Resultant[p, D[p, w], w] == 0, z]]})]
sectorArray[branchPoints_, branchPointOffset_:10^-6, prec_:25] := 
	Block[{\[CurlyEpsilon] = branchPointOffset, rMax, rList, \[CurlyPhi]List}, 
        rMax = If[# == 0., 1, 15/10 #]&[Max[Abs[rList = Abs /@ N[branchPoints, prec]]]];		
		(* the different distances *)
		rList = Union[Prepend[Append[rList, rMax], \[CurlyEpsilon]], SameTest -> ((Abs[#1 - #2]< 2 \[CurlyEpsilon])&)];
		(* the different angles *)
	    \[CurlyPhi]List = Sort[Union[If[# == 0., (* bignum zero *) SetAccuracy[0, prec], 
                         Arg[#]]& /@ branchPoints,  SameTest -> (Abs[#1 - #2] < \[CurlyEpsilon] &)], Less];
		\[CurlyPhi]List = Append[\[CurlyPhi]List, First[\[CurlyPhi]List] + 2 Pi];
		(* the different sectors *)      
        N[#, prec]& @ Table[{rList[[i]] + \[CurlyEpsilon], \[CurlyPhi]List[[j]] + \[CurlyEpsilon], rList[[i + 1]] - \[CurlyEpsilon], \[CurlyPhi]List[[j + 1]] - \[CurlyEpsilon]}, 
				            {i, Length[rList] - 1}, {j, Length[\[CurlyPhi]List] - 1}]]
numberMultiValuedTerms[w_, z_] := 
Block[{root, function, \[Omega], res}, 
res = Block[{i = 1}, 
MapAll[Which[MatchQ[#, Log[_?(MemberQ[#, z, {0, \[Infinity]}]&)]], 
				       (* the Log terms *)  Subscript[Log, i++] @@ #, 
             MatchQ[#, ProductLog[_?(MemberQ[#, z, {0, \[Infinity]}]&)]], 
				       (* the Log terms *)  Subscript[ProductLog, i++] @@ #, 
             MatchQ[#, Power[_?(MemberQ[#, z, {0, \[Infinity]}]&), _Rational]], 
				       (* the rational power terms *) Subscript[Power, i++] @@ #, 
			 MatchQ[#, Power[\[AlignmentMarker]_?(MemberQ[#, z, {0, \[Infinity]}]&), 
					\[AlignmentMarker]_?((MemberQ[#, z, {0, \[Infinity]}] || Not[IntegerQ[#]])&)]], 
				       (* the f[z]^g[z] terms *) Exp[Subscript[Log, i++][#[[1]]] #[[2]]], 
			 MatchQ[#, root[_?(MemberQ[#, z, {0, \[Infinity]}]&), _]], 
				       (* the Root terms *) Subscript[root, i++] @@ #, 
             True, #]&, 
				(* some preprocessing for Root objects *)
				w //. Root[f_, __] :> root[f, Exponent[f[\[Omega]], \[Omega]]] //.   
                (* neutralizing the Function in Root *) Function -> function] //.
			(* some postprocessing for Root objects *) {function -> Function, root -> Root}
	  ];
  If[debugQ, Print[Style[Row[{Text["Numbered multivalued sheets: "], res}], Darker[Green]]]];
  res]
powerAndLogTerms[w_, z_] := 
	Block[{powerTerms, logTerms, productLogTerms, sortedPowerAndLogTerms, rootTerms}, 
           \[AlignmentMarker]powerTerms = Cases[w, Subscript[Power, _][__], {0, \[Infinity]}];
           productLogTerms = Cases[w, Subscript[ProductLog, _][_], {0, \[Infinity]}];
           \[AlignmentMarker]logTerms = Cases[w, Subscript[Log, _][_], {0, \[Infinity]}];
           rootTerms = Cases[w, Subscript[Root, _][__], {0, \[Infinity]}];
	       Sort[Join[powerTerms, logTerms, productLogTerms, rootTerms], #1[[0, 2]] < #2[[0, 2]]&]]
substitutions[powerAndLogTerms_, wFunc_, w_, z_] := 
	Block[{ws = (# -> Subscript[w, #[[0, 2]]][z])& /@ powerAndLogTerms}, 
	   	{Last /@ FoldList[Append[#1, #2 //. #1]&, 
           {First[ws]}, Rest[ws]], ws, wFunc//.ws}]
toODE[\[Omega]:(Subscript[Log, _][\[Xi]_] -> w_), z_] := 
     {\[Omega], D[w, z] == Together[D[\[Xi], z]/\[Xi]]}
toODE[\[Omega]:(Subscript[ProductLog, _][\[Xi]_] -> w_), z_] := 
     {\[Omega], D[w, z] == w/(1 + w) Together[D[\[Xi], z]/\[Xi]]}
toODE[\[Omega]:(Subscript[Power, _][f_, Rational[p_, q_]] -> w_), z_] := 
     {\[Omega], D[w, z]== Together[p/q w D[f, z]/f]}
toODE[\[Omega]:(Subscript[Root, _][\[Xi]_, d_] -> w_), z_] := 
     {\[Omega], D[w, z] == Together[-D[\[Xi], z][w]/D[\[Xi][w], w]]}
derivativeFreeRhs[odes_] := 
Apply[Equal, Last /@ Transpose[{First /@ odes, 
      Last /@ FoldList[Append[#1, #2 //. #1]&, 
              {First[#]}, Rest[#]]&[Apply[Rule, Last /@ odes, {1}]]}], {1}]
(* this function is not used directly later; 
   but comes in handy sometimes *)
makeOdes[f_, {w_, z_}] :=  
        Function[\[Omega], MapAt[derivativeFreeRhs[toODE[#, z]& /@ #]&, 
                     substitutions[powerAndLogTerms[#, z], #, w, z]&[
                                   numberMultiValuedTerms[\[Omega], z]], 1]][f]
sheets[\[Omega]:Subscript[Power, i_][\[Xi]_, r:Rational[p_, q_]], ___] := 
       Table[\[Omega] -> E^((2\[Pi] I j)/q) \[Xi]^r, {j, 0, q - 1}];
sheets[\[Omega]:Subscript[Log, i_][\[Xi]_], ks_List] := 
       (\[Omega] -> Log[\[Xi]] + 2 # I \[Pi])& /@ ks
sheets[\[Omega]:Subscript[ProductLog, i_][\[Xi]_], ks_List] := 
       (\[Omega] -> ProductLog[#, \[Xi]])& /@ ks
sheets[\[Omega]:Subscript[Root, _][\[Xi]_, d_], ___] := 
       Table[\[Omega] -> Root[\[Xi], j], {j, 1, d}]
allSheets[sheetRealizations_] := 
Block[{res},
       res = Flatten[Outer[List, ##]& @@ sheetRealizations, Length[sheetRealizations] - 1];
       If[debugQ, Print[Style[Row[{Text["All sheets: "], res}], Darker[Green]]]];
       res]
odeAzimuthal[
  Derivative[1][Subscript[w_,i_]][z_]==(rhs_),{Subscript[w_,i_],z_},
  {\[Psi]_,\[CurlyPhi]_},r_]:=Derivative[1][Subscript[\[Psi],i]][\[CurlyPhi]]==
        rhs*D[r*E^(I*\[CurlyPhi]),\[CurlyPhi]]/.Subscript[w,j_][z]:>Subscript[\[Psi],j][\[CurlyPhi]]/.z->r*Exp[I*\[CurlyPhi]];
odeRadial[Derivative[1][Subscript[w_,i_]][z_]==(rhs_),{Subscript[w_,i_],z_},
  {\[Psi]_,\[Rho]_},\[ScriptD]_]:=Derivative[1][Subscript[\[Psi],i]][\[Rho]]==
        rhs*D[\[Rho]*\[ScriptD],\[Rho]]/.Subscript[w,j_][z]:>Subscript[\[Psi],j][\[Rho]]/.z->\[ScriptD]*\[Rho];
sectorSheetPatchPoints[args: PatternSequence[{w_, z_}, {w\[Psi]_, w\[Psi]Prime_}, wSheet_, {odes\[CurlyPhi]_, {\[Psi]_, \[CurlyPhi]_}, r_},    
                                             {odesr_, {\[Psi]_, \[Rho]_}, \[ScriptD]_}, sector:{r1_, \[CurlyPhi]1_, r2_, \[CurlyPhi]2_}, {pp\[CurlyPhi]O_, pprO_}, 
                                             prec_, NDSolveOptions___]] := 
	Block[{radialStartingValues, radialInits, radialIFs, azimuthalIFs, points, ra, \[CurlyPhi]a, rb, \[CurlyPhi]b, pp\[CurlyPhi], ppr, \[Psi]s,
            \[CapitalPsi], \[CapitalPsi]P, values, derivatives, defaultNDSolveOptions}, 
        {pp\[CurlyPhi], ppr} = {pp\[CurlyPhi]O, pprO} - 1;
		(* get initial values for the differential equation *)
		radialStartingValues = N[((Last /@ wSheet) //. wSheet) /. z -> r1 Exp[I \[CurlyPhi]1], prec];
		(* the functions of the odes *) 
		\[Psi]s = #[[1, 0, 1]]& /@ odesr;
		(* calculate initial conditions for radial odes *)
		radialInits = Apply[Equal, Transpose[{#[r1]& /@ \[Psi]s, radialStartingValues}], {1}];
        (* default options for numerical ODE solving *)
        defaultNDSolveOptions = Sequence[PrecisionGoal -> 12, AccuracyGoal -> 12, MaxSteps -> 20000,
                                         InterpolationOrder -> All];
		(* solve the differential equation radially *)
		radialIFs = NDSolve[Join[odesr, radialInits] /. \[ScriptD] -> Exp[I \[CurlyPhi]1], \[Psi]s , {\[Rho], r1, r2}, 
                            NDSolveOptions, defaultNDSolveOptions];
        (* solve the differential equation azimuthally *)
    	azimuthalIFs = Table[ra = r1 + i/ppr(r2 - r1);
          NDSolve[Join[odes\[CurlyPhi] /. r -> ra, 
                       Apply[Equal, Transpose[{#[\[CurlyPhi]1]& /@ \[Psi]s, #[ra]& /@ \[Psi]s /. radialIFs[[1]]}], {1}]],
                          \[Psi]s, {\[CurlyPhi], \[CurlyPhi]1, \[CurlyPhi]2}, NDSolveOptions, defaultNDSolveOptions], 
				       {i, 0, ppr}];  azimuthalIFs;
	   (* generate the function itself *)     
       \[CapitalPsi] = (w\[Psi] /. Subscript[w, i_][\[Xi]_] :> Subscript[\[Psi], i][\[CurlyPhi]b] /. azimuthalIFs) /. z -> rb E^(I \[CurlyPhi]b); 
	   (* calculate points for the patch *)	
	   values = Developer`ToPackedArray @ 
                Table[rb = r1 + i/ppr(r2 - r1); \[CurlyPhi]b = \[CurlyPhi]1 + k/pp\[CurlyPhi](\[CurlyPhi]2 - \[CurlyPhi]1); {rb, \[CurlyPhi]b, \[CapitalPsi][[i + 1, 1]]}, 
                      {i, 0, ppr}, {k, 0, pp\[CurlyPhi]}];
       (* generate the derivative *)    
       Clear[rb, \[CurlyPhi]b];
       \[CapitalPsi]P = (w\[Psi]Prime /. Subscript[w, i_][\[Xi]_] :> Subscript[\[Psi], i][\[CurlyPhi]b] /. azimuthalIFs) /. z -> rb E^(I \[CurlyPhi]b); 
   	(* calculate the derivative of the function *)     	
	   derivatives = Developer`ToPackedArray @ 
                     Table[rb = r1 + i/ppr(r2 - r1); \[CurlyPhi]b = \[CurlyPhi]1 + k/pp\[CurlyPhi](\[CurlyPhi]2 - \[CurlyPhi]1); {rb, \[CurlyPhi]b, \[CapitalPsi]P[[i + 1, 1]]}, 
                           {i, 0, ppr}, {k, 0, pp\[CurlyPhi]}];
	   (* return values and derivatives *)
	   {sector, {values, derivatives}}]
interpolateValuesAndDerivatives[{sector:{r1_, \[CurlyPhi]1_, r2_, \[CurlyPhi]2_}, {values_, derivatives_}}, 
                                {pp\[CurlyPhi]IO_, pprIO_}, ifOptions___] :=
Block[{rb, \[CurlyPhi]b, pp\[CurlyPhi]I, pprI, \[CapitalPsi]IF, \[CapitalPsi]PIF, valuesIF, derivativesIF},
     If[(InterpolationOrder /. {ifOptions}) === Automatic,
        Apply[{#1 Cos[#2], #1 Sin[#2], #3}&, {values, derivatives}, {-2}],
       {pp\[CurlyPhi]I, pprI} = {pp\[CurlyPhi]IO, pprIO} - 1;
       (* form interpolating functions for function values and derivatives *)
       \[CapitalPsi]IF = Interpolation[Flatten[values, 1], ifOptions];
       \[CapitalPsi]PIF = Interpolation[Flatten[derivatives, 1], ifOptions];
       (* form array of interpolated values *)
       valuesIF = Developer`ToPackedArray @ 
             Table[rb = r1 + i/pprI(r2 - r1); \[CurlyPhi]b = \[CurlyPhi]1 + k/pp\[CurlyPhi]I(\[CurlyPhi]2 - \[CurlyPhi]1); 
                   {rb Cos[\[CurlyPhi]b], rb Sin[\[CurlyPhi]b], \[CapitalPsi]IF[rb, \[CurlyPhi]b]}, 
                   {i, 0, pprI}, {k, 0, pp\[CurlyPhi]I}];
       derivativesIF = Developer`ToPackedArray @ 
             Table[rb = r1 + i/pprI(r2 - r1); \[CurlyPhi]b = \[CurlyPhi]1 + k/pp\[CurlyPhi]I(\[CurlyPhi]2 - \[CurlyPhi]1); 
                   {rb Cos[\[CurlyPhi]b], rb Sin[\[CurlyPhi]b], \[CapitalPsi]PIF[rb, \[CurlyPhi]b]}, 
                   {i, 0, pprI}, {k, 0, pp\[CurlyPhi]I}];
      {valuesIF, derivativesIF}
       ]
      ]
makePointsAndNormals = 
Compile[{{M, _Real, 2}, {valuesAndDerivatives, _Complex, 4}},
       Block[{values = valuesAndDerivatives[[1]], derivatives = valuesAndDerivatives[[2]],
               a = {0., 0., 0.}, b = {0., 0., 0.}},
       {Map[Re[M.{#[[1]], #[[2]], Re[#[[3]]], Im[#[[3]]]}]&, values, {2}], 
        Map[(a = Re[M.{1., 0.,  Re[#[[3]]], Im[#[[3]]]}];
             b = Re[M.{0., 1., -Im[#[[3]]], Re[#[[3]]]}];
          (* form cross product *)
            -Re[{a[[2]] b[[3]] - a[[3]] b[[2]],
                 a[[3]] b[[1]] - a[[1]] b[[3]],
                 a[[1]] b[[2]] - a[[2]] b[[1]]}])&, derivatives, {2}]}]
        ];
makeVertexColors[values_, cf_] := 
    cf @@@ Flatten[Map[{#[[1]], #[[2]], Re[#[[3]]], Im[#[[3]]]}&, values, {2}], 1]
makeGraphicsComplex[{points_?ArrayQ, normals_?ArrayQ}]  :=  
Block[{lOuter = Length[points], lInner = Length[points[[1]]]}, 
       GraphicsComplex[Flatten[N[points], 1], {EdgeForm[], 
       GraphicsGroup[Polygon[Flatten[#, 1]& @ 
           Table[{i lInner + j, i lInner + j + 1, (i + 1) lInner + j + 1, (i + 1) lInner + j}, 
                 {i, 0, lOuter - 2}, {j, lInner - 1}]]]}, 
                    VertexNormals -> (Normalize /@ Flatten[N[normals], 1])]]
sheetGraphicsComplex[M_, {values_, derivatives_}, cf_:Automatic]  := 
With[{gc =  makeGraphicsComplex[makePointsAndNormals[M, {values, derivatives}]]},
    If[cf === Automatic, gc, Append[gc, VertexColors -> makeVertexColors[values, cf]]]
    ]
MRe = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}};
MIm = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 0, 1}};
complexUnion[l_, SameTest -> func_] := 
Fold[Function[{u, v}, If[Select[u, func[#, v]&, 1] === {}, 
           Append[u, v], u]], {First[#]}, Rest[#]]&[Union[l]]
differentSheetRealizations[allSheets_, wnum_, z_, z1_, prec_] := 
complexUnion[{#, N[(wnum //. #) /. z -> z1, prec]}& /@ allSheets, 
		       SameTest -> Function[{x, y}, If[x[[2]] == 0, x[[2]] == y[[2]], 
                                               Abs[(x[[2]] - y[[2]])/x[[2]]] < 10^-(prec - 10)]]];
identicalSheetsQ[{sector1_, pointArray1_}, {sector2_, pointArray2_}]  :=  
Block[{ap1 = Map[Last, pointArray1, {2}], ap2 = Map[Last, pointArray2, {2}], 
        \[Delta]\[CapitalSigma], scale, \[CurlyEpsilon] = 10^-4}, 
        (* check sum of absolute values of differences at all points *)
        \[Delta]\[CapitalSigma] = Total[Flatten[Abs[ap1 - ap2]]];
        scale =  Total[Flatten[Abs[ap1]]] + Total[Flatten[Abs[ap2]]];
        If[scale == 0, True, \[Delta]\[CapitalSigma]/scale < \[CurlyEpsilon]]
       ]
dropMultipleSheetPointArrays[sectorSheetPointArrays_]  :=  
Union[sectorSheetPointArrays, SameTest -> (identicalSheetsQ[#1[[1]], #2[[1]]]&)]
sectorSheetPatches[{w_, z_}, w\[Psi]AndPrime_, wnum_, sheetRealizations_, 
                   {odes\[CurlyPhi]_, {\[Psi]_, \[CurlyPhi]_}, r_}, {odesr_, {\[Psi]_, \[Rho]_}, dir_}, 
                   sector:{r1_, \[CurlyPhi]1_, r2_, \[CurlyPhi]2_}, {pp\[CurlyPhi]_, ppr_}, {pp\[CurlyPhi]I_, pprI_}, 
                   prec_, io_, NDSolveOptions___] := 
Block[{sheetValues, sheetPointArrays, uniqueSheetDataArrays}, 
		(* the various log and radical sheet realizations *)
       sheetValues = First /@ differentSheetRealizations[sheetRealizations, wnum, z, r1 E^(I \[CurlyPhi]1), prec]; 
		(* calculating the sheet patches *)
       sheetPointArrays = sectorSheetPatchPoints[Sequence @@ ({
				                    {w, z}, w\[Psi]AndPrime, #, {odes\[CurlyPhi], {\[Psi], \[CurlyPhi]}, r}, {odesr, {\[Psi], \[Rho]}, dir}, 
	                                sector, {pp\[CurlyPhi], ppr}, prec, NDSolveOptions})]& /@ sheetValues;
       uniqueSheetDataArrays = dropMultipleSheetPointArrays[sheetPointArrays];
        (* interpolate data *)
       interpolateValuesAndDerivatives[#, {pp\[CurlyPhi]I, pprI}, 
                                       InterpolationOrder -> io]& /@ uniqueSheetDataArrays] // Quiet
makeConnectingPatches[patchArray_] := 
Block[{dr = Length[patchArray], d\[CurlyPhi] = Length[patchArray[[1]]],
        patchArrayPairsR, patchArrayPairs\[CapitalPhi], stitchPatches},
       (* radial *)
        patchArrayPairsR = Flatten[Table[{patchArray[[k, j]], patchArray[[k + 1, j]]},
                                         {k, dr - 1}, {j, d\[CurlyPhi]}], 1]; 
       (* azimuthal *)
        patchArrayPairs\[CapitalPhi] = Flatten[Table[{patchArray[[k, j]], patchArray[[k, Mod[j + 1, d\[CurlyPhi], 1]]]},
                                         {k, dr}, {j, d\[CurlyPhi]}], 1];   
       (* form stitch patches *)
        stitchPatches = Join[Flatten[addConnectingStripRadial /@ patchArrayPairsR, 1],
                             Flatten[addConnectingStripAzimuthal /@ patchArrayPairs\[CapitalPhi], 1]];
        If[debugQ, Print[Style[Row[{Text["Number of stitch patches:"], Text[Length[stitchPatches]]}], 
                               Darker[Green]]]];
        stitchPatches
        ]
glueDifference[{patchLine1_, patchPoints2_}] :=
With[{T = (Take[#, {2, -2}]& /@ #)&},
     (* ignore endpoints--because of branch cuts derivatives can be quite different *)
     Mean[Norm /@ Flatten[T[patchLine1 - patchPoints2], 1]]/(* scale *)
     If[# == 0., 1, #]&[Mean[Join[Norm /@ Flatten[T[patchLine1], 1], 
                                  Norm /@ Flatten[T[patchPoints2], 1]]]]
      ]       
addConnectingStripRadial[{patch1_, patch2_}] := 
Block[{n = Length[patch1], \[CurlyEpsilon] = 10.^-3, bottoms, tops, diffData},
       bottoms = Table[{Last @ patch1[[k, 1]], Last @ patch1[[k, 2]]}, {k, n}];
       tops = Table[{First @ patch2[[k, 1]], First @ patch2[[k, 2]]}, {k, n}];
       Table[diffData = Sort[{N[glueDifference[#]], #}& /@ Table[{bottoms[[i]], tops[[j]]}, {j, n}],
                             #1[[1]] < #2[[1]]&];
             If[diffData[[1, 1]] < \[CurlyEpsilon], Transpose[diffData[[1, 2]]], Sequence @@ {}],
            {i, n}]
      ]
addConnectingStripAzimuthal[{patch1_, patch2_}] := 
Block[{n = Length[patch1], \[CurlyEpsilon] = 10.^-3, lefts, rights, diffData},
       lefts = Table[{Last /@ patch1[[k, 1]], Last /@ patch1[[k, 2]]}, {k, n}]; 
       rights = Table[{First /@ patch2[[k, 1]], First /@ patch2[[k, 2]]}, {k, n}]; 
       Table[diffData = Sort[{N[glueDifference[#]], #}& /@ Table[{lefts[[i]], rights[[j]]}, {j, n}],
                             #1[[1]] < #2[[1]]&]; 
             If[diffData[[1, 1]] < \[CurlyEpsilon], Transpose[diffData[[1, 2]]], Sequence @@ {}],
            {i, n}]
      ]
RiemannSurfacePlot3D::cantFindAllBranchPoints=
"Unable to calculate all branchpoints for the function `1`.";
RiemannSurfacePlot3D::noBrachPointsPresent=
"`1` is not a multivalued function. Simply use Plot3D or ParametricPlot3D to display the surface.";
RiemannSurfacePlot3D::containsFunctionCurrentlyNotTreated=
"`1` contains the functions `2` which are are not treated in this implementation.";
Clear[branchCutFreeQI
];
branchCutFreeQI[f_, z_] := True /; FreeQ[f, z, {0, Infinity}]
branchCutFreeQI[z_^n_Integer, z_] := True 
branchCutFreeQI[z_, z_] := True 
branchCutFreeQI[p_Plus, z_] := And @@ (branchCutFreeQI[#, z]& /@ (List @@ p)) 
branchCutFreeQI[t_Times, z_] := And @@ (branchCutFreeQI[#, z]& /@ (List @@ t))
branchCutFreeQI[Power[f_, _Integer], z_] := branchCutFreeQI[f, z]
branchCutFreeQI[Power[f_, a_], z_] := True /; FreeQ[Together //@ f, z, {0, Infinity}]
branchCutFreeQI[Power[a_, f_], z_] := True /; FreeQ[a, z, {0, Infinity}]
branchCutFreeQI[(Exp | Sin | Cos | Tan | Cot | Sec | Csc | 
                 Sinh | Cosh | Tanh | Coth | Sech | Csch)[f_], z_] := 
                branchCutFreeQ[f, z]
(* gives true for a function that does not have branch cuts. 
   False can be returned in case of absence or presence of branch cuts *)
branchCutFreeQ[expr_, z_] := TrueQ[branchCutFreeQI[expr, z]]
notTreatedFunctions[w_, z_] := 
Block[{foreignFunction, u}, 
foreignFunction[z] = {};
foreignFunction[u] = {};
(* numerical values *)			
foreignFunction[_?NumericQ] := {};
foreignFunction[Power[b_, e_]] := foreignFunction /@ {b, e};
foreignFunction[pt_Plus|pt_Times] := foreignFunction /@ (List@@pt);
(* the known numeric functions *)		
Map[(foreignFunction[#[f_]] = foreignFunction[f])&, 
      	{Exp, Log, ProductLog,
           Sin, Cos, Tan, Cot, Sec, Csc, 
      	 Sinh, Cosh, Tanh, Coth, Sech, Csch, 
      	 ArcSin, ArcCos, ArcTan, ArcCot, ArcSec, ArcCsc, 
      	 ArcSinh, ArcCosh, ArcTanh, ArcCoth, ArcSech, ArcCsch}];
(* the Root case *)		
foreignFunction[Root[p_, __]] := foreignFunction[p[u]];
(* return first "foreign" function *)		
First /@ Union[Flatten[{foreignFunction[w]}]]]
numericQ[w_, z_] := 
	Block[{x}, SetAttributes[x, NumericFunction]; NumericQ[w //. z->x[1]]]
RiemannSurfacePlot3D::notANumericFunction = "`1` is not a numeric function of `2`.";
patchPlotPoints[{r1_, \[CurlyPhi]1_, r2_, \[CurlyPhi]2_}, rMax_, {pp\[CurlyPhi]_, ppr_}] := 
                Max[#, 4]& /@ Round[{pp\[CurlyPhi], ppr} {\[CurlyPhi]2 - \[CurlyPhi]1, r2 - r1}/{2 Pi, rMax}]
Clear[RiemannSurfacePatchData];
RiemannSurfacePatchData[args:PatternSequence[{w_, z_}, {pp\[CurlyPhi]_, ppr_}, {pp\[CurlyPhi]I_, pprI_}, ks_, 
                        branchPointOffset_, workingPrecision_, interpolationOrder_, NDSolveOptions___]] := 
Block[{wf, wcan, bps, secarray, wnum, \[CapitalPsi]\[CapitalPsi]p, \[CurlyPhi], rplts, subs, odes1, plts, 
		odes2, sr, asp, odes\[CurlyPhi], odesr, rMax, polys, test1, test2, test3, test4, oF, sheetGraphics, 
        \[Psi],  \[Rho] , r,  \[ScriptD], \[Lambda]1, \[Lambda]2, 
        allSheetPatchesArray,  allSheetPatches, allSheetPatchesArrayRes}, 
		     (* are all functions occuring in w known? *)
		     test1 = (oF = notTreatedFunctions[w, z]) === {};
		     If[test1, 
                 wcan = canonicalizeFunction[w];
          	   (* can we calculate all branch points *)
	             test2 = ((bps = N[branchPoints[w, z], workingPrecision]) =!= $Failed); 
			     (* is there at least one finite branch point *)
			   test3 = bps =!= {};  
               test4 = bps =!= $Failed;  
              allSheetPatchesArray = 
                    If[test2 && test3 && test4,
					   (* all the steps from above  *)
		               secarray = sectorArray[bps, branchPointOffset, workingPrecision]; 
                       If[debugQ, Print[Style[Row[{Text["Inside RiemannSurfacePatchData, numbering sheets ..."]}], 
                                              Darker[Green]]]];
                       wnum = numberMultiValuedTerms[wcan, z];
                       plts = powerAndLogTerms[wnum, z];
                       If[debugQ, Print[Style[Row[{Text["Inside RiemannSurfacePatchData, forming substitutions ..."]}], 
                                              Darker[Green]]]];
                       subs = substitutions[plts, wnum, wf, z];
                       If[debugQ, Print[Style[Row[{Text["Inside RiemannSurfacePatchData, forming differential equations ..."]}], 
                                              Darker[Green]]]];
                       odes1 = toODE[#, z]& /@ subs[[1]];
                       odes2 = derivativeFreeRhs[odes1]; 
                       \[CapitalPsi]\[CapitalPsi]p = {subs[[-1]], D[subs[[-1]], z] //. (Rule @@@ odes2)};
                       If[debugQ, Print[Style[Row[{Text["Inside RiemannSurfacePatchData, calculating sheets ..."]}], 
                                              Darker[Green]]]];
                       sr = sheets[#, ks]& /@ plts; 
                       asp = allSheets[sr];
                       odes\[CurlyPhi] = odeAzimuthal[#, {#[[1, 0, 1]], z}, {\[Psi], \[CurlyPhi]}, r]& /@ odes2;
                       odesr = odeRadial[#, {#[[1, 0, 1]], z}, {\[Psi], \[Rho]}, \[ScriptD]]& /@ odes2;
		               rMax = Max[#[[3]]& /@ Flatten[secarray, 1]];
                       If[debugQ, Print[Style[Row[{Text["Inside RiemannSurfacePatchData, calculating sector patches ..."]}], 
                                              Darker[Green]]]];
	                   \[Lambda]1 = Length[secarray]; \[Lambda]2 = Length[secarray[[1]]]; \[Lambda] = \[Lambda]1 \[Lambda]2;
                       Function[f, If[TrueQ[debugQ || sheetProgressMonitoringQ], 
                                      Monitor[f, Style[Text["\[SixPointedStar] Calculating sector " <> 
                                                               ToString[(jk1 - 1) \[Lambda]2 + jk2]  <> " out of " <>
                                                                    ToString[\[Lambda]]], Darker[ Green,0.2]]], f], {HoldAll}][
                       Table[sectorSheetPatches[{wf, z}, \[CapitalPsi]\[CapitalPsi]p, wnum, asp, 
				                          {odes\[CurlyPhi], {\[Psi], \[CurlyPhi]}, r}, {odesr, {\[Psi], \[Rho]}, \[ScriptD]}, secarray[[jk1, jk2]],
                                          patchPlotPoints[secarray[[jk1, jk2]], rMax, {pp\[CurlyPhi], ppr}], 
                                          patchPlotPoints[secarray[[jk1, jk2]], rMax, {pp\[CurlyPhi]I, pprI}],
                                          workingPrecision, interpolationOrder, NDSolveOptions], 
                              {jk1, \[Lambda]1}, {jk2, \[Lambda]2}]], 
                       $Failed]];
                (* return sheets *)
                allSheetPatches = If[allSheetPatchesArray =!= $Failed, Flatten[allSheetPatchesArray, 1], $Failed];
			    allSheetPatchesArrayRes = If[allSheetPatchesArray =!= $Failed &&
                                             TrueQ[And @@ (ArrayQ[#, 4, NumberQ]& /@ Flatten[allSheetPatches, 1])], 
                                             allSheetPatchesArray, $Failed]; 
                ((* cache last result to allow for plotting real/imaginary part quickly *) 
                 DownValues[RiemannSurfacePatchData] = Take[DownValues[RiemannSurfacePatchData], -1];
                 RiemannSurfacePatchData[args] = allSheetPatchesArrayRes;
                 allSheetPatchesArrayRes) /;
 	(* generate appropriate messages if something fails *)
     (If[Not[test1], Message[RiemannSurfacePlot3D::containsFunctionCurrentlyNotTreated, w, oF]];
	  If[test1 && Not[test2], Message[RiemannSurfacePlot3D::cantFindAllBranchPoints, w]];
	  If[test1 && Not[test3] && branchCutFreeQ[w, z], 
         Message[RiemannSurfacePlot3D::noBrachPointsPresent, w]];
	  TrueQ[test1 && test2 && test3])] /;   
      If[numericQ[w, z], True, RiemannSurfacePlot3D::notANumericFunction; False]
RiemannSurfaceGraphics3DData[{w_, z_}, reImMatrix_, colorFunction_,
                             {pp\[CurlyPhi]_, ppr_}, {pp\[CurlyPhi]I_, pprI_}, ks_, 
                             branchPointOffset_, workingPrecision_, 
                             interpolationOrder_, stitchQ_, NDSolveOptions___] := 
Block[{rsfpd = RiemannSurfacePatchData[{w, z}, {pp\[CurlyPhi], ppr}, {pp\[CurlyPhi]I, pprI}, ks, 
                                        branchPointOffset, workingPrecision, interpolationOrder, NDSolveOptions],
       stitchPatches, stitchCGs},  
     If[debugQ, Print[Style[Row[{Text["Call to RiemannSurfacePatchData with: "],
                       HoldForm[RiemannSurfacePatchData][{w, z}, {pp\[CurlyPhi], ppr}, {pp\[CurlyPhi]I, pprI}, ks, 
                                      branchPointOffset, workingPrecision, interpolationOrder, NDSolveOptions]}], 
                               Darker[Green]]]];
     If[rsfpd === $Failed || Head[rsfpd] === RiemannSurfacePatchData, $Failed,
        If[debugQ, Print[Style[Row[{Text["Inside RiemannSurfaceGraphics3DData, forming graphics complexes ..."]}], 
                               Darker[Green]]]];
        {sheetGraphicsComplex[reImMatrix, #, colorFunction]& /@ 
                             (* all patches for all sectors *) Flatten[rsfpd, 2],
         If[TrueQ[stitchQ], 
            If[debugQ, Print[Style[Row[{Text["Inside RiemannSurfaceGraphics3DData, stitching ..."]}], 
                               Darker[Green]]]];
            stitchPatches = makeConnectingPatches[rsfpd];
            stitchCGs = sheetGraphicsComplex[reImMatrix, #, colorFunction]& /@ stitchPatches, {}]}]]
RiemannSurfaceGraphics3DData[{eq_Equal, w_, z_}, reImMatrix_, colorFunction_,
                             {pp\[CurlyPhi]_, ppr_}, {pp\[CurlyPhi]I_, pprI_}, ks_, 
                             branchPointOffset_, workingPrecision_, 
                             interpolationOrder_, stitchQ_, NDSolveOptions___] := 
With[{sol = Quiet[Solve[eq, w]]}, 
      If[debugQ, Print[Style[Row[{Text["After solving for w, now plotting function "], w /. sol[[1, 1]]}], Darker[Green]]]];
	  RiemannSurfaceGraphics3DData[{w /. sol[[1, 1]], z}, reImMatrix, colorFunction,
                                   {pp\[CurlyPhi], ppr}, {pp\[CurlyPhi]I, pprI}, ks, 
                                   branchPointOffset, workingPrecision, 
                                   interpolationOrder, stitchQ, NDSolveOptions] /;
      Head[sol] =!= Solve && (* only one connected solution branch *)
      Total[Boole[MemberQ[First[#], z | w, {0, Infinity}]]& /@ FactorList[eq]] === 1]
makeMatrix[Re[w_], {z_, w_}] := MRe
makeMatrix[Im[w_], {z_, w_}] := MIm
makeMatrix[f_, {z_, w_}] := Coefficient[#, {Re[z], Im[z], Re[w], Im[w]}]& /@ {Re[z], Im[z], f}
makeMatrix[v3_List, {z_, w_}] := Coefficient[#, {Re[z], Im[z], Re[w], Im[w]}]& /@ v3
makeColorFunction[rhs_, {z_, w_}] := 
         Function[Evaluate[(rhs /. {Re[z] -> #1, Im[z] -> #2, Re[w] -> #3, Im[w] -> #4,
                                    Abs[z] -> (#1^2 + #2^2), Abs[w] -> (#3^2 + #4^2),
                                    Arg[z] -> ArcTan[#1, #2], Arg[z] -> ArcTan[#3, #4]})]]
makeColorFunction[Automatic, {z_, w_}] := Automatic
nonGraphics3DOptions = PlotPoints | PlotStyle | Coloring | WorkingPrecision |
                       BranchPointOffset | StitchPatches | NDSolveOptions | LogSheets | 
                       InterpolationPoints | InterpolationOrder;
Options[RiemannSurfacePlot3D] = 
Join[Options[Graphics3D] /. {(BoxRatios -> _) :> (BoxRatios -> {1, 1, 1.2}),
                             (PlotRange -> _) :> (PlotRange -> Automatic)}, 
         {InterpolationPoints -> Automatic, 
          PlotPoints -> {30, 12}, 
          PlotStyle -> Automatic, 
          Coloring -> Automatic, 
          InterpolationOrder -> Automatic, 
          BranchPointOffset -> 10^-6, 
          StitchPatches -> False,
          WorkingPrecision -> 25,
          LogSheets -> {-1, 0, 1}, 
          NDSolveOptions -> {}}];
RiemannSurfacePlot3D[eq_Equal, reimw_, {z_, w_}, opts:OptionsPattern[]] :=  
Block[{pps, ips, lsr, ndsos, bpo, wp, io, sp, MMat, colF, rsfgcs, rsfgcs1, ps,
        setGraphics3DOptions, defaultGraphics3DOptions, actualGraphics3DOptions}, 
       (* set options *)
       pps = OptionValue[PlotPoints] {1, 1};
       ips = OptionValue[InterpolationPoints] /. Automatic -> pps;
       lsr = OptionValue[LogSheets];
       ndsos = OptionValue[NDSolveOptions];
       bpo = OptionValue[BranchPointOffset]; 
       wp = OptionValue[WorkingPrecision]; 
       io = If[pps === ips, (* no point interpolating *) Automatic, OptionValue[InterpolationOrder]]; 
       sp = TrueQ @ OptionValue[StitchPatches]; 
       MMat = Developer`ToPackedArray[N[makeMatrix[reimw, {z, w}]]];
       colF = makeColorFunction[OptionValue[Coloring], {z, w}];
       (* main call *)       
       rsfgcs = RiemannSurfaceGraphics3DData[{eq, w, z}, MMat, colF, pps, ips, lsr, bpo, wp, io, sp,
                                             Sequence @@ ndsos];
       (ps = OptionValue[PlotStyle];
       (* add plot style *)
        rsfgcs1 = If[ps === Automatic, rsfgcs, {ps, rsfgcs}];
       (* form graphics 3D options *) 
       setGraphics3DOptions = DeleteCases[{opts}, nonGraphics3DOptions -> _];
       defaultGraphics3DOptions = DeleteCases[Options[RiemannSurfacePlot3D], nonGraphics3DOptions -> _];
       actualGraphics3DOptions = Sequence @@ Join[setGraphics3DOptions, defaultGraphics3DOptions];
       (* form and return Graphics3D-object *)
       Graphics3D[rsfgcs1, actualGraphics3DOptions]) /; 
            Head[rsfgcs] =!= RiemannSurfaceGraphics3DData && rsfgcs =!= $Failed &&
            Max[Exponent[#, {Re[z], Im[z], Re[w], Im[w]}]& /@ Flatten[{reimw}]] === 1 && 
            MatchQ[lsr, {_Integer ..}] && Positive[bpo] && Positive[wp]
       ]
End[]