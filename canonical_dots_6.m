(* ::Package:: *)

nloops=ToExpression@$CommandLine[[2]];
ntower = ToExpression@$CommandLine[[3]];
maxndots=ToExpression@$CommandLine[[4]];

(*nloops=6;
ntower=2;
maxndots=2;*)


If[$Notebooks, SetDirectory[NotebookDirectory[]];];
<<vacuum_iso_bubbles.m;
Get["tables/vacuum_basis_"<>ToString[nloops]<>"/vacuum_basis_"<>ToString[nloops]<>"_"<>ToString[ntower]<>".m"];


totndots=Binomial[nloops,2]+nloops;


ClearAll[permDots];
inverseIso[iso_]:=iso/.Rule[a_,b_]:>Rule[b,a]/.Rule[a_?Negative,b_]:>Rule[-a,-b]
permDots[dots_,auto_,withauto_:False]:=Module[{n=Length[dots],perm,res},
perm=Abs/@Values@KeySort@(Sort@Join[auto,Thread[Complement[Range[totndots],First/@auto]->Complement[Range[totndots],Abs/@Last/@auto]]]);
If[withauto,{Permute[dots,perm],inverseIso@auto},Permute[dots,perm]]
];
addDots[mult_,graph_]:=Module[{props=Union@@Abs@graph,dots},
dots=Part[IdentityMatrix[Length@mult],props];
Table[(mult+dots[[i]]),{i,1,Length@dots}]
];


Print["Generating canonical configurations in tower ",ntower,"..."];
Do[
	Print["    Doing prop level ",nprop,"..."];
	
	Do[
		Print["        Doing diag num ",ndiag,"/",Length[basisnofact[ntower][nprop]],"..."];
		ClearAll[F,H,graph,automorphisms,canonical,dotrules,rules,canonicaldots,permuted,autoaux];
		graph=basisnofact[ntower][nprop][[ndiag]];
		automorphisms=Catenate@isoTables[ntower][nprop][[ndiag]];

		Print["            Finding canonical configurations with up to "<>ToString[maxndots]<>" dots..."];
		ndots=0;
		rules={};
		canonicaldots={};
		addeddots={(If[MemberQ[Union@@Abs@graph,#],1,0]&/@Range[totndots])};
		aux=First@addeddots;
		permuted=(permDots[aux,#,True]&/@automorphisms);
		autoaux=Last/@Cases[permuted,{aux,b__}];
		AppendTo[canonicaldots,{aux,autoaux}];
		rules=Join[rules,{(First@#->aux),Last@#}&/@List@@@Normal[Map[Last]/@GroupBy[permuted,First]]];
		addeddots=DeleteCases[addeddots,Alternatives@@(First@@@rules)];
		canonical[ndots]=canonicaldots;
		dotrules[ndots]=DeleteCases[rules,{Rule[a_,a_],b_}]/.{(A_List->B_List):>(H[ntower,A])->(H[ntower,B])};
		Print["            "<>ToString[ndots]<>" dot(s) done."];
		
		timing=Do[
			rules={};
			canonicaldots={};
			addeddots=Flatten[addDots[#,graph]&/@First/@canonical[ndots-1],1];
			While[addeddots=!={},
				aux=First@addeddots;
				permuted=(permDots[aux,#,True]&/@automorphisms);
				autoaux=Last/@Cases[permuted,{aux,b__}];
				AppendTo[canonicaldots,{aux,autoaux}];
				rules=Join[rules,{(First@#->aux),Last@#}&/@List@@@Normal[Map[Last]/@GroupBy[permuted,First]]];
				addeddots=DeleteCases[addeddots,Alternatives@@(First@@@rules)];
			];
			canonical[ndots]=canonicaldots;
			dotrules[ndots]=DeleteCases[rules,{Rule[a_,a_],b_}]/.{(A_List->B_List):>(H[ntower,A])->(H[ntower,B])};
			Print["            "<>ToString[ndots]<>" dot(s) done."];
		,{ndots,1,maxndots}]//AbsoluteTiming//First;
		Print["            Dot configurations found in ",timing," s.  Saving..."];

		
		
		savefile="canonical_dots/canonical_dots_"<>ToString[nloops]<>"_"<>ToString[ntower]<>"_"<>ToString[nprop]<>"_"<>ToString[ndiag]<>"_"<>ToString[maxndots]<>".m";
		
		dotsRules[nprop,ndiag]=AssociationThread[Range[0,maxndots],dotrules/@Range[0,maxndots]];
		dotsCanonical[nprop,ndiag]=AssociationThread[Range[0,maxndots],Map[{H[ntower,First@#],Last[#]}&]/@(canonical/@Range[0,maxndots])];
		dotsCount[nprop,ndiag]=Length/@dotsCanonical[nprop,ndiag];
		If[FileExistsQ[savefile],DeleteFile[savefile]];
		Save[savefile,{dotsCount,dotsCanonical,dotsRules}];
		Clear[dotsRules,dotsCanonical,dotsCount];
		
		Print["            Done with diagram ",ndiag,"!"];
		
		
,{ndiag,Length[basisnofact[ntower][nprop]]}];
	Print["        Done with prop level ",nprop,"!"];
,{nprop,Keys[basisnofact[ntower]]}]



