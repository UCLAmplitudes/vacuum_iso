(* ::Package:: *)

nprop=ToExpression@$CommandLine[[2]];
ndiag=ToExpression@$CommandLine[[3]];
maxndots=ToExpression@$CommandLine[[4]];


savefile="canonical_dots/canonical_dots_"<>ToString[nprop]<>"_"<>ToString[ndiag]<>"_"<>ToString[maxndots]<>".m";


If[$Notebooks, SetDirectory[NotebookDirectory[]];];
<<vacuum_iso_bubbles.m;
<<vacuum_basis.m;


ClearAll[permDots];
permDots[dots_,auto_,withperm_:False]:=Module[{n=Length[dots],auxauto,res},
auxauto=Sort@Join[auto,(#->#)&/@Complement[Range[n],First/@auto]];
res={Permute[dots,Abs/@Values@KeySort@#],#}&@(auxauto);
If[Not[withperm],First@res,res]
];
addDots[mult_,graph_]:=Module[{props=Union@@Abs@graph,dots},
dots=Part[IdentityMatrix[Length@mult],props];
Table[(mult+dots[[i]]),{i,1,Length@dots}]
];


ClearAll[F,graph,automorphisms,canonical,dotrules,rules,canonicaldots,permuted,autoaux];
graph=basisnofact[nprop][[ndiag]];
automorphisms=automorphismRules[graph];
canonical[0]={{(If[MemberQ[Union@@Abs@graph,#],1,0]&/@Range[15]),automorphisms}};
dotrules[0]={};
Print["Finding canonical configurations with up to "<>ToString[maxndots]<>" dots..."];
Do[
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
dotrules[ndots]=DeleteCases[rules,{Rule[a_,a_],b_}]/.{(A_List->B_List):>F@@A->F@@B};
Print[ToString[ndots]<>" dot(s) done."];
,{ndots,1,maxndots}]//AbsoluteTiming//First
Print["All done."];


dotsRules[nprop,ndiag]=AssociationThread[Range[0,maxndots],dotrules/@Range[0,maxndots]];
dotsCanonical[nprop,ndiag]=AssociationThread[Range[0,maxndots],Map[{F@@First@#,Last[#]}&]/@(canonical/@Range[0,maxndots])];
dotsCount[nprop,ndiag]=Length/@dotsCanonical[nprop,ndiag];


If[FileExistsQ[savefile],DeleteFile[savefile]];
Save[savefile,{dotsCount,dotsCanonical,dotsRules}];
