(* ::Package:: *)

If[$MachineName=="julio-mba",
  SetDirectory[NotebookDirectory[]];
  <<VacuumData5Loops;,
  Get["/u/project/bern/bern/fourloop/bcj/math_integrateN8/VacuumData5Loops"]
];
<<vacuum_iso_bubbles.m;
<<fiveloop.m
<<vacuum_basis.m;


ClearAll[graphToMult,multToGraph,findVacuumRep]

graphToMult::usage = "graphToMult[G] takes the graph G given
in our standard notation {{-1},{-2},...,{1,5,-7},...} and uses
momentum conservation in the vertices to bundle together doubled 
propagators, producing a daughter graph representative.";
graphToMult[graph_]:=Module[{rules,invrules,mult,collapsed},
rules=momCons[graph];
invrules=Cases[momCons[graph],(a_->b_Plus)->((b^2)->a^2)]//Factor;
mult=KeyMap[(#/.{l[a_]^2:>a})&,Counts[(((l/@Range[nEdges[graph]])/.rules)^2//Factor)/.invrules]];
collapsed=Association@@((#->0)&/@Complement[Range[nEdges[graph]],Keys[mult]]);
Values@KeySort@Union[mult,collapsed]
]

multToGraph::usage = "multToGraph[M,G] takes a parent graph G given
in our standard notation {{-1},{-2},...,{1,5,-7},...} and a list of 
multiplicities M of the different edges and produces the daughter graph 
with the right propagator structure by pinching the edges in the parent
with negative or zero multiplicity.";
multToGraph[mult_,parent_]:=collapsePropagator[parent,Catenate@Union[Position[mult,0],Position[mult,x_Integer?Negative]]]
toVacuum[graph_,n_:4]:=Module[{auxgraph,tocollaps},
  auxgraph=(collapsePropagator[graph,Range[n]]/.{a_Integer :> Sign[a](Abs[a]-n)});
  multToGraph[graphToMult[auxgraph],auxgraph]/.{a_Integer :> Sign[a](Abs[a]+n)}
];

findVacuumRep::usage = "findVacuumRep[G,B] takes a graph G and a set of 
graphs B both given in our standard notation {{-1},{-2},...,{1,5,-7},...}, 
and returns:
a) a representative of G in slideBubblesp[G]
b) corresponding graph in B that is isomorphic to (a), if any.
The function has an optional third argument which when True only returns the
indices of (a) in slideBubblesp[G] and of (b) in the Association B.";
findVacuumRep[graph_,basis_List,onlyIndices_:False]:=findVacuumRep[graph,GroupBy[basis,nEdges]]
findVacuumRep[graph_?hasTadpolesQ,basis_,onlyIndices_:False]:=If[onlyIndices,{#[[1]],{#[[2]],#[[3]]}},{slideBubbles[graph][[#[[1]]]],basis[#[[2]]][[#[[3]]]]}]&@Insert[First@Position[Outer[isomorphicQuptoTadpoles,slideBubbles[graph],basis@nEdges[graph],1],True],nEdges[graph],{2}]
findVacuumRep[graph_,basis_Association,onlyIndices_:False]:=If[onlyIndices,{#[[1]],{#[[2]],#[[3]]}},{slideBubbles[graph][[#[[1]]]],basis[#[[2]]][[#[[3]]]]}]&@Insert[First@Position[Outer[isomorphicQ,slideBubbles[graph],basis@nEdges[graph],1],True],nEdges[graph],{2}]


graphPlot@multToGraph[{2,2,1,1,1,1,1,1,1,1,0,0},cont[0][[-5]]]
findVacuumRep[multToGraph[{2,2,0,1,-1,-1,1,1,1,1,0,0},cont[0][[-5]]],basiswfact]


representatives=Table[findVacuumRep[toVacuum[Diag[i]],basiswfact],{i,1,100}];


graphPlot/@representatives[[1]]
