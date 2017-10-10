(* ::Package:: *)

If[$MachineName=="julio-mba",
  SetDirectory[NotebookDirectory[]];
  <<VacuumData5Loops;,
  Get["/u/project/bern/bern/fourloop/bcj/math_integrateN8/VacuumData5Loops"]
];
<<vacuum_iso_bubbles.m;
<<fiveloop.m
<<vacuum_basis.m;


graphToMult::usage = "graphToMult[G] takes the graph G given
in our standard notation {{-1},{-2},...,{1,5,-7},...} and uses
momentum conservation in the vertices to bunble together doubled 
propagators, producing a daughter graph representative."
graphToMult[graph_]:=Module[{rules,invrules,mult,collapsed},
rules=momCons[graph];
invrules=Cases[momCons[graph],(a_->b_Plus)->((b^2)->a^2)]//Factor;
mult=KeyMap[(#/.{l[a_]^2:>a})&,Counts[(((l/@Range[nEdges[graph]])/.rules)^2//Factor)/.invrules]];
collapsed=Association@@((#->0)&/@Complement[Range[nEdges[graph]],Keys[mult]]);
Values@KeySort@Union[mult,collapsed]
]

multToGraph::usage = "multToGraph[M,G] takes a parent graph G given
in our standard notation {{-1},{-2},...,{1,5,-7},...} and a list of 
multiplicities M of the different edges and produces a doughter graph 
with the right propagator structure by pinching edges in the parent
with negative of zero multiplicity."
multToGraph[mult_,parent_]:=collapsePropagator[parent,Catenate@Union[Position[mult,0],Position[mult,x_Integer?Negative]]]
toVacuum[graph_,n_:4]:=Module[{auxgraph,tocollaps},
  auxgraph=(collapsePropagator[graph,Range[n]]/.{a_Integer :> Sign[a](Abs[a]-n)});
  multToGraph[graphToMult[auxgraph],auxgraph]/.{a_Integer :> Sign[a](Abs[a]+n)}
];

findVacuumRep::usage = "findVacuumRep[G,B] takes a graph G given
in our standard notation {{-1},{-2},...,{1,5,-7},...} and a list of 
graphs B (see below) in the same notation and finds a representative of B by sliding bubbles,
and a corresponding graph in B that is isomorphic to the representative, if any.
For speed B must be an Association of the form #ofEdges->{List of graphs}"
findVacuumRep[graph_?hasTadpolesQ,basis_]:=Insert[First@Position[Outer[isomorphicQuptoTadpoles,slideBubbles[graph],basis@nEdges[graph],1],True],nEdges[graph],{2}]
findVacuumRep[graph_,basis_]:=Insert[First@Position[Outer[isomorphicQ,slideBubbles[graph],basis@nEdges[graph],1],True],nEdges[graph],{2}]


graphPlot@multToGraph[{2,2,1,1,1,1,1,1,1,1,0,0},cont[0][[-5]]]
findVacuumRep[multToGraph[{2,2,0,1,-1,-1,1,1,1,1,0,0},cont[0][[-5]]],basiswfact]


representatives=Table[findVacuumRep[toVacuum[Diag[i]],basiswfact],{i,1,100}];


testcase=84;
graphPlot@slideBubbles[toVacuum[Diag[testcase]]][[representatives[[testcase,1]]]]
graphPlot@basiswfact[representatives[[testcase,2]]][[representatives[[testcase,3]]]]



