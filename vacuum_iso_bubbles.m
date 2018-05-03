(* ::Package:: *)

BeginPackage["vacuumisobubbles`"];

Needs["IGraphM`"];


(* ::Chapter:: *)
(*Documentation*)


(* General Graph Functions / Translation between our notation and Mathematica's *)
standardGraphRep::usage = "standardGraphRep[G] takes a graph G in in our standard notation
{{-1},{-2},...,{1,5,-7},...}, and translates to the OLD mathematica graph notation";
graphPlot::usage = "graphPlot[G] takes a graph G in in our standard notation
{{-1},{-2},...,{1,5,-7},...}, and displays it using the OLD mathematica graph routines";
toMmaGraph::usage = "toMmaGraph[G] takes a graph G in in our standard notation
{{-1},{-2},...,{1,5,-7},...}, and translates to the NEW mathematica graph notation";
toMmaUndirGraph::usage = "toMmaUndirGraph[G] takes a graph G in in our standard notation
{{-1},{-2},...,{1,5,-7},...}, translates to the NEW mathematica graph notation, 
and forgets about the orientations";
toMmaAdj::usage="toMmaAdj[g] turns graph g in our standard notation
{{-1},{-2},...,{1,5,-7},...} into its adjacency graph in the NEW Mathematica notation.";
toMmaGraphwLab::usage = "toMmaGraph[G] takes a graph G in in our standard notation
{{-1},{-2},...,{1,5,-7},...}, and translates to the NEW mathematica graph notation
collapsing bubble-like objects into a single edge with several labels";

cubicQ::usage="cubiQ[G] = True if the graph G given in our standard 
notation {{-1},{-2},...,{1,5,-7},...}) is trivalent";
nEdges::usage="nEdges[G] returns the number of edges of the graph G given 
in our standard notation {{-1},{-2},...,{1,5,-7},...}";
nLoops::usage="nLoops[G] returns the number of loops of the graph G given 
in our standard notation {{-1},{-2},...,{1,5,-7},...}";
connectedQ::usage= "connectedQ[V1,V2] = True if the vertives V1 and V2 are connected by at least one edge";
collapsePropagator::usage= "collapsePropagator[G,L] takes graph G in our 
standard notation {{-1},{-2},...,{1,5,-7},...}, and pinches the leg L. It 
also admists a list of legs as an argument and will collapse all of them.";
removePropagator::usage= "removePropagator[G,L] takes graph G in in our 
standard notation {{-1},{-2},...,{1,5,-7},...}, and deletes the leg L. It 
also admists a list of legs as an argument and will delete all of them.";
removeExtLegs::usage=""
removeDots::usage=""
remove1PRLegs::usage=""
momCons::usage = " momCons[G,n,L] solves momentum conservation for the graph G given
in our standard notation {{-1},{-2},...,{1,5,-7},...}. Optionally one can specify the
number n of external particles and thenumber of loops L in order to solve for the right
variables. By default it works for five-loop vacuum graphs.";


(* Graph Isomorphism related functions *)
isomorphicAdjQ::usage= "isomorphicAdjQ[g1,g2] = True if there is an isomorphism
between g1 and g2. The comparison uses the adjacency graphs. Uses IGraphM when there are bubbles present.";
isomorphicQ::usage= "isomorphicQ[g1,g2] = True if there is an isomorphism
between g1 and g2.  Uses IGraphM when there are bubbles present.";
isomorphismRule::usage = "isomorphismRule[G1,G2] provides an isomorphism between graphs G1 and G2
given in our standard notation {{-1},{-2},...,{1,5,-7},...}.
I also works for graphs with bubbles, sunsets etc.";
isomorphismRules::usage = "isomorphismRule[G1,G2] provides all isomorphisms between graphs G1 and G2
given in our standard notation {{-1},{-2},...,{1,5,-7},...}.
I also works for graphs with bubbles, sunsets etc.";
automorphismRules::usage = "automorphismRules[G] all automorphisms of graph G given 
in our standard notation {{-1},{-2},...,{1,5,-7},...}.
I also works for graphs with bubbles, sunsets etc.";


(* Functions to deal with bubble-like objects *)
findOutLegs::usage="findOutLegs[B] finds the outgoing legs in each vertex of
the bubble-like object B";
findInLegs::usage="findInLegs[B] finds the incoming legs in each vertex of the
bubble-like object B";
findBubbleLike::usage = "findBubbleLike[G] searchs for 'Bubble-Like' objects within a graph given in our standard notation {{-1},{-2},...,{1,5,-7},...}.
It has two optional arguments. The first one selects the multiplicity of the 'Bubble-Like' objects (2->bubble, 3->sunset etc).
The second one is called valence and it encodes information about the edges going out of the bubble:
If it is 0 it looks for objects with only one set of edges coming out (e.g., a dangling sunset).
If it is 1 it looks for objects with a single outgoing edge on at least one side.
If it is 2 it looks for objects with a single outgoing edge on both sides.
For other values it looks for objects with any configuration of outgoing edges.
By default the function looks for object with all multiplicities and ano configuration of out\.08going edges.";
hasBubbleLikeQ::usage= "hasBubbleLikeQ checks whether a bubble-like object with given properties exists withing a graph";
nBubbleLike::usage= "nBubbleLikeQ counts bubble-like object with given properties exists withing a graph";
slideBubbles::usage=" slideBubbles[G] takes the bubble-like objects within the graph G (given in our standard {{-1},{-2},...,{1,5,-7},...},)
and slides them in all possible ways by exchanging the bubble-like objects with the adjacent edges. 
Optionally a one or set of bubble-like objects can be provided and it will slide only those.";
isomorphicQuptoBubblesAll::usage="isomorphicQuptoBubblesOld[G1,G2] = True if the two graphs G1 and G2 given in our 
standard notation {{-1},{-2},...,{1,5,-7},...} can be made isomorphic by 'sliding bubbles'. 
This version compares all possible ways of 'sliding bubbles' in both graphs";
isomorphicQuptoBubbles::usage="isomorphicQuptoBubbles[G1,G2] = True if the two graphs G1 and G2 given in our 
standard notation {{-1},{-2},...,{1,5,-7},...} can be made isomorphic by 'sliding bubbles'. 
This version compares only compares graphs with 'sliding bubbles' that have the same number of propagators";


(* Functions to deal with tadpoles *)
findTadpoleLegs::usage= "findTadpoleLegs[G] finds any legs in diagram G given in our standard notation 
{{-1},{-2},...,{1,5,-7},...}, which correspond to a simple tadpole.";
findTadpoles::usage= "findTadpoleLegs[G] finds any vertices in diagram G given in our standard notation 
{{-1},{-2},...,{1,5,-7},...}, which contain simple tadpoles.";
nTadpoles::usage= "nTadpoles[G] counts how many simple tadpoles there are in diagram G given in our 
standard notation  {{-1},{-2},...,{1,5,-7},...}.";
hasTadpolesQ::usage= "hasTadpolesQ[G] = True if diagram G given in our standard notation  
{{-1},{-2},...,{1,5,-7},...} contains any simple tadpoles.";
hasDanglingSunsetQ::usage= "hasDanglingSunsetQ[G] = True if diagram G given in our standard notation  
{{-1},{-2},...,{1,5,-7},...} contains any dangling sunset graphs.";
isomorphicQuptoTadpoles::usage="isomorphicQuptoTadpoles[G1,G2] = True if the two graphs G1 and G2 given in our 
standard notation {{-1},{-2},...,{1,5,-7},...} can be made isomorphic by removing the tadpoles and sliding bubbles.";


(* Functions to deal with vacuum graphs and multiplicity/FIRE-style notation" *)
findDots::usage = "";
graphToMult::usage = "graphToMult[G] takes the graph G given
in our standard notation {{-1},{-2},...,{1,5,-7},...} and uses
momentum conservation in the vertices to bundle together doubled 
propagators, producing a daughter graph representative.";
multToGraph::usage = "multToGraph[M,G] takes a parent graph G given
in our standard notation {{-1},{-2},...,{1,5,-7},...} and a list of 
multiplicities M of the different edges and produces the daughter graph 
with the right propagator structure by pinching the edges in the parent
with negative or zero multiplicity.";
toVacuum::usage = "toVacuum[G] takes a graph G given in our standard 
notation {{-1},{-2},...,{1,5,-7},...} and produces the corresponding vacuum graph.
By default it considers 4-point graphs, but it has an optional argument to 
specify a different number.";
findVacuumRep::usage = "findVacuumRep[G,B] takes a graph G and a set of 
graphs B both given in our standard notation {{-1},{-2},...,{1,5,-7},...}, 
and returns:
a) a representative of G in slideBubblesp[G]
b) corresponding graph in B that is isomorphic to (a), if any.
The function has an optional third argument which when True only returns the
indices of (a) in slideBubblesp[G] and of (b) in the Association B.
If there is no matching representative it returns an empty list.";
multIsoPerm::usage = "multIsoPerm[M,I] takes the multiplicities M and the isomorphism
I and returns the multiplicities reordered according to I";


(* ::Chapter::Closed:: *)
(*Functions*)


Begin["`Private`"];


(* ::Chapter:: *)
(*General Graph Functions*)


ClearAll[standardGraphRep,graphPlot,toMmaGraph,toMmaUndirGraph,toMmaAdj,toMmaGraphwLab]

standardGraphRep[graph_] := Module[{}, Table[{Position[graph, j][[1, 1]] -> Position[graph, -j][[1, 1]], j}, {j, Union[Abs[Flatten[graph]]]}]]

graphPlot[graph_] := GraphPlot[standardGraphRep[graph], DirectedEdges -> True]

toMmaGraph[graph_]:=Graph@Table[Position[graph, j][[1, 1]]\[DirectedEdge]Position[graph, -j][[1, 1]], {j, Union[Abs[Flatten[graph]]]}]

toMmaUndirGraph[graph_]:=Graph@Table[Position[graph, j][[1, 1]]<->Position[graph, -j][[1, 1]], {j, Union[Abs[Flatten[graph]]]}]

toMmaAdj[graph_]:=(Graph[Abs[Select[graph,Length[#]>1&]]/.{{x__}/;Depth[{x}]<3:>Sequence@@(Sort/@EdgeList@CompleteGraph[Length[{x}]]/.MapThread[(#1->#2)&,{VertexList@CompleteGraph[Length[{x}]],{x}}])},
VertexLabels->Automatic]);

toMmaGraphwLab[graph_?hasBubbleLikeQ]:=Module[{labels,bubbleop},
labels=GroupBy[standardGraphRep[graph],First[#]/.Rule->DirectedEdge&->Last];
bubbleop=-labels/@(Keys[labels]/.{DirectedEdge[a_,b_]:>DirectedEdge[b,a]})/.Missing[__]->{};
labels=AssociationThread[Keys@labels,MapThread[Union,{Values@labels,bubbleop}]];
labels=DeleteDuplicates[labels,Sort@#1==Sort[-#2]&];
Graph[Keys[labels],EdgeLabels->Normal[labels]]
];
toMmaGraphwLab[graph_]:=Graph@@{First@Transpose[#],EdgeLabels->Rule@@@#}&@standardGraphRep[graph]


ClearAll[cubicQ,nEdges,nLoops,connectedQ]

cubicQ[graph_]:=Union[Length/@Select[graph,Length[#]>1&]]=={3}

nEdges[graph_]:=Length[Union@@Abs[graph]]

nLoops[graph_]:=Length@FindFundamentalCycles@(toMmaUndirGraph[graph])

connectedQ[{vert1_,vert2_}]:=(Length[Union@@#]>Length[Union@@Abs@#])&@{vert1,vert2}


ClearAll[collapsePropagator]

collapsePropagator[diagram_, leg_List]:=Fold[collapsePropagator,diagram,leg]
collapsePropagator[diagram_, leg_] := Module[{graph, a1, a2, a3, b1,b2,b3,b4},
   graph = diagram;
   graph = graph /. {a1___, {b2___, leg, b1___}, a2___, {b4___, -leg, b3___},
                       a3___} :> {a1, {b1, b2, b3, b4}, a2, a3};
   graph = graph /. {a1___, {b2___, -leg, b1___}, a2___, {b4___, leg, b3___},
                       a3___} :> {a1, {b1, b2, b3, b4}, a2, a3};
   graph = graph /. {a1___,{b1___, leg, b2___, -leg, b3___}, a2___}:> {a1,{b1,b2,b3},a2};
   graph = graph /. {a1___,{b1___, -leg, b2___, leg, b3___}, a2___}:> {a1,{b1,b2,b3},a2};
Return[graph]]


ClearAll[removePropagator]

removePropagator[diagram_, leg_List]:=Fold[removePropagator,diagram,leg]
removePropagator[diagram_, leg_] := Module[{graph, a1, a2, a3, b1,b2,b3,b4},
   graph = diagram;
   graph = graph /. {a1___, {b2___, leg, b1___}, a2___, {b4___, -leg, b3___},
                       a3___} :> {a1, {b2, b1}, a2, {b4, b3}, a3};
   graph = graph /. {a1___, {b2___, -leg, b1___}, a2___, {b4___, leg, b3___},
                       a3___} :> {a1, {b2, b1}, a2, {b4, b3}, a3};
Return[graph]]


ClearAll[removeExtLegs,removeDots,remove1PRLegs,removeTadpoles]
removeExtLegs[graph_]:=FixedPoint[collapsePropagator[#,Catenate@Cases[#,{a_}]]&,graph]
removeDots[graph_]:=FixedPoint[collapsePropagator[#,Cases[#,{a_,b_}:>Max[Abs[a],Abs[b]]]]&,graph]
remove1PRLegs[graph_]:=collapsePropagator[graph,Part[Union@@Abs[graph],Catenate@Position[ConnectedGraphQ/@toMmaUndirGraph/@(removePropagator[graph,#]&/@Union@@Abs[graph]),False]]]
removeTadpoles[graph_]:=FixedPoint[collapsePropagator[#,findTadpoleLegs/@findTadpoles[#]]&,graph]



ClearAll[momCons]

momCons[graph_,n_:0,L_:5]:=Module[{mom},
mom[i_]/;i<0:=-mom[-i];
mom[i__]:=mom/@{i};
mom[i_]/;Abs[i]<n:=k[i];
mom[n]:= - Total[k/@Range[1,n-1]];
mom[i_]/;Abs[i]>n:=l[i];
Solve[(Total/@(mom@@@(Select[graph,Length[#]>1&])))==0(*,l/@Range[n+L+1,nEdges[graph]]*)][[1]]
]


(* ::Chapter:: *)
(*Graph Isomorphism*)


ClearAll[isomorphicQ, isomorphicAdjQ];

isomorphicAdjQ[diag1_List,diag2_List]:=Module[
{g1,g2,bubQ,igmFlag=ContainsAny[$Packages,{"IGraphM`"}]},
        g1 = toMmaAdj[diag1];
        g2 = toMmaAdj[diag2];
        (* Changed following check from And to Or because Mathematica 10 has some bugs when finding Multigraphs *)
        bubQ = And[MultigraphQ[g1],MultigraphQ[g2]];
        If[bubQ&&igmFlag,
                Return[IGIsomorphicQ[g1,g2]]];
        Return[IsomorphicGraphQ[g1,g2]]
];

isomorphicQ[diag1_List,diag2_List]:=Module[
{g1,g2,bubQ,igmFlag=ContainsAny[$Packages,{"IGraphM`"}]},
        g1 = toMmaUndirGraph[diag1];
        g2 = toMmaUndirGraph[diag2];
        (* Changed following check from And to Or because Mathematica 10 has some bugs when finding Multigraphs *)
        bubQ = And[MultigraphQ[g1],MultigraphQ[g2]]; 
        If[bubQ&&igmFlag,
                Return[IGIsomorphicQ[g1,g2]]];
        Return[IsomorphicGraphQ[g1,g2]]
];

(* Workaround for Mathematica 10 bug *)
If[Not@OrderedQ[{10.0, 2}, {$VersionNumber, $ReleaseNumber}],
 ClearAll[isomorphicQ];
 isomorphicQ[diag1_List,diag2_List]:=IGIsomorphicQ[toMmaUndirGraph@diag1,toMmaUndirGraph@diag2];
]



ClearAll[isomorphismRule]

isomorphismRule[{},graph_]:={};
isomorphismRule[graph_,{}]:={};
isomorphismRule[graph1_?hasBubbleLikeQ,graph2_?hasBubbleLikeQ]:=Module[{mmagraph1,mmagraph2,colors1,colors2,vertexiso,edges1,edges2,targetedges},
  mmagraph1=toMmaGraphwLab[graph1];
  mmagraph2=toMmaGraphwLab[graph2];
  colors1 = Length/@Association@@(PropertyValue[mmagraph1,EdgeLabels]/.DirectedEdge[a_,b_]:>UndirectedEdge[a,b]);
  colors2 = Length/@Association@@(PropertyValue[mmagraph2,EdgeLabels]/.DirectedEdge[a_,b_]:>UndirectedEdge[a,b]);
  vertexiso = IGVF2GetIsomorphism[{UndirectedGraph@mmagraph1,"EdgeColors"->colors1},{UndirectedGraph@mmagraph2,"EdgeColors"->colors2}];
  edges1=PropertyValue[mmagraph1,EdgeLabels]/.Rule[a_,b_]:>Rule[a/.vertexiso,b];
  edges2=PropertyValue[mmagraph2,EdgeLabels];
  targetedges=Catenate[(((First/@edges1)/.edges2)/.DirectedEdge[a_,b_]:>-DirectedEdge[b,a])/.edges2];
  Sort@Catenate[Thread/@Thread[(Last/@edges1)-> targetedges]]/.Rule[a_?Negative,b_]:>Rule[-a,-b]
]
isomorphismRule[graph1_,graph2_]:=Module[{mmagraph1,mmagraph2,colors1,colors2,vertexiso,edges1,edges2,targetedges},
  mmagraph1=toMmaGraphwLab[graph1];
  mmagraph2=toMmaGraphwLab[graph2];
  vertexiso = IGVF2GetIsomorphism[UndirectedGraph@mmagraph1,UndirectedGraph@mmagraph2];
  edges1=PropertyValue[mmagraph1,EdgeLabels]/.Rule[a_,b_]:>Rule[a/.vertexiso,b];
  edges2=PropertyValue[mmagraph2,EdgeLabels];
  targetedges=Catenate[(((First/@edges1)/.edges2)/.DirectedEdge[a_,b_]:>-DirectedEdge[b,a])/.edges2];
  Sort[Thread[(Last/@edges1)-> targetedges]]/.Rule[a_?Negative,b_]:>Rule[-a,-b]
]


ClearAll[isomorphismRules]
isomorphismRules[graph1_,graph2_]:=Module[{mmagraph1,mmagraph2,colors1,colors2,vertexiso,edges1,edges2,targetedges,bubbleisos},
  mmagraph1=toMmaGraphwLab[graph1];
  mmagraph2=toMmaGraphwLab[graph2];
  colors1 = Length/@Association@@(PropertyValue[mmagraph1,EdgeLabels]/.DirectedEdge[a_,b_]:>UndirectedEdge[a,b]);
  colors2 = Length/@Association@@(PropertyValue[mmagraph2,EdgeLabels]/.DirectedEdge[a_,b_]:>UndirectedEdge[a,b]);
  vertexiso = IGVF2FindIsomorphisms[{UndirectedGraph@mmagraph1,"EdgeColors"->colors1},{UndirectedGraph@mmagraph2,"EdgeColors"->colors2}];
  edges1=Table[PropertyValue[mmagraph1,EdgeLabels]/.Rule[a_,b_]:>Rule[a/.iso,b],{iso,vertexiso}];
  edges2=PropertyValue[mmagraph2,EdgeLabels];
  targetedges=Table[(((First/@aux)/.edges2)/.DirectedEdge[a_,b_]:>-DirectedEdge[b,a])/.edges2,{aux,edges1}];
  bubbleisos=MapThread[Sort[Thread[(Last/@#1)-> #2]]&,{edges1,targetedges}];
  bubbleisos//.{x___,Rule[a_List,b_List],y___}:>Sequence@@({x,Sequence@@#,y}&/@ Map[Thread[Rule[a,#]]&, Permutations[b]])/.Rule[a_?Negative,b_]:>Rule[-a,-b]
]


ClearAll[automorphismRules]
(*automorphismRules[graph_]:=isomorphismRules[graph,graph]*)
automorphismRules[graph_]:=Module[{mmagraph,colors,vertexiso,edges1,edges2,targetedges,bubbleisos},
  mmagraph=toMmaGraphwLab[graph];
  colors = Length/@Association@@(PropertyValue[mmagraph,EdgeLabels]/.DirectedEdge[a_,b_]:>UndirectedEdge[a,b]);
  vertexiso = IGVF2FindIsomorphisms[{UndirectedGraph@mmagraph,"EdgeColors"->colors},{UndirectedGraph@mmagraph,"EdgeColors"->colors}];
  edges1=Table[PropertyValue[mmagraph,EdgeLabels]/.Rule[a_,b_]:>Rule[a/.iso,b],{iso,vertexiso}];
  edges2=PropertyValue[mmagraph,EdgeLabels];
  targetedges=Table[(*Catenate[*)(((First/@aux)/.edges2)/.DirectedEdge[a_,b_]:>-DirectedEdge[b,a])/.edges2(*]*),{aux,edges1}];
  bubbleisos=MapThread[Sort[Thread[(Last/@#1)-> #2]]&,{edges1,targetedges}];
  bubbleisos//.{x___,Rule[a_List,b_List],y___}:>Sequence@@({x,Sequence@@#,y}&/@ Map[Thread[Rule[a,#]]&, Permutations[b]])/.Rule[a_?Negative,b_]:>Rule[-a,-b]
]



(* ::Chapter:: *)
(*Bubble-like objects*)


ClearAll[findOutLegs,findInLegs]

findOutLegs[bubble_]:={Complement@@({1,-1}bubble),Complement@@({1,-1}Reverse[bubble])}

findInLegs[bubble_]:=Intersection@@({1,-1}bubble)


ClearAll[findBubbleLike,hasBubbleLikeQ]

findBubbleLike[graph_,multiplicity_:0,valence_:-1]/;Depth[graph]>3 := findBubbleLike[#,multiplicity,valence]&/@graph
findBubbleLike[graph_,multiplicity_:0,valence_:-1]:=Module[{pairs,bubbles},
    pairs=Select[Subsets[graph,{2}],connectedQ];
    If[multiplicity==0,
      bubbles=Select[pairs,((Length[Union@@#]-Length[Union@@Abs@#])>1)&],
      bubbles=Select[pairs,((Length[Union@@#]-Length[Union@@Abs@#])==multiplicity)&]
    ];
    Which[
      valence==0, Select[bubbles, Or[Length@First@findOutLegs[#]==0,Length@Last@findOutLegs[#]==0]&],
      valence==1, Select[bubbles, Or[Length@First@findOutLegs[#]==1,Length@Last@findOutLegs[#]==1]&],
      valence==2, Select[bubbles, And[Length@First@findOutLegs[#]==1,Length@Last@findOutLegs[#]==1]&],
      True, bubbles
    ]
];

hasBubbleLikeQ[graph_,multiplicity_:0,valence_:-1]:=Length[findBubbleLike[graph,multiplicity,valence]]>0

nBubbleLike[graph_,multiplicity_:0,valence_:-1]:=Length[findBubbleLike[graph,multiplicity,valence]]


ClearAll[slideBubbles]
slideBubbles[graph_,{}]:={{graph}};
slideBubbles[graph_,bubbles_:{}]/;Length[bubbles]==1:=slideBubbles[graph,First@bubbles];
slideBubbles[graph_,bubbles_:{}]/;Depth[graph]>3:=slideBubbles[#,bubbles]&/@graph;
slideBubbles[graph_,bubbles_:{}]/;Depth[bubbles]>3:=Module[{newGraphs,bubblesofNewGraphs,newBubbles,newslid},
newGraphs=slideBubbles[graph,First@bubbles];
bubblesofNewGraphs=(findBubbleLike[#,0,1]&/@newGraphs);
newBubbles=Select[#,(ContainsAll[findInLegs/@Drop[bubbles,1],{findInLegs[#]}])&]&/@bubblesofNewGraphs;
Return[DeleteDuplicates[{graph}~Join~(Fold[Union,Flatten[#,Depth[#]-4]&/@MapThread[slideBubbles,{newGraphs,newBubbles}]])]];
];
slideBubbles[graph_,bubbles_:{}]/;And[Depth[graph]==3,Depth[bubbles]==3]:=Module[{outLegs},
    If[Not@MemberQ[findBubbleLike[graph,0,1],bubbles],
      Print["Bubble is not in graph"];,
      outLegs=findOutLegs@bubbles;
      Which[
        Length/@outLegs=={1,1},
        Return[{graph,collapsePropagator[graph,outLegs[[1,1]]],collapsePropagator[graph,outLegs[[2,1]]]}],
        Length@First@outLegs==1,
        Return[{graph,Append[collapsePropagator[graph,outLegs[[1,1]]]/.{bubbles[[2]]-> Join[outLegs[[2]],outLegs[[1]]]},Join[Complement[bubbles[[2]],outLegs[[2]]],-outLegs[[1]]]]}],
        Length@Last@outLegs==1,
        Return[{graph,Append[collapsePropagator[graph,outLegs[[2,1]]]/.{bubbles[[1]]-> Join[outLegs[[1]],outLegs[[2]]]},Join[Complement[bubbles[[1]],outLegs[[1]]],-outLegs[[2]]]]}]
      ]
    ]
]
slideBubbles[graph_,bubbles_:{}]/;bubbles=={}&&findBubbleLike[graph,0,1]=={}:={graph};
slideBubbles[graph_,bubbles_:{}]/;bubbles=={}:=slideBubbles[graph,findBubbleLike[graph,0,1]];


ClearAll[isomorphicQuptoBubblesAll,isomorphicQuptoBubbles]
isomorphicQuptoBubblesAll[g1_,g2_]/;(Not[hasBubbleLikeQ[g1,0,1]]&&Not[hasBubbleLikeQ[g2,0,1]]):=isomorphicQ[g1,g2]
isomorphicQuptoBubblesAll[g1_,g2_]/;(nBubbleLike[g1]!=nBubbleLike[g2]):=False
isomorphicQuptoBubblesAll[g1_,g2_]:=Or@@isomorphicQ@@@Catenate@Outer[List,Union[slideBubbles[g1],SameTest->isomorphicQ],Union[slideBubbles[g2],SameTest->isomorphicQ],1]

isomorphicQuptoBubbles[g1_,g2_]/;(Not[hasBubbleLikeQ[g1,0,1]]&&Not[hasBubbleLikeQ[g2,0,1]]):=isomorphicQ[g1,g2]
isomorphicQuptoBubbles[g1_,g2_]/;(nBubbleLike[g1]!=nBubbleLike[g2]):=False
isomorphicQuptoBubbles[g1_,g2_]:=Module[{a1,a2,temp},
a1=GroupBy[Union[slideBubbles[g1],SameTest->isomorphicQ],nEdges];
a2=GroupBy[Union[slideBubbles[g2],SameTest->isomorphicQ],nEdges];
temp=Catenate/@Merge[{KeyTake[a1,#],KeyTake[a2,#]}&@Intersection[Keys[a1],Keys[a2]],Outer[List,#[[1]],#[[2]],1]&];
temp=Or@@Values@(Or@@isomorphicQ@@@#&/@temp)
]


(* ::Chapter:: *)
(*Tadpole-like objects*)


ClearAll[findTadpoleLegs,findTadpoles,hasTadpolesQ,hasDanglingSunsetQ,isomorphicQuptoTadpoles]

findTadpoleLegs[vertex_]:=Flatten[DeleteDuplicates[Abs@Intersection[vertex,-vertex]]];

findTadpoles[graph_]:=Select[graph,((Length[Union@#]-Length[Union@Abs@#])>0)&]

nTadpoles[graph_]:=Length[Union@@findTadpoleLegs/@findTadpoles[graph]];

hasTadpolesQ[graph_]:=Length[findTadpoles[graph]]>0

hasDanglingSunsetQ[graph_]:=Or@@PossibleZeroQ/@Length/@Catenate@(findOutLegs/@findBubbleLike[graph,3,0])

isomorphicQuptoTadpoles[g1_,g2_]/;(Not[hasTadpolesQ[g1]]&&Not[hasTadpolesQ[g2]]):=isomorphicQ[g1,g2]
isomorphicQuptoTadpoles[g1_,g2_]/;(nTadpoles[g1]!=nTadpoles[g2]):=False
isomorphicQuptoTadpoles[g1_,g2_]:=isomorphicQuptoBubbles[collapsePropagator[g1,Union@@findTadpoleLegs/@findTadpoles[g1]],collapsePropagator[g2,Union@@findTadpoleLegs/@findTadpoles[g2]]]


(* ::Chapter:: *)
(*Vacuum graphs and dots*)


ClearAll[graphToMult,multToGraph,toVacuum,findVacuumRep,multIsoPerm]

graphToMult[graph_]:=Module[{rules,invrules,mult,collapsed},
rules=momCons[graph];
invrules=Cases[momCons[graph],(a_->b_Plus)->((b^2)->a^2)]//Factor;
mult=KeyMap[(#/.{l[a_]^2:>a})&,Counts[(((l/@Union@@Abs[graph])/.rules)^2//Factor)/.invrules]];
collapsed=Association@@((#->0)&/@Complement[Union@@Abs[graph],Keys[mult]]);
Values@KeySort@Union[mult,collapsed]
]

findDots[fullgraph_,reducedgraph_]:=Module[{dots,zeros},
rules=momCons[fullgraph];
dots=AssociationThread[Union@@Abs[reducedgraph],Count[(l[#]^2&/@Union@@Abs[fullgraph])/.rules//Factor,#]&/@((l[#]^2&/@Union@@Abs[reducedgraph])/.rules//Factor)];
zeros=AssociationThread@@{#,ConstantArray[0,Length[#]]}&@Complement[Union@@Abs[fullgraph],Union@@Abs[reducedgraph]];
Values@KeySort@Union[dots,zeros]
]

multToGraph[mult_,parent_]:=collapsePropagator[parent,Part[Union@@Abs[parent],Catenate@Position[mult,x_?NonPositive]]]

(*toVacuum[graph_,n_:4]:=Module[{auxgraph,tocollaps},
  auxgraph=(collapsePropagator[graph,Range[n]]/.{a_Integer :> Sign[a](Abs[a]-n)});
  auxgraph=FixedPoint[collapsePropagator[#,Catenate@Cases[#,{a_}]]&,auxgraph];
  auxgraph=FixedPoint[collapsePropagator[#,Cases[#,{a_,b_}\[RuleDelayed]Abs[a]]]&,auxgraph];
  multToGraph[graphToMult[auxgraph],auxgraph]/.{a_Integer :> Sign[a](Abs[a]+n)}
];*)


toVacuum[graph_,n_:4]:=Module[{auxgraph},
  auxgraph=FixedPoint[(*removeDots[*)removeExtLegs[remove1PRLegs[#]](*]*)&,graph];
  multToGraph[graphToMult[auxgraph],auxgraph]
];


findVacuumRep[graph_,basis_List,onlyIndices_:False]:=findVacuumRep[graph,GroupBy[basis,nEdges]]
findVacuumRep[graph_?hasTadpolesQ,basis_,onlyIndices_:False]:=Module[{isomorphic},
  isomorphic=Flatten@Position[Outer[isomorphicQuptoTadpoles,slideBubbles[graph],basis@nEdges[graph],1],True];
  If[isomorphic=={}, 
    Return[{}], 
    If[onlyIndices,
      Return[{First@isomorphic,{nEdges[graph],Last@isomorphic}}],
      Return[{{First@isomorphic,{nEdges[graph],Last@isomorphic}},slideBubbles[graph][[First@isomorphic]],basis[nEdges[graph]][[Last@isomorphic]]}]
    ];
  ];
];
findVacuumRep[graph_,basis_Association,onlyIndices_:False]:=Module[{isomorphic,foundLevel},
  isomorphic={};
  foundLevel=0;
  Do[
     isomorphic=Flatten@Position[Outer[isomorphicQ,slideBubbles[graph],basis[basislevel],1],True];
     If[isomorphic =!= {}, foundLevel=basislevel;Break[]];
  ,{basislevel,basis//Keys}];
  If[isomorphic==={}, 
    Return[{}], 
    If[onlyIndices,
      Return[{First@isomorphic,{foundLevel,Last@isomorphic}}],
      Return[{{First@isomorphic,{foundLevel,Last@isomorphic}},slideBubbles[graph][[First@isomorphic]],basis[foundLevel][[Last@isomorphic]]}]
    ];
  ];
];

multIsoPerm[mult_,{}]:={}
multIsoPerm[mult_,iso_,offset_:0]:=Module[{max,multaux,aux,auxzero},
aux=KeyMap[(#-offset)&,KeySort[Abs/@Association@@iso]];
max=Max[Length[mult],Keys[aux],Values[aux]];
auxzero=AssociationThread[Complement[Range[max],Keys[aux]],Complement[Range[max],Values[aux]]];
multaux=If[Length[mult]<max,Join[mult,ConstantArray[0,max-Length[mult]]],mult];
Permute[multaux,Values@(Normal@KeySort@Union[aux,auxzero])]
]

(*multIsoPerm[mult_,{}]:={};
(*multIsoPer[mult_,iso_]/;Length[iso]\[Equal]Length[mult]:=*)
multIsoPerm[mult_,iso_,offset_:0]:=Module[{max,multaux,aux,auxrest},
aux=KeyMap[(#-offset)&,KeySort[Abs/@Association@@iso]];
max=Max[Keys[#],Values[Abs/@#]]&@aux;
(*Print[max];*)
multaux=If[Length[mult]<max,Join[mult,ConstantArray[0,max-Length[mult]]],mult];
auxrest=AssociationThread[Catenate@Position[multaux,0],Complement[Range[Length@multaux],Values[aux]]];
Permute[multaux,Values@(Normal@KeySort@Union[aux,auxrest])]
]*)


 


End[]; (* `Private` *)

EndPackage[];
