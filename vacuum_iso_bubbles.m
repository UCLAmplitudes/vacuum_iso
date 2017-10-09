(* ::Package:: *)

<<IGraphM`;


(* ::Chapter:: *)
(*General Graph Functions*)


ClearAll[standardGraphRep,graphPlot,toMmaGraph,toMmaUndirGraph,toMmaAdj]

standardGraphRep::usage = "standardGraphRep[G] takes a graph G in in our standard notation
{{-1},{-2},...,{1,5,-7},...}, and translates to the OLD mathematica graph notation";
standardGraphRep[graph_] := Module[{}, Table[{Position[graph, j][[1, 1]] -> Position[graph, -j][[1, 1]], j}, {j, Union[Abs[Flatten[graph]]]}]];

graphPlot::usage = "graphPlot[G] takes a graph G in in our standard notation
{{-1},{-2},...,{1,5,-7},...}, and displays it using the OLD mathematica graph routines";
graphPlot[graph_] := GraphPlot[standardGraphRep[graph], DirectedEdges -> True]

toMmaGraph::usage = "toMmaGraph[G] takes a graph G in in our standard notation
{{-1},{-2},...,{1,5,-7},...}, and translates to the NEW mathematica graph notation";
toMmaGraph[graph_]:=Graph@Table[Position[graph, j][[1, 1]]\[DirectedEdge]Position[graph, -j][[1, 1]], {j, Union[Abs[Flatten[graph]]]}]

toMmaUndirGraph::usage = "toMmaUndirGraph[G] takes a graph G in in our standard notation
{{-1},{-2},...,{1,5,-7},...}, translates to the NEW mathematica graph notation, 
and forgets about the orientations";
toMmaUndirGraph[graph_]:=Graph@Table[Position[graph, j][[1, 1]]<->Position[graph, -j][[1, 1]], {j, Union[Abs[Flatten[graph]]]}]

toMmaAdj::usage="toMmaAdj[g] turns graph g in our standard notation
{{-1},{-2},...,{1,5,-7},...} into its adjacency graph in the NEW Mathematica notation.";
toMmaAdj[graph_]:=(Graph[Abs[Select[graph,Length[#]>1&]]/.{{x__}/;Depth[{x}]<3:>Sequence@@(Sort/@EdgeList@CompleteGraph[Length[{x}]]/.MapThread[(#1->#2)&,{VertexList@CompleteGraph[Length[{x}]],{x}}])},
VertexLabels->Automatic]);


ClearAll[cubicQ,nEdges,nLoops,connectedQ]

cubicQ::usage="cubiQ[G] = True if the graph G given in our standard 
notation {{-1},{-2},...,{1,5,-7},...}) is trivalent";
cubicQ[graph_]:=Union[Length/@Select[graph,Length[#]>1&]]=={3}

nEdges::usage="nEdges[G] returns the number of edges of the graph G given 
in our standard notation {{-1},{-2},...,{1,5,-7},...}";
nEdges[graph_]:=Length[Union@@Abs[graph]]

nLoops::usage="nLoops[G] returns the number of loops of the graph G given 
in our standard notation {{-1},{-2},...,{1,5,-7},...}";
nLoops[graph_]:=Length@FindFundamentalCycles@(toMmaUndirGraph[graph])

connectedQ::usage= "connectedQ[V1,V2] = True if the vertives V1 and V2 are connected by at least one edge";
connectedQ[{vert1_,vert2_}]:=(Length[Union@@#]>Length[Union@@Abs@#])&@{vert1,vert2}


ClearAll[isomorphicQ, isomorphicAdjQ];

isomorphicAdjQ::usage= "isomorphicAdjQ[g1,g2] = True if there is an isomorphism
between g1 and g2. The comparison uses the adjacency graphs. Uses IGraphM when there are bubbles present.";
isomorphicAdjQ[diag1_List,diag2_List]:=Module[
{g1,g2,bubQ,igmFlag=ContainsAny[$Packages,{"IGraphM`"}]},
        g1 = toMmaAdj[diag1];
        g2 = toMmaAdj[diag2];
        bubQ = And[MultigraphQ[g1],MultigraphQ[g2]];
        If[bubQ&&igmFlag,
                Return[IGIsomorphicQ[g1,g2]]];
        Return[IsomorphicGraphQ[g1,g2]]
];

isomorphicQ::usage= "isomorphicQ[g1,g2] = True if there is an isomorphism
between g1 and g2.  Uses IGraphM when there are bubbles present.";
isomorphicQ[diag1_List,diag2_List]:=Module[
{g1,g2,bubQ,igmFlag=ContainsAny[$Packages,{"IGraphM`"}]},
        g1 = toMmaUndirGraph[diag1];
        g2 = toMmaUndirGraph[diag2];
        bubQ = And[MultigraphQ[g1],MultigraphQ[g2]];
        If[bubQ&&igmFlag,
                Return[IGIsomorphicQ[g1,g2]]];
        Return[IsomorphicGraphQ[g1,g2]]
];


ClearAll[collapsePropagator]
collapsePropagator::usage= "collapsePropagator[G,L] takes graph G in our 
standard notation {{-1},{-2},...,{1,5,-7},...}, and pinches the leg L. It 
also admists a list of legs as an argument and will collapse all of them.";
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


ClearAll[momCons]
momCons::usage = " momCons[G,n,L] solves momentum conservation for the graph G given
in our standard notation {{-1},{-2},...,{1,5,-7},...}. Optionally one can specify the
number n of external particles and thenumber of loops L in order to solve for the right
variables. By default it works for five-loop vacuum graphs.";
momCons[graph_,n_:0,L_:5]:=Module[{mom},
mom[i_]/;i<0:=-mom[-i];
mom[i__]:=mom/@{i};
mom[i_]/;Abs[i]<n:=k[i];
mom[n]:= - Total[k/@Range[1,n-1]];
mom[i_]/;Abs[i]>n:=l[i];
Solve[(Total/@(mom@@@(Select[graph,Length[#]>1&])))==0,l/@Range[n+L+1,nEdges[graph]]][[1]]
]


(* ::Chapter::Closed:: *)
(*Tadpole-like objects*)


(* Starting to build similar functionality for tadpoles *)


ClearAll[findTadpoleLegs,findTadpoles,hasTadpolesQ,hasDanglingSunsetQ,isomorphicQuptoTadpoles]

findTadpoleLegs[vertex_]:=Flatten[DeleteDuplicates[Abs@Intersection[vertex,-vertex]]];

findTadpoles[graph_]:=Select[graph,((Length[Union@#]-Length[Union@Abs@#])>0)&]

nTadpoles[graph_]:=Length[Union@@findTadpoleLegs/@findTadpoles[graph]];

hasTadpolesQ[graph_]:=Length[findTadpoles[graph]]>0

hasDanglingSunsetQ[graph_]:=Or@@PossibleZeroQ/@Length/@Catenate@(findOutLegs/@findBubbleLike[graph,3,0])

isomorphicQuptoTadpoles::usage="isomorphicQuptoTadpoles[G1,G2] = True if the two graphs G1 and G2 given in our 
standard notation {{-1},{-2},...,{1,5,-7},...} can be made isomorphic by moving the tadpoles around. 
This version compares only compares graphs with 'sliding bubbles' that have the same number of propagators";
isomorphicQuptoTadpoles[g1_,g2_]/;(Not[hasTadpolesQ[g1]]&&Not[hasTadpolesQ[g2]]):=isomorphicQ[g1,g2]
isomorphicQuptoTadpoles[g1_,g2_]/;(nTadpoles[g1]!=nTadpoles[g2]):=False
isomorphicQuptoTadpoles[g1_,g2_]:=isomorphicQuptoBubbles[collapsePropagator[g1,Union@@findTadpoleLegs/@findTadpoles[g1]],collapsePropagator[g2,Union@@findTadpoleLegs/@findTadpoles[g2]]]


(* ::Chapter:: *)
(*Bubble-like objects*)


ClearAll[findOutLegs,findInLegs]

findOutLegs::usage="findOutLegs[B] finds the outgoing legs in each vertex of
the bubble-like object B";
findOutLegs[bubble_]:={Complement@@({1,-1}bubble),Complement@@({1,-1}Reverse[bubble])}

findInLegs::usage="findInLegs[B] finds the incoming legs in each vertex of the
bubble-like object B";
findInLegs[bubble_]:=Intersection@@({1,-1}bubble)


ClearAll[findBubbleLike,hasBubbleLikeQ]

findBubbleLike::usage = "findBubbleLike[G] searchs for 'Bubble-Like' objects within a graph given in our standard notation {{-1},{-2},...,{1,5,-7},...}.
It has two optional arguments. The first one selects the multiplicity of the 'Bubble-Like' objects (2->bubble, 3->sunset etc).
The second one specifies information about the edges going out of the bubble:
If it is 0 it looks for objects with only one set of edges coming out (e.g., a dangling sunset).
If it is 1 it looks for objects with a single outgoing edge on at least one side.
If it is 2 it looks for objects with a single outgoing edge on both sides.
For other values it looks for objects with any configuration of outgoing edges.
By default the function looks for object with all multiplicities and at least a single ourgoing edge.";
findBubbleLike[graph_,multiplicity_:0,valence_:1]/;Depth[graph]>3 := findBubbleLike[#,multiplicity,valence]&/@graph
findBubbleLike[graph_,multiplicity_:0,valence_:1]:=Module[{pairs,bubbles},
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

hasBubbleLikeQ::usage= "hasBubbleLikeQ checks whether a bubble-like object with given properties exists withing a graph";
hasBubbleLikeQ[graph_,multiplicity_:0,valence_:1]:=Length[findBubbleLike[graph,multiplicity,valence]]>0

nBubbleLike::usage= "nBubbleLikeQ counts bubble-like object with given properties exists withing a graph";
nBubbleLike[graph_,multiplicity_:0,valence_:1]:=Length[findBubbleLike[graph,multiplicity,valence]]


ClearAll[slideBubbles]
slideBubbles::usage=" slideBubbles[G] takes the bubble-like objects within the graph G (given in our standard {{-1},{-2},...,{1,5,-7},...},)
and slides them in all possible ways by exchanging the bubble-like objects with the adjacent edges. 
Optionally a one or set of bubble-like objects can be provided and it will slide only those.
WARNING: For the moment this does not keep track of powers of propagators.";
slideBubbles[graph_,{}]:={{graph}};
slideBubbles[graph_,bubbles_:{}]/;Length[bubbles]==1:=slideBubbles[graph,First@bubbles];
slideBubbles[graph_,bubbles_:{}]/;Depth[graph]>3:=slideBubbles[#,bubbles]&/@graph;
slideBubbles[graph_,bubbles_:{}]/;Depth[bubbles]>3:=Module[{newGraphs,bubblesofNewGraphs,newBubbles,newslid},
newGraphs=slideBubbles[graph,First@bubbles];
bubblesofNewGraphs=(findBubbleLike/@newGraphs);
newBubbles=Select[#,(ContainsAll[findInLegs/@Drop[bubbles,1],{findInLegs[#]}])&]&/@bubblesofNewGraphs;
Return[Union[Fold[Union,Flatten[#,Depth[#]-4]&/@MapThread[slideBubbles,{newGraphs,newBubbles}]],SameTest->isomorphicQ]];
];
slideBubbles[graph_,bubbles_:{}]/;And[Depth[graph]==3,Depth[bubbles]==3]:=Module[{outLegs},
    If[Not@MemberQ[findBubbleLike[graph],bubbles],
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
slideBubbles[graph_,bubbles_:{}]/;bubbles=={}&&findBubbleLike[graph]=={}:={graph};
slideBubbles[graph_,bubbles_:{}]/;bubbles=={}:=slideBubbles[graph,findBubbleLike@graph];


ClearAll[isomorphicQuptoBubbles,isomorphicQuptoBubblesOld]
isomorphicQuptoBubblesOld::usage="isomorphicQuptoBubblesOld[G1,G2] = True if the two graphs G1 and G2 given in our 
standard notation {{-1},{-2},...,{1,5,-7},...} can be made isomorphic by 'sliding bubbles'. 
This version compares all possible ways of 'sliding bubbles' in both graphs";
isomorphicQuptoBubblesOld[g1_,g2_]/;(Not[hasBubbleLikeQ[g1]]&&Not[hasBubbleLikeQ[g2]]):=isomorphicQ[g1,g2]
isomorphicQuptoBubblesOld[g1_,g2_]/;(nBubbleLike[g1,0,-1]!=nBubbleLike[g2,0,-1]):=False
isomorphicQuptoBubblesOld[g1_,g2_]:=Or@@isomorphicQ@@@Catenate@Outer[List,slideBubbles[g1],slideBubbles[g2],1]

isomorphicQuptoBubbles::usage="isomorphicQuptoBubbles[G1,G2] = True if the two graphs G1 and G2 given in our 
standard notation {{-1},{-2},...,{1,5,-7},...} can be made isomorphic by 'sliding bubbles'. 
This version compares only compares graphs with 'sliding bubbles' that have the same number of propagators";
isomorphicQuptoBubbles[g1_,g2_]/;(Not[hasBubbleLikeQ[g1]]&&Not[hasBubbleLikeQ[g2]]):=isomorphicQ[g1,g2]
isomorphicQuptoBubbles[g1_,g2_]/;(nBubbleLike[g1,0,-1]!=nBubbleLike[g2,0,-1]):=False
isomorphicQuptoBubbles[g1_,g2_]:=Module[{a1,a2,temp},
a1=GroupBy[slideBubbles[g1],nEdges];
a2=GroupBy[slideBubbles[g2],nEdges];
temp=Catenate/@Merge[{KeyTake[a1,#],KeyTake[a2,#]}&@Intersection[Keys[a1],Keys[a2]],Outer[List,#[[1]],#[[2]],1]&];
temp=Or@@Values@(Or@@isomorphicQ@@@#&/@temp)
]