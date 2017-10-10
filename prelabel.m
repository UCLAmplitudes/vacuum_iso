(* ::Package:: *)

If[$MachineName=="julio-mba",
  SetDirectory[NotebookDirectory[]];
  <<VacuumData5Loops;,
  Get["/u/project/bern/bern/fourloop/bcj/math_integrateN8/VacuumData5Loops"]
];
<<vacuum_iso_bubbles.m;
<<fiveloop.m
<<vacuum_basis.m;


graphToMult[graph_]:=Module[{rules,invrules,mult,collapsed},
rules=momCons[graph];
invrules=Cases[momCons[graph],(a_->b_Plus)->((b^2)->a^2)]//Factor;
mult=KeyMap[(#/.{l[a_]^2:>a})&,Counts[(((l/@Range[nEdges[graph]])/.rules)^2//Factor)/.invrules]];
collapsed=Association@@((#->0)&/@Complement[Range[nEdges[graph]],Keys[mult]]);
Values@KeySort@Union[mult,collapsed]
]
multToGraph[mult_,parent_]:=collapsePropagator[parent,Catenate@Union[Position[mult,0],Position[mult,x_Integer?Negative]]]

(*toVacuum[graph_,n_:4]:=Module[{auxgraph,tocollaps},
  auxgraph=(collapsePropagator[graph,Range[n]]/.{a_Integer \[RuleDelayed] Sign[a](Abs[a]-n)});
  tocollaps=Catenate@Position[graphToMult[auxgraph],0];
  collapsePropagator[auxgraph,tocollaps]/.{a_Integer \[RuleDelayed] Sign[a](Abs[a]+n)}
];*)
toVacuum[graph_,n_:4]:=Module[{auxgraph,tocollaps},
  auxgraph=(collapsePropagator[graph,Range[n]]/.{a_Integer :> Sign[a](Abs[a]-n)});
  multToGraph[graphToMult[auxgraph],auxgraph]/.{a_Integer :> Sign[a](Abs[a]+n)}
];

findVacuumRep[graph_?hasTadpolesQ,basis_]:=Insert[First@Position[Outer[isomorphicQuptoTadpoles,slideBubbles[graph],basis@nEdges[graph],1],True],nEdges[graph],{2}]
findVacuumRep[graph_,basis_]:=Insert[First@Position[Outer[isomorphicQ,slideBubbles[graph],basis@nEdges[graph],1],True],nEdges[graph],{2}]


(*graphPlot@multToGraph[{2,2,1,1,1,1,1,1,1,1,0,0},cont[0][[-5]]]*)
findVacuumRep[multToGraph[{2,2,0,1,-1,-1,1,-1,1,1,0,0},cont[0][[-5]]],basiswfact]


(* ::InheritFromParent:: *)
(*Graphics[Annotation[GraphicsComplex[CompressedData["*)
(*1:eJw1jHk01AkAx3+OtUu5mqIZ54SelVderhS+v5Cj7VFJRU3rnK190tqtVqbD*)
(*JmXdu3lKWCQbnWzpGuSMxTKuMYxkV+YyZpJqZWaabf/o+97nfd7nny89+sjO*)
(*OE2CIPw/8r8/LW9rOJsVKgNr54GNJTOvsFuL9dnYvjk41tON18wQpHS/yNFm*)
(*twyBwT+c1XwzBuIN/fOsYidIx9Jlt2kKfPpptVxaKHCZQ0xd2CN7KRcbjP0p*)
(*euG1OBX0LvtD1gwCixzOn5iqw4XDBo1f9kiQbnumfIP7E2ynahQzdCSo+tWB*)
(*Jt3ZCH0PsQNvkxgpUWVmlbRmcO20xbx4EWzlNcXZshY0PP6TXVYohHXqponi*)
(*a22IbLvIT2wRYPlz7s9Bns+guZnfmCucRkWEoN1OMIzLBZ771dmDqFw/Kcq4*)
(*yoVrXbWI0d+PdwmZnb4hI0j1bSn71oADC+dQb5p0BE2ulO64zB7cuH7W8nkS*)
(*D3lbe/7KlHciJPZuQORbHmrOJM8e3NqOxdDcmpNRo0hftfIhvaQZR+tdjxo1*)
(*fGxybcaTfjYiLUanrI9cxGNmvmIPXwLrSlgy7Athv3r5C3+ZGJOl3rdsJKWY*)
(*yGEZv9UUw8g1zWrNaAVWm38TfosqQsooQfdBFSKiVozVrReiZtG7763dbejq*)
(*n1CGBwtQlrCNObGlFut2pe2djJ9GVa/z6+/d7+P0FPu33JyXSH9qyGLf7MMv*)
(*fJWZms9BU1YY5X45B1XV1PZj23rRxKXdZ5/sh9vQZr8n8i7kWVS8ucscwIDN*)
(*3tzRqx34vVrECvh6EAvc1gvjzDY4pVEiXJhDkIbFtj7zbMbp6yJrW9YwrPUO*)
(*HHf8qh7aXeWqpyVcfOdg9W+a3wPs8qLuYbYRpKbPRGL+cwkWbZLzFx8RpJG+*)
(*gRvzlRi0dHnyqZsEeUsYyHinLYaJu/GW+SKCjCgoeJhjJkL2spUDkRkEyZkX*)
(*Jia5CKG9ZM+17uMEKRDbqRUhApgtpLzwiCLIwIZkjaiEaczw2P5/BBHkVNe4*)
(*syz3JW4jcpnsbxUma9XH58c4WGBL32vcVUHPm5IwGNSL3uTsucAkFV7GGgdn*)
(*SLvgFhdAH/JSQQa/fGppB05w2sfr1EocnfMIyYhuA/NSUYlGoxKdOjd9Jt2b*)
(*YWpor+pOUmJhmOLoTNZjZTP9joOTEkMB1bqMDQ9gGhRWGx9JkHLne5mmuTMw*)
(*WTWga+5JkA7/xHWY90rwinHZ4ycqQboXeZmv+UKCnrM6LsFzamRNUgtTvMQw*)
(*kJ2Tr7unxuy2kY79CSL8yPO2YiepUaMVW0gUCRF86YWJpo8azOyYHXdaBbhE*)
(*if+wwlCN96kHE6tF0zhcviu1lKGEnq3p60NZgzCI6dqRt1YJgL/7XF8/NnpZ*)
(*dsZrKaEjOLcveCkH9muL433GFYi6piG9cb4HJtPV5y0eK+BGCxjeIemEke8s*)
(*TfeKAvKhU973/NoRsoQnNDijQDQO8ecLmnGFVxjtflABp5HtvGNtbPwHNjof*)
(*IA==*)
(*"], {{RGBColor[0.5, 0., 0.], Arrowheads[{{0.03317328148120508, 0.8}}], {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["1", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{1, 2}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["5", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{1, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 4}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["8", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{1, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 4}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["7", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{2, 6}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["2", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{3, 2}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["4", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{3, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 5}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["10", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{3, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 5}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["3", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{4, 5}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["6", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{6, 4}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["9", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{6, 5}]}}, {RGBColor[0, 0, 0.7], Tooltip[Point[1], 1], Tooltip[Point[2], 3], Tooltip[Point[3], 6], Tooltip[Point[4], 2], Tooltip[Point[5], 5], Tooltip[Point[6], 4]}}], VertexCoordinateRules -> {{0., 0.9476908358722813}, {1.0575229911820816`, 1.0849247182925033`}, {2.1148282373829, 0.9480148198225868}, {0.4211431764535729, 0.0002534715523621611}, {1.694013530517334, 0.}, {1.0575814894467375`, 0.015059037119393437`}}], AspectRatio -> Automatic, FrameTicks -> None, ImagePadding -> {{0., 1.}, {1., 0.}}, PlotRange -> {{-0.2643535296728625, 2.379181767055763}, {-0.1356155897865629, 1.2205403080790662`}}, PlotRangePadding -> Automatic]*)


(* ::InheritFromParent:: *)
(*Graphics[Annotation[GraphicsComplex[CompressedData["*)
(*1:eJw1jGs0lAkAhj+ydmkTTdGMSyZ0rJxyElJ4v5DbduimoqZFzNYeae1Wi0ms*)
(*W9Z9N0cJiy4bUbGRrZFSxMpq3IcRs4u5mWbIZWVmmm1/9J7znOc8f1562Ol9*)
(*EZoEQXh94H9/XJ5fEJu1XwbWvmPbS6amcXAZ65PhIzOwbaQbbJwiSOlRka3F*)
(*QRl8/L9P0pwbBjFH/zSr2A7S4XTZHZoCH38sT/EbsDSNKleOfZ/3KLYZeFF0*)
(*g2oR77uQ/T5rCj5FNmmx4/W4eEqv6YtOCdItE8q3OT3CHqpGMUNbgopfbGjS*)
(*fU1Y4Sy24e4QIzG0zPgmrRkDVlpibqQIlvKa4mzZMzx++Ce7rFAI8+Qdo8U3*)
(*WhDScokX/UyA1a8HfvJ1eQHNnbymXOEkrgcLWq0E/bhS4HJUnd2Lm1v4ooxr*)
(*A3CorxQxuruxEJXZ7hEwiGSPZ2Xf6HFgar/fjSYdxFMHysuIzE7cvpVk9jqG*)
(*izy/zr8y5e0ICL/nHTLPRU1C3JsTfq1Y2p9bcz50COnr1zbQS5pxptHhjP7j*)
(*D01uynjUzUaI6dC4+elLeMjMVxziSWB+E2YM60JYb1g95iUTg1/qVm0hKcVo*)
(*DstgXlMMfYfUdRuHrmODyddB1VQREocIujsqEBy6Zrh+ixA1S26v5q3uQGdF*)
(*rDLIX4CyqN3M0V212Hwg9TA/chIVXfZvv3Oqw4Vx9q+5ORNIf7KSxa56hZ95*)
(*KmM1j4OnWYGUunIOKiqprWd3d+HpAK2Ofb4bjn07PR/JO5Bnen3uHrMHPRaH*)
(*c4euteG3ShHL+6teLA48vzjCbIFdKiV4K7MP0sDw5y9cmnHhlsjcktUPc91j*)
(*52y/bIRWR7nqSckAvrVZ92+q5wMccKUeYrYQpKb7aHT+awmWLOLyl/4gSP0V*)
(*eo7MaTFo6fK4+CqCrBb6MBa0xDB0Mtg1W0SQwQUFDTnGImSvWtsTkkGQnFlh*)
(*dMxWIbSWH7rx8hxBCsRWakWAAMaLiWPOoQTp8zhOIzRqElNcttfvvgQ53jFi*)
(*L8udwB2ErJL9rQK/Vn1udpiDRbb0ncY9FXTdKFG9vl3oisue8YlRYSLcwD9D*)
(*2gHHCG96n6sKMnjmU0vbEMtpHalXK3FmxjkgI6wFzMtFJRpNSrRrV7nznZph*)
(*tNJa9TJGicV+iq092Yi1zfS7NnZK9HlX6jC2PYCRb2BtZAhByu3vZxrlTsFw*)
(*fY+OiQtB2vwT0WbSJcE044rzj1SCdCpyNdn4mQSdSdpb/WfUyOJTCxNdxdCT*)
(*pcg331fjze7BtqNRIvzAdVvHjlGjZll4IVEkhP/lMUNNdzWY2cf33n0uwGVK*)
(*5Ps1K9V4l3wiulI0iVPlB5JLGUroWhq9PZnVC73jHXvzNikB8A6mvOrGdlez*)
(*9shlSmgLUo74f86B9abiSPcRBUJvaEhvp3XCcLIyzfShAo407/69knboe7yh*)
(*6VxVQN4X73bfsxUBy7lCvQQFwnCSN1vQjKvcwjCnEwrYDe7hnm1h4z+S3R+1*)
(**)
(*"], {{RGBColor[0.5, 0., 0.], Arrowheads[{{0.03317328148120508, 0.8}}], {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["1", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{1, 2}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["5", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{1, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 4}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["8", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{1, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 4}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["7", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{2, 6}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["2", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{3, 2}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["4", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{3, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 5}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["10", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{3, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 5}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["3", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{4, 5}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["6", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{6, 4}]}, {Arrowheads[{{0.5, 0.5, GraphicsBox[{GrayLevel[0], InsetBox[BoxData[FormBox[StyleBox["9", StripOnInput -> False], TraditionalForm]], {0, 0}, ImageScaled[{0.5, 0.5}], Automatic, None, Background -> GrayLevel[1]]}]}, {0.03317328148120508, 0.8}}], Arrow[{6, 5}]}}, {RGBColor[0, 0, 0.7], Tooltip[Point[1], 1], Tooltip[Point[2], 3], Tooltip[Point[3], 6], Tooltip[Point[4], 2], Tooltip[Point[5], 5], Tooltip[Point[6], 4]}}], VertexCoordinateRules -> {{0., 0.9476908358722813}, {1.0575229911820816`, 1.0849247182925033`}, {2.1148282373829, 0.9480148198225868}, {0.4211431764535729, 0.0002534715523621611}, {1.694013530517334, 0.}, {1.0575814894467375`, 0.015059037119393437`}}], AspectRatio -> Automatic, FrameTicks -> None, ImagePadding -> {{0., 1.}, {1., 0.}}, PlotRange -> {{-0.2643535296728625, 2.379181767055763}, {-0.1356155897865629, 1.2205403080790662`}}, PlotRangePadding -> Automatic]*)


testgraph=cont[0][[-5]];
graphToMult[testgraph]
Catenate@Position[%,0]
col=collapsePropagator[testgraph,%]
graphPlot/@{testgraph,col}
Position[Outer[isomorphicQ,slideBubbles[col],basiswfact@nEdges[col],1],True]
graphPlot/@slideBubbles[col]





Table[findVacuumRep[toVacuum[Diag[i]],basiswfact],{i,1,100}]




