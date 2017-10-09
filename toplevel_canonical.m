(* ::Package:: *)

(* Top level vacuum graphs with ready for canonical labels *)
top[1]={{1,-4,-6},{4,-2,7},{2,-3,8},{3,-1,9},{5,6,10},{-10,-7,11},{-11,-8,12},{-12,-9,-5}};
top[2]={{-2,-4,-6},{4,1,7},{2,-3,8},{3,-1,9},{5,6,10},{-10,-7,11},{-11,-8,12},{-12,-9,-5}};
top[3]={{1,-4,-6},{4,-2,7},{2,-3,8},{3,-1,9},{5,6,10},{-8,-7,11},{-10,-11,12},{-12,-9,-5}};
top[4]={{-2,-4,-6},{4,1,7},{2,-3,8},{3,-1,9},{5,6,10},{-8,-7,11},{-10,-11,12},{-12,-9,-5}};


(* Extra momentum shift needed to reach canonical labels *)
extrashift[1]={l[5]->l[5]-l[1]};
extrashift[2]={l[5]->l[5]-l[1],l[4]->l[4]-l[1]-l[2]};
extrashift[3]={l[5]->l[5]-l[1]};
extrashift[4]={l[5]->l[5]-l[1],l[4]->l[4]-l[1]-l[2]};