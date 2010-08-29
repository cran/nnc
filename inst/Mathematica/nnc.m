Print[Style[StringJoin[
        "Nearest Neighbor Autocovariates. \nJuly 30, 2010"
        ]
  ,FontSize->9,FontFamily->"Times", FontColor->
  RGBColor[0.333293, 0.419599, 0.184301]
   ]];

(* Author: A.I. McLeod *)
(* License: GPL (>= 2))
(* NNC: Nearest Neighbor Autocovariates *)
(* Summary: Computes nearest neighbor autocovariates.*)

BeginPackage["nnc`"];

NNC /: NNC::usage = "NNC[X, y, k] computes the n-by-(Q-1) matrix of \
autocovariates, where Q is the number of classes, n is the number of \
observations. Inputs: X matrix of inputs, y are output classes which may be \
numeric or strings, k is parameter for kNN, the number of nearest neighbors.";
 
EU /: EU::usage = "EU[u,v] computes Euclident distance for x-inputs in \
vectors u and v of the form u, v = (\!\(\*SubscriptBox[\(x\), \
\(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(x\), \(p\)]\),y).";

GetNearest /: GetNearest::usage = "GetNearest[Xy, x0, k] finds the k nearest \
neighbors of x0, a p-vector, in the n-by-(p+1) matrix Xy. Note that if x0 is \
a row in Xy, it is included in the selection.";
 
Getz /: Getz::usage = "Getz[nnk,Q] computes the autocovariates from, nnk, the \
list of all nearest neighbors obtained by `GetNNCnhd`. Q the number of \
classes, 1, 2, ..., Q. There are Q-1 autocovariates. The autocovariates are \
unscaled integer differences in [-k,k].";

(********************************************************************) 

Begin["`Private`"];

(********************************************************************) 

NNC[X_, y_, k_] := Module[{n = Length[y], Q, \[ScriptY], Xy}, 
     classes = Union[y]; Q = Length[classes]; \[ScriptY] = 
       y /. MapThread[#1 -> #2 & , {classes, Range[Q]}]; 
      Xy = Transpose[Join[Transpose[X], {\[ScriptY]}]]; 
      N[Table[Getz[GetNNCnhd[Xy, i, k], Q], {i, n}]/k]];
 
Getz[nnk_, Q_] := Module[{a, ni, nnkAdj}, 
     nnkAdj = Join[nnk, Transpose[{Table[Null, {Q}], Range[Q]}]]; 
      a = GatherBy[nnkAdj, Last]; 
      ni = First /@ Sort[Transpose[MapAt[#1 - 1 & , {Length /@ a, 
            Flatten[(Union[#1] & ) /@ Map[Last, a, {2}]]}, 1]], 
         Last[#1] < Last[#2] & ]; Rest[ni] - First[ni]];
 
GetNNCnhd[Xy_, i_, k_] := Module[{n = Length[Xy], j}, 
     j = Complement[Range[n], {i}]; GetNearest[Xy[[j]], Most[Xy[[i]]], k]]
 
GetNNCnhd /: GetNNCnhd::usage = "GetNNCnhd[Xy,i,k] returns a list of the \
k-nearest neightbors of the point \
Xy\[LeftDoubleBracket]i\[RightDoubleBracket]. The point \
Xy\[LeftDoubleBracket]i\[RightDoubleBracket] is not included in this list.";
 
GetNearest[Xy_, x0_, k_] := Nearest[Xy, Append[x0, 0], k, 
     DistanceFunction -> EU];
 
EU[u_, v_] := EuclideanDistance[Most[u], Most[v]];

End[];
EndPackage[];
 

