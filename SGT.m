(* ::Package:: *)

BeginPackage["EGT`"];


(* ::Subsection:: *)
(*Global defaults, which are overriden lexically by Simulate *)


$actions = {-1, 0, 1};
$temperature = 1;
$mutation = 0;
$mutations = 3;
$payoff = Function[0];
$maskentries = {
4 -> 
{{0, 1, 0},
 {1, 0, 1},
 {0, 1, 0}}, 
6 -> 
{{1, 1, 0},
 {1, 0, 1},
 {0, 1, 1}}, 
8 -> 
{{1, 1, 1},
 {1, 0, 1},
 {1, 1, 1}}
};


mask[n_] := Prepend[Position[n /. $maskentries, 1] - 2, {0,0}];
SetMask[n_] := ($maskn = n; $maskvals = n /. $maskentries; $mask = mask[n]);
SetMask[6];


(* ::Subsection:: *)
(*Utility functions for dealing with spatial grids *)


GridThread[arrays_] := Transpose[arrays, {3,1,2}];

GridMap[function_, array_] := Map[function, array, {2}];
GridMap2[function_, array1_, array2_List] := MapThread[function, {array1, array2}, 2];
GridMapConstant[function_, array1_, arg2_] := 
	If[arg2 === None, 
		Map[function, array1, {2}],
		GridMap[function[#1, arg2]&, array1]];

(* Determine grid of cell-wise neighborhoods *)
$boundary = "Cyclic";
GridNeighbors[cells_] := Block[{$boundary = "Cyclic"}, GridNeighbors[ArrayPad[cells, 1, "Fixed"]][[2;;-2,2;;-2]]] /; ($boundary == "Fixed");
GridNeighbors[cells_] := GridThread[RotateLeft[cells, #]& /@ $mask] /; ($boundary == "Cyclic");


(* ::Subsection:: *)
(*Misc utility functions*)


FunctionToGraph[symbol_] := Select[Function[#[[1,1,1]]->#[[2]]]/@ DownValues[symbol], !MatchQ[#[[1,0]], Blank|Pattern]&];
GraphToFunction[graph_] := With[{dispatch = Dispatch[graph]}, Function[Replace[#, dispatch]]];

MostCommon[list_] := Commonest[list,1][[1]];
MostCommonN[list_, n_] := With[{c = Quiet[Commonest[list,n]]}, PadRight[c,n,c]];

Decimate::usage = "Take n equally spaced samples from a list";
Decimate[list_, l_List] :=
	Part[list, DeleteDuplicates[Clip[l,{1,Length[list]}]]];
Decimate[list_, n_: 10] :=
  With[{len = Length[list]},
   With[{nn = Min[n - 1, len - 1]},
    Part[list, 1 + Floor[(Range[0, nn]/nn)*(len - 1)]]]];
Decimate[{x_}, _] := {x};


(* ::Subsection:: *)
(*Agent update functions*)


ComplexReplace[lhs_, Agent_] := With[
	{agent = AgentTable[Agent]},
	Replace[
		If[MatchQ[agent, _Function | _Symbol], agent[lhs], Replace[lhs, agent]],
		w_Weighted :> RandomChoice[Thread[List @@ w, Rule]]]];

SimpleReplace[lhs_, Agent_] := Replace[lhs, AgentTable[Agent]];


(* ::Subsection:: *)
(*Local functions that agents can be made to use*)


(* Some functions of cell neighborhoods that can be used as inputs to each agent *)
RandomNeighbor[cells_] := GridMap[RandomChoice, GridNeighbors[cells]];
ClipLocalTotal[cells_] := Clip[LocalTotal[cells], {Min[$actions],Max[$actions]}];
LocalTotal[cells_] := ListConvolve[Reverse[Reverse /@ $maskvals], cells, {2,2}];
LocalCount[cells_] := GridMap[Function[local, Count[local, #]& /@ Most[$actions]], GridNeighbors[cells]];
LocalAgentType[cells_] := GridMap[MostCommon, GridNeighbors[cells]];
$local = LocalTotal;


(* ::Subsection:: *)
(*Simulation functions*)


$agenttype = "Simple";

UpdateAgents[cells_, agents_] :=
	Clip[
		GridMap2[ComplexReplace,
		GridThread[{$local[cells], cells}], agents],
		{Min[$actions],Max[$actions]}
	] /; ($agenttype == "Complex");

UpdateAgents[cells_, agents_] :=
	GridMap2[SimpleReplace, GridThread[{$local[cells], cells}], agents] /; ($agenttype == "Simple");

UpdateAgents[cells_, agents_] := agents /; ($agenttype == "Constant");
	
SelectAgents[cells_, agents_, steps_] :=			
	With[{scores = Mean[GridMapConstant[$payoff, GridNeighbors[#], If[$global =!= None, $global[cells], None]]& /@ NestList[UpdateAgents[#, agents]&, cells, steps - 1]]},
		GridMap2[
			ChooseAgent, 
			GridNeighbors[agents], 
			GridNeighbors[Exp[N[scores / $temperature]]]
		]
	];

SelectAgents[cells_, agents_, 1] :=			
	With[{scores = GridMapConstant[
			$payoff, 
			GridNeighbors[cells], 
			If[$global =!= None, $global[cells], None]]},
		GridMap2[
			ChooseAgent, 
			GridNeighbors[agents], 
			GridNeighbors[Exp[N[scores / $temperature]]]
		]
	];

ChooseAgent[agents_, scores_]:=
	With[{me = agents[[1]]}, 
	If[RandomReal[] < $mutation,
		CacheAgent[Nest[MutateAgent, AgentTable[me], $mutations], me],
		RandomChoice[scores -> agents]]];


(* ::Subsection:: *)
(*Hashing, caches and data structures for storing agents and their properties*)


InitTables[] := (
	$hashcounter = 0;
	ParentTable[_] := {};
	AgentTable[agent_] := agent;
	DriftTable[hash_Integer] := {0, 0.8};
	AgeTable[hash_Integer] := 0;
	SequenceTable[hash_Integer] := 0;
);
$timestep = 0;

HashAgent[hash_Integer] := hash;
HashAgent[agent_] := Hash[agent] + $hashcounter++;

CacheAgent[agent_] := 
	With[{hash = HashAgent[agent]},  
		AgentTable[hash] = agent; 
		InverseAgentTable[agent] = hash;
		SequenceTable[hash] = $hashcounter;
		AgeTable[hash] = $timestep;
		hash];

CacheAgentStatic[agent_] := InverseAgentTable[agent] /. InverseAgentTable -> CacheAgent;

CacheAgent[agent_, parent_] := 
	If[agent =!= AgentTable[parent],
		With[{duplicate = InverseAgentTable[agent]},
			If[NumericQ[duplicate], duplicate,
			With[
				{hash1 = CacheAgent[agent], 
				 hash2 = HashAgent[parent]}, 
				DriftTable[hash1] = MapAt[Clip[#,{0.5,1.0}]&, DriftTable[hash2] + RandomChoice[{-0.08, -0.04, 0.04, 0.08}, 2], 2];
				ParentTable[hash2] = AppendTo[ParentTable[hash2], hash1];
				hash1
			]]],
		parent
	];


(* ::Subsection:: *)
(*Building payoff functions*)


Pairwise[rulelist_]:=
	Module[{actions, dispatch, min, table},
		actions = Sort @ Select[DeleteDuplicates[Flatten[rulelist[[All,1]]]], NumericQ];
		dispatch = DeleteDuplicates[
			Append[MapAt[N, #, 2]& /@ rulelist, _ -> 0.], 
			#1[[1]] == #2[[1]]&];
		min = Min[actions]-1;
		table = Table[Replace[{a,b}, dispatch], {a, actions}, {b, actions}];
		With[{MIN = min, TABLE = table}, Compile[{{cells,_Integer,1}},
			With[{me = cells[[1]]},
				Mean @ Rest @ Table[
					TABLE[[me - MIN, you - MIN]],
					{you, cells}]
			]
		]]
	]

Pairwise[rulelist_, "Symmetric"] :=
	Pairwise[Join[rulelist, Function[Reverse[#1] -> #2] @@@ rulelist]];

Pairwise[rulelist_, "Antisymmetric"] :=
	Pairwise[Join[rulelist, Function[Reverse[#1] -> -#2] @@@ rulelist]];

PrisonersDilemma = Pairwise[{
	{-1,-1} -> -2, 
	{-1, 1} ->  4, 
	{ 1, 1} ->  2, 
	{ 1,-1} -> -4, 
	{ 0, _} -> 0.5}];

Ising = Pairwise[{
	{-1,-1} -> -1, 
	{-1, 1} ->  1, 
	{ 1,-1} ->  1, 
	{ 1, 1} -> -1, 
	{ 0, _} -> 0.5}];

Consensus = Function[x, Count[x,x[[1]]]/Length[x]];

RockPaperScissors = 
	Pairwise[{
	{-1, 0} -> 1,
	{ 0, 1} -> 1,
	{ 1,-1} -> 1},
	"Antisymmetric"];


(* ::Subsection:: *)
(*Building agents*)


SetAttributes[MakeAgent, HoldFirst];

makeAllPairs[local_, actions_] :=
	Module[{grids, locals, result},
		grids = Tuples[actions, 9];
		locals = DeleteDuplicates[local[Partition[#, 3]][[2,2]]& /@ grids];
		result = Tuples[{locals, actions}];
		makeAllPairs[local, actions] = result;
		result
	];	
	

MakeAgent[function_] := 
	({##} -> function[##])& @@@ makeAllPairs[$local, $actions];


RandomAgent[] := MakeAgent[RandomChoice[$actions]&];
ConstantAgent[action_] := MakeAgent[action&];
StaticAgent[] := MakeAgent[#2&];


(* ::Subsection:: *)
(*Mutating agents*)


Mutate[Rule[a_, b_]]:= Rule[a, Mutate[b]];
Mutate[RuleDelayed[a_, b_]]:= RuleDelayed[a, b];
Mutate[i_Integer] := RandomChoice[$actions];
Mutate[r_Real] := RandomReal[{0.5, 2}] * r;
Mutate[weights_Weighted] := MapAt[Mutate, weights, {RandomInteger[{1,Length[weights]}],1}];

MutateAgent[rules_List] := MapAt[Mutate, rules, RandomInteger[{1, Length[rules]}]];
MutateAgent[rule_] := MapAt[Mutate, rule, {2}];


(* ::Subsection:: *)
(*Symbolic expressions for creating initial strategy distributions*)


Sectored::usage = "";
Banded::usage = "";
Concentric::usage = "";
Randomized::usage = "";
Uniform::usage = "";

SetAttributes[{Sectored, Banded, Concentric, Randomly, Uniform}, HoldFirst];

makeWorld[___] := Throw[$Failed];

makeWorld[creats_List, size_] := With[{agents = CacheAgentStatic /@ creats}, RandomChoice[agents, size]];

makeWorld[Agents_List, function_Function, size_] :=
	With[
		{length = Length[Agents],
		 agents = CacheAgentStatic /@ Agents,
		max = Max[size]},
		Array[
			Part[
				agents, 
				Mod[Floor[length * function[#1/max,#2/max]], length] + 1
			]&,
			size, -Round[size/2]]];	

makeWorld[Sectored[Agents_], size_] :=
	makeWorld[Agents, Function[0.5 + 1/2 * ArcTan[#1+0.001, #2+0.001] / Pi], size];	

makeWorld[Banded[Agents_, vec_:{1,0}], size_] := 
	makeWorld[Agents, Function[Norm[({#1, #2} + Round[size/2]) . vec]], size];

makeWorld[Concentric[Agents_], size_] :=
	makeWorld[Agents, Function[Clip[Sqrt[3] * Norm[{#1, #2}],{0,0.9999}]], size];

makeWorld[Uniform[agent_], size_] := makeWorld[{agent}, size];

makeWorld[Randomized[Agents_, opts___], size_] := 
	With[{smoothing = "Smoothing" /. {opts} /. "Smoothing" -> 0},
	With[{agents = CacheAgentStatic /@ Agents},
	smooth[RandomChoice[agents,size], smoothing]]];


blurMode[arr_,n_]:=
	With[{pos=Position[DiskMatrix[n],1]-(n+1)}, 
		GridMap[MostCommon,
		GridThread[RotateLeft[arr,#]& /@ pos]]];

smooth[arr_,0|-1] := arr;
smooth[arr_,n_] := smooth[blurMode[arr,n],n-1];


resolveSteps[int_Integer] := int;
resolveSteps[{__Real}] := $steps;
resolveSteps[Span[_, last_, ___]] := last /. All -> $steps;
resolveSteps[steps:{(_Real | _Integer)...}] := Round[Max[steps]];


(* ::Subsection:: *)
(*Simulation options and symbolic expressions*)


Seed::usage="Seed for random number generator";
Size::usage="Size of square grid";
InitialPopulation::usage="Option for specifying strategies and their initial spatial distribution";
MutationRate::usage="Probability of an agent being a mutant";
MutationNumber::usage="Number of mutations that occur to a mutant";
Temperature::usage="Statistical temperature of strategy fixation process";
SelectPeriod::usage="How often to update agent strategies by natural selection";
SelectDuration::usage="What period to average over when determining fitness";
Neighborhood::usage="4, 6, or 8";
Legend::usage="Whether to show a color code legend for agents";
Boundary::usage = "Boundary conditions, can be \"Fixed\" or \"Cyclic\"";
Actions::usage="List of available actions for agents to take";
GlobalFunction::usage = "Optional function applied to entire grid that feeds into payoff function";
LocalFunction::usage = "Optional function applied to cell neighbourhoods that feeds into agent decision making";
Payoff::usage="Payoff function as a function of action neighborhood (and optional global value)";
Weighted::usage = "Weights for probabilistic agent actions, such as Weighted[1 -> 0, 2 -> 1]";
AgentType::usage = "\"Complex\" if agent choices are probabilistic or potentially out of bound, \"Simple\" if not, \"Constant\" if agent is constant-choice";
Options[Simulate]=
{
	Seed -> Automatic,
	Steps -> 1 ;; 50,
	Actions -> Automatic,
	Size -> 30,
	InitialPopulation -> Uniform[0],
	SelectPeriod -> 1,
	SelectDuration -> 1,
	MutationRate -> 0.0,
	MutationNumber -> 2,
	Temperature -> 1/2,
	Neighborhood -> Automatic,
	SingleNeighbor -> False,
	Boundary -> Automatic,
	LocalFunction -> LocalTotal,
	GlobalFunction -> None,
	AgentType -> "Simple",	
	ProgressIndicator -> True,
	Payoff -> Automatic
};


(* ::Subsection:: *)
(*Simulation code*)


$steps = 100;
Simulate[options:OptionsPattern[]] := Module[
	{len = resolveSteps[OptionValue[Steps]],
	 size = OptionValue[Size],
	 seed = OptionValue[Seed] /. Automatic -> RandomInteger[256],
	 period = OptionValue[SelectPeriod],
	 duration = OptionValue[SelectDuration],
	 initial = OptionValue[InitialPopulation],
	 frames, data, t = 1},

	frames = OptionValue[Steps] /. Span -> Range /. All -> $steps;
	frames = frames /. r_Real :> Round[1+len*r];
	frames = Clip[frames, {1, len}];
	frames = DeleteDuplicates[Flatten[{frames}]];

	BlockRandom @ Block[
		{$mutation = OptionValue[MutationRate],
		 $mutations = OptionValue[MutationNumber],
		 $agenttype = OptionValue[AgentType],
		 $temperature = OptionValue[Temperature],
		 $local = OptionValue[LocalFunction],
		 $global = OptionValue[GlobalFunction],
		 $boundary = OptionValue[Boundary] /. Automatic -> $boundary,
		 $maskn = OptionValue[Neighborhood] /. Automatic -> $maskn,
		 $payoff = OptionValue[Payoff] /. Automatic -> $payoff,
		 $actions = OptionValue[Actions] /. Automatic -> $actions,
		 ParentTable, AgentTable, DriftTable, AgeTable, SequenceTable, InverseAgentTable, $timestep = 1},

		SetMask[$maskn];
		SeedRandom[seed];
		InitTables[];

		Module[
			{cells = RandomChoice[$actions, size * {1,1}], 
			 agents = makeWorld[initial, size * {1,1}],
			 time = 1},
	
			If[OptionValue[ProgressIndicator]//TrueQ, 
				PrintTemporary[ProgressIndicator[Dynamic[time], {1,len + 1}]]];

			data = Last @ Reap[ 
				For[$timestep = 1, $timestep <= len + 1, $timestep++; time = $timestep,
			
					(*Print["Frame ", $timestep, " ", Developer`PackedArrayQ[agents], Developer`PackedArrayQ[cells]];*)

					Check[
					If[Mod[$timestep, period] == 0 && $timestep != 1, 
						agents = SelectAgents[cells, agents, duration]];

					cells = UpdateAgents[cells, agents];,

					Throw[$Aborted]];
					If[MemberQ[frames, $timestep], Sow[cells, 1]; Sow[agents, 2];];
				]
			];
		];

		If[data == {}, data = {{}, {}}];

		SimulationData[
			{data[[1]], data[[2]], frames}, 
			FunctionToGraph /@ {AgentTable, ParentTable, AgeTable, SequenceTable, DriftTable},
			{RandomSeed -> seed, SelectPeriod -> period, SelectDuration -> duration, 
			MutationRate -> $mutation, MutationNumber -> $mutations, Temperature -> $temperature, Boundary -> $boundary, 
			Payoff -> $payoff, Actions -> $actions, Neighborhood -> $maskn,
			LocalFunction -> $local, GlobalFunction -> $global}
		]
	]
];


Format[SimulationData[{cells_, agents_, frames_}, _, _], StandardForm] := 
	HoldForm[SimulationData]["<" <> ToString[Length[frames]] <> "/" <> ToString[Max[frames]] <> ">"];


SimulationOptions[SimulationData[_, _, options_]] := options;


(* ::Subsection:: *)
(*Visualization functions *)


ColorRandomly[value_] := With[{hash = Hash[value]}, Hue[Mod[hash, 64] / 64.0, 0.7 + (Mod[hash / 64, 5]/25)]];
PastelHue[hue_Real] := Hue[hue, 0.7];
PastelHue[hue_Integer] := Hue[0.55+hue/5.5, 0.8];

PostprocessGrid[SimulationData[{_, agents_, _}, tables:{agentcache_, ___}, _], function_, normalize_] :=
	Block[{ParentTable, AgentTable, DriftTable, AgeTable, SequenceTable},
		{AgentTable, ParentTable, AgeTable, SequenceTable, DriftTable} = GraphToFunction /@ tables;
		
		With[{rules = (# -> function[#])& /@ agentcache[[All,1]]},
			With[{dispatch = Dispatch[rules],
				norm = If[normalize, N[1 / Max[rules[[All,2]]]] /. Infinity -> 1, 1]},
				Function[x, Replace[x, dispatch] * norm]
			]
		]
	];


backend[data:SimulationData[{cells_, agents_, frames_}, {agentcache_, ___}, _], OptionsPattern[Visualize]] :=
	Module[{
	colrules = OptionValue[ColorRules],
	colfunc = OptionValue[ColorFunction],
	postfunc,
	legend},

	If[colrules =!= None, 
		With[{patterns = colrules[[All,1]]},
			postfunc = PostprocessGrid[data, Function[Position[patterns, AgentTable[#],1][[1,1]]], False];
			colrules[[All,1]] = Range[Length[colrules]]];
	,
		postfunc = PostprocessGrid[data, OptionValue[ColorUsing], OptionValue[ColorNormalize]]
	];

	If[OptionValue[Legend], 
		legend = GenerateLegend[Flatten[agents, 2], agentcache, colfunc, postfunc],
		legend = None
	];
	
	With[{
		LEGEND = legend, 
		POSTFUNC = postfunc, 
		LABEL = OptionValue[Label],
		OPTS = {
			If[colrules =!= None, 
				ColorRules -> colrules, 
				ColorFunction -> colfunc],
			Show -> OptionValue[Show],
			Alignment -> OptionValue[Alignment]}
		},

		Function[{t}, 
			If[t == 0, LEGEND,
			PlotState[
				cells[[t]], GridMap[POSTFUNC, agents[[t]]], 
				Label -> ToString[StringForm[LABEL, frames[[t]]]],
				OPTS
			]]
		]
	]
];


(* ::Subsection:: *)
(*Videos and animations*)


ExportAnimation[file_, data_SimulationData, opts:OptionsPattern[Visualize]] :=
Module[{func, options, render, label, len},

	label = OptionValue[Label];
	func = backend[data, opts];
	len = Length[data[[1]]];

	render = Table[
				frameno = i;
				Pane[func[i], ImageMargins -> 8],
			{i, 1, len, 1}] ~Monitor~ ProgressIndicator[i, {1, len}];
	
	Export[
		file,
		render,
		"FrameRate" -> 15
	]
];


Animation[data_SimulationData, opts:OptionsPattern[Visualize]] :=
Module[{func, options, legend},

	func = backend[data, opts];
	
	Animate[
			Check[func[i], "Error"],
			{i, If[TrueQ @ OptionValue[Legend], 0, 1], Length[data[[1,1]]], 1},
			AnimationRunning -> False,
			AnimationRate -> 15
		]
];


(* ::Subsection:: *)
(*Static galleries of images*)


ColorUsing::usage = "The preprocess function that operates on agent hashes and produces numeric values";
ColorFunction::usage = "The function that colors based on values in the grid";

Options[Visualize] =
{
	Legend -> False,
	Alignment -> Horizontal,
	ColorFunction -> ColorRandomly,
	ColorNormalize -> False,
	ColorRules -> None,
	ColorUsing -> Identity,
	Label -> "t = ``",
	Panels -> 20,
	Show -> All
};


Visualize[data:SimulationData[{Cells_, Agents_, Frames_}, {agentcache_, __}, _], opts:OptionsPattern[]] :=
Module[{sim, func, options, legend},

	sim = MapAt[Decimate[#, OptionValue[Panels]]&, data, {{1,1}, {1,2}, {1,3}}];
	func = backend[sim, opts];
	
	Grid[
		List @ Table[
			func[i], 
			{i, If[TrueQ @ OptionValue[Legend], 0, 1], Length[sim[[1,1]]]}
		]
	]
];


(* ::Subsection:: *)
(*Creating color-code legends for agents*)


displayRulebox[x_Integer] := With[{y = x /. $actioncolors}, If[y =!= x, Graphics[{FaceForm[y], EdgeForm[Black], Rectangle[]}, ImageSize -> {10,10}], x]];
displayRulebox[x_] := x;
displayRule[{a_, b_} -> c_] := {a, displayRulebox[b]} -> displayRulebox[c];
displayRule[lhs_ -> c_] := lhs -> displayRulebox[c];
displayRule[other_] := other;

DisplayAgent[agent_List] := 
	If[Length[DeleteDuplicates[Last /@ agent]] == 1, 
		displayRule[_ -> agent[[1,2]]],
		Grid[Partition[displayRule /@ agent, 4, 4, 1, {}]]];

DisplayAgent[agent_] := agent;

GenerateLegend[agentlist_, agentcache_, colorfunc_, postfunc_] :=
	Module[{legend},
		legend = Reverse @ SortBy[Tally[agentlist], Last];
		legend = legend[[1 ;; Min[Length[legend],20]]];
		legend = Grid[
			Prepend[
				{Item[Tooltip[#[[2]], DisplayAgent[#[[1]] /. agentcache]], 
				 Background -> colorfunc[postfunc[#[[1]]]]]}& /@ legend,
				{Style["Legend", Small]}
			], 
			Frame->{None,None,{{{2,-1},{1,1}}->True}}, 
			ItemSize->Full];
		Style[legend, 12]
	];


(* ::Subsection:: *)
(*Drawing individual simulation frames*)


$actioncolors = {-1 -> Gray, 0 -> White, 1 -> Black} ~Join~  Map[(#1+2) -> Hue[#1/5, 0.7]&, Range[5]];

Options[PlotState] :=
{
	ColorFunction -> ColorRandomly,
	ColorRules -> None,
	Show -> All,
	Label -> None,
	Alignment -> Vertical
};

PlotState[cells_, agents_, OptionsPattern[]]:=
	With[{label = OptionValue[Label], show = OptionValue[Show],
	layoutfunc = OptionValue[Alignment] /. {Horizontal -> GraphicsRow, Vertical -> GraphicsColumn}},
	Replace[
	{
		If[show === All || show == "Choices",
		ArrayPlot[
			cells,
			Frame -> False,
			PixelConstrained -> 5,
			ColorRules -> $actioncolors
		]] ~Replace~ (Null -> Sequence[]),
		If[show === All || show == "Agents",
		ArrayPlot[
			agents,  
			Frame -> False,
			PixelConstrained -> 5, 
			If[OptionValue[ColorRules] =!= None, 
				ColorRules -> OptionValue[ColorRules],
				ColorFunction -> OptionValue[ColorFunction]],
			ColorFunctionScaling -> False
		]] ~Replace~ (Null -> Sequence[])
	}, {{x_} :> x, {x_,y_} :> layoutfunc[{x,y}, Frame -> All, Spacings -> {5,5}]}] // If[label =!= None, Labeled[#, label], #]&
	];



EndPackage[];
