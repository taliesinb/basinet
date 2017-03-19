BeginPackage["Basinet`"]

BasinetRun::usage = "BasinetRun[f, dir, niter]"
BasinetRunAsync::usage = "BasinetRunAsync[script, dir, nkernels, niter]"
$BasinetKernelID::usage = "$BasinetKernelID represents the index of the kernel the sampler is being run on."

Begin["`Private`"];

Needs["GeneralUtilities`"];

General::invscript = "Script file `` does not exist."
General::invoutdir = "Output directory `` does not exist."
General::invkcount = "Invalid kernel count ``."
General::invrepcount = "Invalid repititions count ``."

BasinetRun[f_, dir_, n_:Infinity] := Scope[
	If[!DirectoryQ[dir], ReturnFailed["invoutdir", dir]];
	$dir = AbsoluteFileName[dir];
	$subdirCache = <||>;
	$sentinel = FileNameJoin[{$dir, "sentinel"}];
	$logSamples = !$Notebooks;
	Do[
		$i = i;
		If[FileExistsQ[$sentinel], Break[]];
		If[FailureQ[doSample[f]], Break[]];
	,
		{i, n}
	];
];

istr[x_] := ToString[x, InputForm];

BasinetRunAsync::synscript = "Script file `` contains invalid syntax.";

$KernelPath = First[$CommandLine];

BasinetRunAsync[script_String, dir_String, kn_:1, n_:Infinity] := Scope[
	If[!FileExistsQ[script], ReturnFailed["invscript", script]];
	If[!DirectoryQ[dir], ReturnFailed["invoutdir", dir]];
	If[!IntegerQ[kn], ReturnFailed["invkcount", kn]];
	If[!IntegerQ[n], ReturnFailed["invrepcount", n]];
	script = AbsoluteFileName @ script;
	dir = AbsoluteFileName @ dir;
	fstr = FileString[script];
	If[Block[{$Context = "Dummy`"}, !SyntaxQ[fstr]], ReturnFailed["synscript", script]];
	script = istr[script];	
	dir = istr[dir];
	n = IntegerString[n];
	Table[
		codestring = StringJoin[
			$GetMeString,
			"Basinet`Private`BasinetRunInternal[", IntegerString[id], ", ", script, ", ", dir, ", ", n, "];"
		];
		temp = CreateTemporary[];
		WriteString[temp, codestring];
		Close[temp];
		StartProcess[{$KernelPath, "-script", temp}]
	,
		{id, kn}
	]
];

$BasinetKernelID = 1;
$GetMeString = "Get[" <> istr[$InputFileName] <> "];\n";

BasinetRunInternal[id_, script_, dir_, n_] := Block[
	{func, seed},
	seed = Hash[{$ProcessID, AbsoluteTime[]}];
	SeedRandom[seed];
	$BasinetKernelID = id;
	$HistoryLength = 0;
	logdir = EnsureDirectory[{dir, "_log"}];
	log = OpenWrite[
		FileNameJoin[{logdir, IntegerString[$ProcessID]}],
		BinaryFormat -> True, FormatType -> OutputForm
	];
	$Output = {log};
	$Messages = {log};
	SetOptions[$Output, FormatType -> OutputForm];
	Print["Kernel #", id, " (process ", $ProcessID, ") is using master seed ", seed, "."];
	Print["Getting script \"", script, "\"."];
	func = Get[script];
	Print["Sampling..."];
	BasinetRun[func, dir, n];
	Print["Done."];
	Close[log];
];


BasinetRun::badout = "Function did not produce an association, aborting run. Seed was ``."

$logSamples = False;

doSample[f_] := Scope[
	seed = RandomInteger[16^8];
	BlockRandom[
		seedStr = IntegerString[seed, 16, 8];
		SeedRandom[seed];
		If[$logSamples, Print["Running sample #", $i, " using seed \"", seedStr, "\"."]];
		{time, tmp} = AbsoluteTiming[res = f[];];
		If[$logSamples, Print["Sample took ", TextString @ time, " seconds."]];
	];
	If[!AssociationQ[res], 
		Message[BasinetRun::badout, seedStr];
		ReturnFailed[];
	];
	dirs = CacheTo[$subdirCache, #, EnsureDirectory[{$dir, #}]]& /@ Keys[res];
	MapThread[writeOut, {FileNameJoin[{#, seedStr}]& /@ dirs, Values[res]}];
	seedStr
];

(* 'small' data just gets written in plaintext *)
writeOut[f_, v_Real | v_Rational | v_Integer | v_String] := Put[v, f <> ".m"];

(* this mimics what Export[".wlnet", ...] does, but faster *)
writeOut[f_, v_ ? NeuralNetworks`ValidNetQ] := NeuralNetworks`WLNetExport[f <> ".wlnet", v];

(* this mimics what Export[".mx", ...] does, but faster *)
writeOut[f_, v_] := (
	System`Private`ConvertersPrivateDumpSymbol = Compose[HoldComplete, v];
	DumpSave[
		f <> ".mx", 
		System`Private`ConvertersPrivateDumpSymbol, 
		HoldAllComplete
	];
	Clear[System`Private`ConvertersPrivateDumpSymbol];
);

End[]
EndPackage[]
