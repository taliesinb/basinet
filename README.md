# basinet

Basinet provides a super-simple framework for Monte Carlo sampling in Mathematica, on a single machine. It's really for sampling neural net training runs given a fixed net topology and dataset, where only the initial weights vary (the clue is in the name). 

* Ideal sample computation time is a few seconds upwards.
* Multiple 'columns' can be stored from each sample, the sample function returns an association `<|"x" -> value, "y" -> value, ...|>`.
* Results are stored on disk, one file per column per sample, using a value-dependent format.
* A unique random seed is used for each sample to make it reproducible, and the seed is used as the file name under which the sample's results are stored.
* With `BasinetRunScriptAsync`, multiple slave kernels will be used, `$BasinetKernelID` indexes them, so `NetTrain[..., TargetDevice -> {"GPU", $BasinetKernelID}]` is the obvious thing when you have a multi-GPU machine. 
* Outputs of all kernels are logged to a separate "_logs" directory.
* Sentinel file to gracefully stop early.
* It's unlikely, but partial results could be written in the event of a crash. Might revisit this in future if it ever happens, but dealing with it manually is pretty trivial.