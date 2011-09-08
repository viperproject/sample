def before(p: ProgramPoint): PartitionedState[D] = {
	(this /: TracePartitioning.get[D](p))((s, d) => s.apply(d))
}
