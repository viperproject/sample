override def createObject(t: Type, pp: ProgramPoint): PartitionedState[D] = {
	map(_.createObject(t, pp))
}
