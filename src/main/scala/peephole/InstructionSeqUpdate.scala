package peephole

case class InstructionSeqUpdate[Instruction]
    (fromIndex: Int, toIndex: Int, newInstructions: List[Instruction])
