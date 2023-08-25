/* Defines instructions for the virtual machine. Instructions are intended to be
 * flat, and very simple, and should not contain any strings. They can contain other
 * simple scalar constants, however. 
 * 
 * The virtual machine uses a stack model for computation. Local variables are 
 * allocated in advance, but most of the actual computation happens with temporaries
 * stored an operation stack. These frames are themselves organized in a call stack,
 * which can be one contigous memory region but can also be seperate objects (i.e.
 * I haven't decided yet).
 * 
 * "The Stack" is virtual, it is actually likely to live on the heap. 
 * 
 * Right now, I am optimizing for instruction simplicity (which hopefully equals
 * easily generated code). This will occur at the cost of runtime speed. */


/* Specify the type of arguments to some operation. */
#[derive(Clone, Copy)]
pub enum IntSize {
    OneByte,
    TwoByte,
    FourByte,
    EightByte,
}

/* Specify the type of arguments to some floating point operation. */
#[derive(Clone, Copy)]
pub enum FloatSize {
    FourByte,
    EightByte,
}

impl Into<u32> for IntSize {
    fn into(self) -> u32 {
        match self {
            IntSize::OneByte => 1,
            IntSize::TwoByte => 2,
            IntSize::FourByte => 4,
            IntSize::EightByte => 8,
        }
    }
}

#[derive(Clone, Copy)]
pub enum Instruction {
    IntegerBinaryOperation (IntegerBinaryOperation, IntSize),  // Both operands must be the same type. Pops two operands, pushes one as the result.
    UnaryOperation (IntegerUnaryOperation, IntSize),  // Pops one operand, pushes one as the result
    AdvanceStackPtr (usize),  // Moves stack pointer up, for alignment purposes
    RetractStackPtr (usize),  // Moves stack pointer down, for alignment purposes. Alternatively, used to ignore a value.
    DebugPrintUnsigned (IntSize),  // Likely to remove. Peaks the top value.
    Duplicate (IntSize),  // Duplicates the top item.
    Exit,
}

#[derive(Clone, Copy)]
pub enum IntegerBinaryOperation {
    UnsignedAddition,
    SignedAddition,
    UnsignedSubtraction,
    SignedSubtraction,
    UnsignedMultiplication,
    SignedMultiplication,
    UnsignedDivision,
    SignedDivision,
}

#[derive(Clone, Copy)]
pub enum IntegerUnaryOperation {
    NegateSigned,
}
