/* Defines instructions for the virtual machine. Instructions are intended to be
 * flat, and very simple, and should not contain any strings. They can contain other
 * simple scalar constants, however. 
 * 
 * The virtual machine uses a stack model for computation. Local variables are 
 * allocated in advance, but most of the actual computation happens with temporaries
 * stored an operation stack. These frames are themselves organized in a call stack,
 * which is stored in one continuous memory region.
 * 
 * Nom's "stack memory" actually lives in Rust's heap.
 * 
 * Right now, I am optimizing for instruction simplicity (which hopefully equals
 * easily generated code). This will occur at the cost of runtime speed. */


/* Specify the type of arguments to some operation. */
#[allow(clippy::enum_variant_names)]
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub enum IntSize {
    OneByte,
    TwoByte,
    FourByte,
    EightByte,
}

impl IntSize {
    pub fn to_usize(self) -> usize {
        match self {
            IntSize::OneByte => 1,
            IntSize::TwoByte => 2,
            IntSize::FourByte => 4,
            IntSize::EightByte => 8,
        }
    }
}

impl TryFrom<usize> for IntSize {
    type Error = crate::error::GenerateError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(IntSize::OneByte),
            2 => Ok(IntSize::TwoByte),
            4 => Ok(IntSize::FourByte),
            8 => Ok(IntSize::EightByte),
            _ => Err(format!("Bad usize to IntSize conversion: {value}").into())
        }
    }
}

/* Specify the type of arguments to some floating point operation. */
#[derive(Clone, Copy)]
pub enum FloatSize {
    #[allow(unused)] // Not yet used..
    FourByte,
    #[allow(unused)] // Not yet used..
    EightByte,
}

#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    // Both operands must be the same type. Pops two operands, pushes one as the result.
    // The result is the same size (and type) as the input.
    IntegerBinaryOperation (IntegerBinaryOperation, IntSize),

    // Both operands must be the same type and size. Consumes two operands, and
    // pushes a single byte, which is the result.
    IntegerComparisonOperation { comparison: Comparison, size: IntSize, signed: bool },

    // Pops one operand, pushes one as the result
    UnaryOperation (IntegerUnaryOperation, IntSize),  

    // Pops one value from the stack, and pushs the result.
    BooleanNot,

    // Moves stack pointer up, for alignment purposes
    AdvanceStackPtr (usize),  

    // Moves stack pointer down, for alignment purposes. Alternatively, used to ignore a value.
    RetractStackPtr (usize),

    // Moves the stack pointer down along with the item directly below the stack pointer.
    // Useful after, for example, a function call places something at alignment 8
    // when it actually has lower alignment, and it needs to be used with items lower
    // on the stack.
    RetractMoving (usize, IntSize),

    // Likely to remove. Peaks the top value.
    DebugPrintSigned (IntSize),  

    // Duplicates the top item.
    Duplicate (IntSize),  

    // Constant is always expressed as an unsigned integer here. Of course, those
    // same bits can represent a signed integer or a float, or some struct etc.
    PushConstant (Constant),

    // isize is a possibly negative offset, in bytes. Result placed on the stack.
    ReadBase (isize, IntSize),  

    // As above, an offset and a arg size. Result is consumed from the stack.
    WriteBase (isize, IntSize), 

    // The instruction index. Precondition: The stack has, at an alignment of 8, 
    // allocated space for the return value, and has evaluated and placed
    // arguments on the top of the stack. The stack currently has an alignment of 8,
    // Postcondition: The stack base pointer points one byte above the final
    // argument (where the stack pointer is at in the precondition). The stack
    // pointer has been advanced 16 bytes, and those 16 bytes hold the return
    // address (an index) and the previous stack base pointer. The instruction
    // pointer jumps to the instruction index.
    Call (usize),  

    // Components are the size and signedness of the input, and the size and signedness of the output.
    // May emit an error.
    IntegerConversion (IntSize, bool, IntSize, bool),

    // Precondition: The base pointer has not moved since a previous call instruction.
    // The function return value has been placed below the function arguments (which
    // are just below the current base pointer).
    // Postcondition: The stack pointer takes the value of the current base pointer.
    // The base pointer takes the saved base pointer. The instruction pointer takes the saved
    // index. Note that the next instructions in the calling function need to lower 
    // the stack pointer below the arguments, to just above the return value.
    Return, 

    // Jumps: Instead of advancing the instruction pointer by one, the instruction pointer
    // is adjusted by the i32 shift, positive or negative.

    // Consumes a bool (1 byte), and jumps if it was true
    RelativeJumpIfTrue (i32),

    // Consumes a bool (1 byte), and jumps if it was false.
    RelativeJumpIfFalse (i32),

    // Unconditional Jump
    RelativeJump (i32),

    // Exit the program
    Exit,
}

#[derive(Clone, Copy, Debug)]
pub enum Comparison {
    Equals,
    NotEquals,
    LessEquals,
    GreaterEquals,
    Less,
    Greater
}

#[derive(Clone, Copy, Debug)]
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

#[derive(Clone, Copy, Debug)]
pub enum IntegerUnaryOperation {
    NegateSigned,
}

#[allow(clippy::enum_variant_names)]
#[derive(Clone, Copy, Debug)]
pub enum Constant {
    OneByte (u8),
    TwoByte (u16),
    FourByte (u32),
    EightByte (u64),
}
