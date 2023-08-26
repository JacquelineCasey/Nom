
use nom::runtime::Runtime;
use nom::instructions::{Instruction, Instruction::*, Constant, IntegerBinaryOperation, IntSize};

fn run_collecting_output(instructions: Vec<Instruction>) -> Vec<String> {
    let mut runtime = Runtime::new(instructions);

    let mut buf = std::io::BufWriter::new(vec![]);
    runtime.run_debug(&mut buf);

    let a = String::from_utf8(buf.into_inner().expect("No IO Error")).expect("Good Conversion");

    a.lines().map(|a| a.to_string()).collect::<Vec<String>>()
}



/* Refer to: https://users.rust-lang.org/t/transmuting-a-generic-array/45645/5
 * Types must be same size, and this is not checked.
 * 
 * See (private) util.rs */
fn reinterpret<In: Copy, Out: Copy>(i: In) -> Out {
    let ptr = std::ptr::addr_of!(i).cast::<Out>();
    unsafe { *ptr }
}



#[test]
fn minimal_test() {
    let lines = run_collecting_output(vec![
        PushConstant(Constant::FourByte(1234)),
        PushConstant(Constant::FourByte(1111)),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedAddition, IntSize::FourByte),
        DebugPrintUnsigned(IntSize::FourByte),
        Exit,
    ]);

    assert_eq!(lines, ["2345"]);
}

#[test]
fn u8_math() {
    // 40 - (2 + 3 * 8 / 2) = 26
    let lines = run_collecting_output(vec![
        PushConstant(Constant::OneByte(40)),
        PushConstant(Constant::OneByte(2)),
        PushConstant(Constant::OneByte(3)),
        PushConstant(Constant::OneByte(8)),
        PushConstant(Constant::OneByte(2)),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedDivision, IntSize::OneByte),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedMultiplication, IntSize::OneByte),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedAddition, IntSize::OneByte),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedSubtraction, IntSize::OneByte),
        DebugPrintUnsigned(IntSize::OneByte),
        Exit,
    ]);

    assert_eq!(lines, ["26"]);
}

#[test]
fn u16_math() {
    // 40 - (2 + 3 * 8 / 2) = 26
    let lines = run_collecting_output(vec![
        PushConstant(Constant::TwoByte(40)),
        PushConstant(Constant::TwoByte(2)),
        PushConstant(Constant::TwoByte(3)),
        PushConstant(Constant::TwoByte(8)),
        PushConstant(Constant::TwoByte(2)),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedDivision, IntSize::TwoByte),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedMultiplication, IntSize::TwoByte),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedAddition, IntSize::TwoByte),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedSubtraction, IntSize::TwoByte),
        DebugPrintUnsigned(IntSize::TwoByte),
        Exit,
    ]);

    assert_eq!(lines, ["26"]);
}

#[test]
fn u32_math() {
    // 40 - (2 + 3 * 8 / 2) = 26
    let lines = run_collecting_output(vec![
        PushConstant(Constant::FourByte(40)),
        PushConstant(Constant::FourByte(2)),
        PushConstant(Constant::FourByte(3)),
        PushConstant(Constant::FourByte(8)),
        PushConstant(Constant::FourByte(2)),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedDivision, IntSize::FourByte),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedMultiplication, IntSize::FourByte),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedAddition, IntSize::FourByte),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedSubtraction, IntSize::FourByte),
        DebugPrintUnsigned(IntSize::FourByte),
        Exit,
    ]);

    assert_eq!(lines, ["26"]);
}

#[test]
fn u64_math() {
    // 40 - (2 + 3 * 8 / 2) = 26
    let lines = run_collecting_output(vec![
        PushConstant(Constant::EightByte(40)),
        PushConstant(Constant::EightByte(2)),
        PushConstant(Constant::EightByte(3)),
        PushConstant(Constant::EightByte(8)),
        PushConstant(Constant::EightByte(2)),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedDivision, IntSize::EightByte),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedMultiplication, IntSize::EightByte),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedAddition, IntSize::EightByte),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedSubtraction, IntSize::EightByte),
        DebugPrintUnsigned(IntSize::EightByte),
        Exit,
    ]);

    assert_eq!(lines, ["26"]);
}

#[test]
fn function_call() {
    // square(1 + 1) * 5, all in u32
    let lines = run_collecting_output(vec![
        AdvanceStackPtr(4), // Instruction 0: Space for Return Value
        PushConstant(Constant::FourByte(1)),
        PushConstant(Constant::FourByte(1)),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedAddition, IntSize::FourByte), // This is the argument 
        Call(10),
        RetractStackPtr(4), // Retract past the argument, to byte after return value
        PushConstant(Constant::FourByte(5)),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedMultiplication, IntSize::FourByte),
        DebugPrintUnsigned(IntSize::FourByte), // Should be 20
        Exit, // Done!
        ReadBase(-4, IntSize::FourByte), // instruction 10: Start of square(a: u32). Grab first argument.
        Duplicate(IntSize::FourByte),
        IntegerBinaryOperation(IntegerBinaryOperation::UnsignedMultiplication, IntSize::FourByte),
        WriteBase(-8, IntSize::FourByte), // Return sets return value.
        Return,
    ]);
    
    assert_eq!(lines, ["20"]);
    
    // A more advanced example. Indentation shows what instructions might be emitted
    // At each node of the AST.

    // square(square(-3)), all in i32
    let lines = run_collecting_output(vec![
        // square(square(-3))
        AdvanceStackPtr(4), // Space for return value
            // square(-3)
            AdvanceStackPtr(4), // Alignment to 8
            AdvanceStackPtr(4), // Space for return value
                // -3
                PushConstant(Constant::FourByte(reinterpret::<i32, u32>(-3))),
            Call(11),
            RetractStackPtr(4), // Skip through argument
            RetractMoving(4, IntSize::FourByte), // Undo alignment
        Call(11),
        RetractStackPtr(4), // Skip through argument

        // Debug print
        DebugPrintUnsigned(IntSize::FourByte), // Should be 81
        Exit, // Done!

        // instruction 11: Start of square(a: u32).
        ReadBase(-4, IntSize::FourByte), 
        Duplicate(IntSize::FourByte),
        IntegerBinaryOperation(IntegerBinaryOperation::SignedMultiplication, IntSize::FourByte),
        WriteBase(-8, IntSize::FourByte), // Return sets return value.
        Return,
    ]);

    assert_eq!(lines, ["81"]);
}
