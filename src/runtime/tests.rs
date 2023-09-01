
use super::Runtime;

use crate::instructions::{Instruction, Constant, IntegerBinaryOperation, IntSize};
use crate::util::reinterpret;


use Instruction as I;

fn run_collecting_output(instructions: Vec<Instruction>) -> Vec<String> {
    let mut runtime = Runtime::new(instructions);

    let mut buf = std::io::BufWriter::new(vec![]);
    runtime.run_debug(&mut buf);

    let a = String::from_utf8(buf.into_inner().expect("No IO Error")).expect("Good Conversion");

    a.lines().map(|a| a.to_string()).collect::<Vec<String>>()
}


#[test]
fn minimal_test() {
    let lines = run_collecting_output(vec![
        I::PushConstant(Constant::FourByte(1234)),
        I::PushConstant(Constant::FourByte(1111)),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedAddition, IntSize::FourByte),
        I::DebugPrintSigned(IntSize::FourByte),
        I::Exit,
    ]);

    assert_eq!(lines, ["2345"]);
}

#[test]
fn u8_math() {
    // 40 - (2 + 3 * 8 / 2) = 26
    let lines = run_collecting_output(vec![
        I::PushConstant(Constant::OneByte(40)),
        I::PushConstant(Constant::OneByte(2)),
        I::PushConstant(Constant::OneByte(3)),
        I::PushConstant(Constant::OneByte(8)),
        I::PushConstant(Constant::OneByte(2)),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedDivision, IntSize::OneByte),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedMultiplication, IntSize::OneByte),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedAddition, IntSize::OneByte),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedSubtraction, IntSize::OneByte),
        I::DebugPrintSigned(IntSize::OneByte),
        I::Exit,
    ]);

    assert_eq!(lines, ["26"]);
}

#[test]
fn u16_math() {
    // 40 - (2 + 3 * 8 / 2) = 26
    let lines = run_collecting_output(vec![
        I::PushConstant(Constant::TwoByte(40)),
        I::PushConstant(Constant::TwoByte(2)),
        I::PushConstant(Constant::TwoByte(3)),
        I::PushConstant(Constant::TwoByte(8)),
        I::PushConstant(Constant::TwoByte(2)),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedDivision, IntSize::TwoByte),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedMultiplication, IntSize::TwoByte),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedAddition, IntSize::TwoByte),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedSubtraction, IntSize::TwoByte),
        I::DebugPrintSigned(IntSize::TwoByte),
        I::Exit,
    ]);

    assert_eq!(lines, ["26"]);
}

#[test]
fn u32_math() {
    // 40 - (2 + 3 * 8 / 2) = 26
    let lines = run_collecting_output(vec![
        I::PushConstant(Constant::FourByte(40)),
        I::PushConstant(Constant::FourByte(2)),
        I::PushConstant(Constant::FourByte(3)),
        I::PushConstant(Constant::FourByte(8)),
        I::PushConstant(Constant::FourByte(2)),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedDivision, IntSize::FourByte),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedMultiplication, IntSize::FourByte),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedAddition, IntSize::FourByte),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedSubtraction, IntSize::FourByte),
        I::DebugPrintSigned(IntSize::FourByte),
        I::Exit,
    ]);

    assert_eq!(lines, ["26"]);
}

#[test]
fn u64_math() {
    // 40 - (2 + 3 * 8 / 2) = 26
    let lines = run_collecting_output(vec![
        I::PushConstant(Constant::EightByte(40)),
        I::PushConstant(Constant::EightByte(2)),
        I::PushConstant(Constant::EightByte(3)),
        I::PushConstant(Constant::EightByte(8)),
        I::PushConstant(Constant::EightByte(2)),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedDivision, IntSize::EightByte),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedMultiplication, IntSize::EightByte),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedAddition, IntSize::EightByte),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedSubtraction, IntSize::EightByte),
        I::DebugPrintSigned(IntSize::EightByte),
        I::Exit,
    ]);

    assert_eq!(lines, ["26"]);
}

#[test]
fn function_call() {
    // square(1 + 1) * 5, all in u32
    let lines = run_collecting_output(vec![
        I::AdvanceStackPtr(4), // Instruction 0: Space for Return Value
        I::PushConstant(Constant::FourByte(1)),
        I::PushConstant(Constant::FourByte(1)),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedAddition, IntSize::FourByte), // This is the argument 
        I::Call(10),
        I::RetractStackPtr(4), // Retract past the argument, to byte after return value
        I::PushConstant(Constant::FourByte(5)),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedMultiplication, IntSize::FourByte),
        I::DebugPrintSigned(IntSize::FourByte), // Should be 20
        I::Exit, // Done!
        I::ReadBase(-4, IntSize::FourByte), // instruction 10: Start of square(a: u32). Grab first argument.
        I::Duplicate(IntSize::FourByte),
        I::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedMultiplication, IntSize::FourByte),
        I::WriteBase(-8, IntSize::FourByte), // Return sets return value.
        I::Return,
    ]);
    
    assert_eq!(lines, ["20"]);
    
    // A more advanced example. Indentation shows what instructions might be emitted
    // At each node of the AST.

    // square(square(-3)), all in i32
    let lines = run_collecting_output(vec![
        // square(square(-3))
        I::AdvanceStackPtr(4), // Space for return value
            // square(-3)
            I::AdvanceStackPtr(4), // Alignment to 8
            I::AdvanceStackPtr(4), // Space for return value
                // -3
                I::PushConstant(Constant::FourByte(reinterpret::<i32, u32>(-3))),
                I::Call(11),
                I::RetractStackPtr(4), // Skip through argument
                I::RetractMoving(4, IntSize::FourByte), // Undo alignment
                I::Call(11),
                I::RetractStackPtr(4), // Skip through argument

        // Debug print
        I::DebugPrintSigned(IntSize::FourByte), // Should be 81
        I::Exit, // Done!

        // instruction 11: Start of square(a: u32).
        I::ReadBase(-4, IntSize::FourByte), 
        I::Duplicate(IntSize::FourByte),
        I::IntegerBinaryOperation(IntegerBinaryOperation::SignedMultiplication, IntSize::FourByte),
        I::WriteBase(-8, IntSize::FourByte), // Return sets return value.
        I::Return,
    ]);

    assert_eq!(lines, ["81"]);
}
