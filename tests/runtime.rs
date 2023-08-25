
use nom::runtime::Runtime;
use nom::instructions::{Instruction, Constant, IntegerBinaryOperation, IntSize};


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
        Instruction::PushConstant(Constant::FourByte(1234)),
        Instruction::PushConstant(Constant::FourByte(1111)),
        Instruction::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedAddition, IntSize::FourByte),
        Instruction::DebugPrintUnsigned(IntSize::FourByte),
        Instruction::Exit,
    ]);

    assert_eq!(lines, ["2345"]);
}


#[test]
fn u8_math() {
    // 40 - 2 + 3 * 8 / 2 = 26
    let lines = run_collecting_output(vec![
        Instruction::PushConstant(Constant::OneByte(40)),
        Instruction::PushConstant(Constant::OneByte(2)),
        Instruction::PushConstant(Constant::OneByte(3)),
        Instruction::PushConstant(Constant::OneByte(8)),
        Instruction::PushConstant(Constant::OneByte(2)),
        Instruction::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedDivision, IntSize::OneByte),
        Instruction::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedMultiplication, IntSize::OneByte),
        Instruction::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedAddition, IntSize::OneByte),
        Instruction::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedSubtraction, IntSize::OneByte),
        Instruction::DebugPrintUnsigned(IntSize::OneByte),
        Instruction::Exit,
    ]);

    assert_eq!(lines, ["26"]);
}

#[test]
fn u16_math() {
    // 40 - 2 + 3 * 8 / 2 = 26
    let lines = run_collecting_output(vec![
        Instruction::PushConstant(Constant::TwoByte(40)),
        Instruction::PushConstant(Constant::TwoByte(2)),
        Instruction::PushConstant(Constant::TwoByte(3)),
        Instruction::PushConstant(Constant::TwoByte(8)),
        Instruction::PushConstant(Constant::TwoByte(2)),
        Instruction::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedDivision, IntSize::TwoByte),
        Instruction::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedMultiplication, IntSize::TwoByte),
        Instruction::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedAddition, IntSize::TwoByte),
        Instruction::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedSubtraction, IntSize::TwoByte),
        Instruction::DebugPrintUnsigned(IntSize::TwoByte),
        Instruction::Exit,
    ]);

    assert_eq!(lines, ["26"]);
}

#[test]
fn u32_math() {
    // 40 - 2 + 3 * 8 / 2 = 26
    let lines = run_collecting_output(vec![
        Instruction::PushConstant(Constant::FourByte(40)),
        Instruction::PushConstant(Constant::FourByte(2)),
        Instruction::PushConstant(Constant::FourByte(3)),
        Instruction::PushConstant(Constant::FourByte(8)),
        Instruction::PushConstant(Constant::FourByte(2)),
        Instruction::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedDivision, IntSize::FourByte),
        Instruction::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedMultiplication, IntSize::FourByte),
        Instruction::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedAddition, IntSize::FourByte),
        Instruction::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedSubtraction, IntSize::FourByte),
        Instruction::DebugPrintUnsigned(IntSize::FourByte),
        Instruction::Exit,
    ]);

    assert_eq!(lines, ["26"]);
}

#[test]
fn u64_math() {
    // 40 - 2 + 3 * 8 / 2 = 26
    let lines = run_collecting_output(vec![
        Instruction::PushConstant(Constant::EightByte(40)),
        Instruction::PushConstant(Constant::EightByte(2)),
        Instruction::PushConstant(Constant::EightByte(3)),
        Instruction::PushConstant(Constant::EightByte(8)),
        Instruction::PushConstant(Constant::EightByte(2)),
        Instruction::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedDivision, IntSize::EightByte),
        Instruction::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedMultiplication, IntSize::EightByte),
        Instruction::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedAddition, IntSize::EightByte),
        Instruction::IntegerBinaryOperation(IntegerBinaryOperation::UnsignedSubtraction, IntSize::EightByte),
        Instruction::DebugPrintUnsigned(IntSize::EightByte),
        Instruction::Exit,
    ]);

    assert_eq!(lines, ["26"]);
}
