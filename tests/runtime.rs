
use nom::runtime::Runtime;
use nom::instructions::{Instruction, Instruction::*, Constant, IntegerBinaryOperation, IntSize};


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
