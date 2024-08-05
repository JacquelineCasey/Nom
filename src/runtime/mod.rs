#[cfg(test)]
mod tests;

use std::alloc::{alloc, dealloc, Layout};

use crate::instructions::{
    Comparison, Constant, Instruction, IntSize, IntegerBinaryOperation, IntegerUnaryOperation,
};
use crate::util::reinterpret;

const STACK_SIZE: usize = 1_048_576; // In terms of u8 units. This is exactly a megabyte.

pub struct Runtime {
    instructions: Vec<Instruction>,
    instruction_index: usize, // Really just an index
    stack_pointer: *mut u8,   // Current location of the top of the stack, i.e. no value lives here.
    base_pointer: *mut u8, // Current location of bottom of the frame. Locals are available, as well as return value and previous frame pointer.
    stack_bottom: *const u8,
    stack_layout: Layout,
    running: bool,
}

impl Runtime {
    pub fn new(instructions: Vec<Instruction>) -> Runtime {
        let stack_layout =
            Layout::array::<u64>(STACK_SIZE / 8).expect("Memory should be allocated");
        let stack = unsafe { alloc(stack_layout) };

        // See Drop implementation

        Runtime {
            instructions,
            instruction_index: 0,
            stack_pointer: stack,
            stack_bottom: stack,
            base_pointer: stack,
            stack_layout,
            running: false,
        }
    }

    pub fn run(&mut self) {
        self.run_impl(None);
    }

    pub fn run_debug(&mut self, debug_out: &mut dyn std::io::Write) {
        self.run_impl(Some(debug_out));
    }

    fn run_impl(&mut self, mut debug_out: Option<&mut dyn std::io::Write>) {
        self.running = true;

        while self.running {
            let instruction = self.instructions[self.instruction_index];

            self.instruction_index += 1; // Might be overriden by running a jump

            self.eval_instruction(instruction, &mut debug_out);
        }
    }

    #[allow(clippy::too_many_lines)]
    fn eval_instruction(
        &mut self,
        instruction: Instruction,
        debug_out: &mut Option<&mut dyn std::io::Write>,
    ) {
        match instruction {
            Instruction::IntegerBinaryOperation(op, size) => {
                self.eval_binary_int_op(op, size);
            }
            Instruction::UnaryOperation(op, size) => {
                self.eval_unary_int_op(op, size);
            }
            Instruction::IntegerComparisonOperation { comparison, size, signed } => {
                self.eval_int_comparison(comparison, size, signed);
            }
            Instruction::BooleanNot => {
                let arg = u8::pop(self);

                if arg != 0 {
                    u8::push(0, self);
                } else {
                    u8::push(1, self);
                }
            }
            Instruction::AdvanceStackPtr(amount) => {
                self.stack_pointer = unsafe { self.stack_pointer.add(amount) };
            }
            Instruction::RetractStackPtr(amount) => {
                self.stack_pointer = unsafe { self.stack_pointer.sub(amount) };
            }
            Instruction::RetractMoving(amount, size) => match size {
                IntSize::OneByte => {
                    let val = u8::pop(self);
                    self.stack_pointer = unsafe { self.stack_pointer.sub(amount) };
                    u8::push(val, self);
                }
                IntSize::TwoByte => {
                    let val = u16::pop(self);
                    self.stack_pointer = unsafe { self.stack_pointer.sub(amount) };
                    u16::push(val, self);
                }
                IntSize::FourByte => {
                    let val = u32::pop(self);
                    self.stack_pointer = unsafe { self.stack_pointer.sub(amount) };
                    u32::push(val, self);
                }
                IntSize::EightByte => {
                    let val = u64::pop(self);
                    self.stack_pointer = unsafe { self.stack_pointer.sub(amount) };
                    u64::push(val, self);
                }
            },
            Instruction::DebugPrintSigned(size) => {
                if let Some(out) = debug_out {
                    self.eval_instruction(Instruction::Duplicate(size), &mut None);

                    match size {
                        IntSize::OneByte => {
                            let val = reinterpret::<u8, i8>(u8::pop(self));
                            writeln!(out, "{val}").expect("prints");
                        }
                        IntSize::TwoByte => {
                            let val = reinterpret::<u16, i16>(u16::pop(self));
                            writeln!(out, "{val}").expect("prints");
                        }
                        IntSize::FourByte => {
                            let val = reinterpret::<u32, i32>(u32::pop(self));
                            writeln!(out, "{val}").expect("prints");
                        }
                        IntSize::EightByte => {
                            let val = reinterpret::<u64, i64>(u64::pop(self));
                            writeln!(out, "{val}").expect("prints");
                        }
                    }
                }
            }
            Instruction::Duplicate(size) => match size {
                IntSize::OneByte => self.duplicate::<u8>(),
                IntSize::TwoByte => self.duplicate::<u16>(),
                IntSize::FourByte => self.duplicate::<u32>(),
                IntSize::EightByte => self.duplicate::<u64>(),
            },
            Instruction::PushConstant(constant) => match constant {
                Constant::OneByte(val) => u8::push(val, self),
                Constant::TwoByte(val) => u16::push(val, self),
                Constant::FourByte(val) => u32::push(val, self),
                Constant::EightByte(val) => u64::push(val, self),
            },
            Instruction::Exit => {
                self.running = false;
            }
            Instruction::ReadBase(offset, size) => match size {
                IntSize::OneByte => {
                    let val = self.read_base::<u8>(offset);
                    u8::push(val, self);
                }
                IntSize::TwoByte => {
                    let val = self.read_base::<u16>(offset);
                    u16::push(val, self);
                }
                IntSize::FourByte => {
                    let val = self.read_base::<u32>(offset);
                    u32::push(val, self);
                }
                IntSize::EightByte => {
                    let val = self.read_base::<u64>(offset);
                    u64::push(val, self);
                }
            },
            Instruction::WriteBase(offset, size) => match size {
                IntSize::OneByte => {
                    let val = u8::pop(self);
                    self.write_base::<u8>(offset, val);
                }
                IntSize::TwoByte => {
                    let val = u16::pop(self);
                    self.write_base::<u16>(offset, val);
                }
                IntSize::FourByte => {
                    let val = u32::pop(self);
                    self.write_base::<u32>(offset, val);
                }
                IntSize::EightByte => {
                    let val = u64::pop(self);
                    self.write_base::<u64>(offset, val);
                }
            },
            Instruction::Call(index) => {
                let prev_base = self.base_pointer;
                self.base_pointer = self.stack_pointer;

                // Alignment, bounds checked in these functions.
                u64::push(self.instruction_index as u64, self); // index is already 1 past the call instruction
                u64::push(prev_base as u64, self);

                self.instruction_index = index;
            }
            Instruction::Return => {
                // The stack_pointer should maybe already be at this position.
                self.stack_pointer = unsafe { self.base_pointer.offset(16) };

                self.base_pointer = u64::pop(self) as *mut u8;
                self.instruction_index = u64::pop(self) as usize;
            }
            Instruction::IntegerConversion(start_size, start_sign, end_size, end_sign) => {
                self.convert_integer(start_size, start_sign, end_size, end_sign);
            }
            Instruction::RelativeJump(i) => {
                self.instruction_index -= 1; // Ignore normal instruction pointer movement
                self.instruction_index = (self.instruction_index as i32 + i) as usize;
            }
            Instruction::RelativeJumpIfTrue(i) => {
                let val = u8::pop(self);
                if val != 0 {
                    self.instruction_index -= 1; // Ignore normal instruction pointer movement
                    self.instruction_index = (self.instruction_index as i32 + i) as usize;
                }
            }
            Instruction::RelativeJumpIfFalse(i) => {
                let val = u8::pop(self);
                if val == 0 {
                    self.instruction_index -= 1; // Ignore normal instruction pointer movement
                    self.instruction_index = (self.instruction_index as i32 + i) as usize;
                }
            }
            // I'd like to clean this up someday...
            Instruction::WriteStack(src, dest, int_size) => match int_size {
                IntSize::OneByte => unsafe {
                    let val = self.stack_pointer.offset(src).read();
                    self.stack_pointer.offset(dest).write(val);
                },
                IntSize::TwoByte => unsafe {
                    let val = self.stack_pointer.offset(src).cast::<u16>().read();
                    self.stack_pointer.offset(dest).cast::<u16>().write(val);
                },
                IntSize::FourByte => unsafe {
                    let val = self.stack_pointer.offset(src).cast::<u32>().read();
                    self.stack_pointer.offset(dest).cast::<u32>().write(val);
                },
                IntSize::EightByte => unsafe {
                    let val = self.stack_pointer.offset(src).cast::<u64>().read();
                    self.stack_pointer.offset(dest).cast::<u64>().write(val);
                },
            },
        }
    }

    fn convert_integer(
        &mut self,
        start_size: IntSize,
        start_sign: bool,
        end_size: IntSize,
        end_sign: bool,
    ) {
        match (start_size, start_sign) {
            (IntSize::OneByte, true) => self.convert_integer_impl_1::<i8>(end_size, end_sign),
            (IntSize::OneByte, false) => self.convert_integer_impl_1::<u8>(end_size, end_sign),
            (IntSize::TwoByte, true) => self.convert_integer_impl_1::<i16>(end_size, end_sign),
            (IntSize::TwoByte, false) => self.convert_integer_impl_1::<u16>(end_size, end_sign),
            (IntSize::FourByte, true) => self.convert_integer_impl_1::<i32>(end_size, end_sign),
            (IntSize::FourByte, false) => self.convert_integer_impl_1::<u32>(end_size, end_sign),
            (IntSize::EightByte, true) => self.convert_integer_impl_1::<i64>(end_size, end_sign),
            (IntSize::EightByte, false) => self.convert_integer_impl_1::<u64>(end_size, end_sign),
        }
    }

    fn convert_integer_impl_1<In: RuntimeInt>(&mut self, end_size: IntSize, end_sign: bool) {
        match (end_size, end_sign) {
            (IntSize::OneByte, true) => self.convert_integer_impl_2::<In, i8>(),
            (IntSize::OneByte, false) => self.convert_integer_impl_2::<In, u8>(),
            (IntSize::TwoByte, true) => self.convert_integer_impl_2::<In, i16>(),
            (IntSize::TwoByte, false) => self.convert_integer_impl_2::<In, u16>(),
            (IntSize::FourByte, true) => self.convert_integer_impl_2::<In, i32>(),
            (IntSize::FourByte, false) => self.convert_integer_impl_2::<In, u32>(),
            (IntSize::EightByte, true) => self.convert_integer_impl_2::<In, i64>(),
            (IntSize::EightByte, false) => self.convert_integer_impl_2::<In, u64>(),
        }
    }

    fn convert_integer_impl_2<In: RuntimeInt, Out: RuntimeInt>(&mut self) {
        let a = In::pop(self);
        Out::push(Out::from_i128(a.as_i128()), self);
    }

    // Unlike the Instruction, this does nothing to the stack
    fn read_base<S: Stackable>(&mut self, offset: isize) -> S {
        unsafe {
            let ptr = self.base_pointer.offset(offset);

            // TODO: Alignment check, bounds check

            ptr.cast::<S>().read()
        }
    }

    // Unlike the Instruction, this does nothing to the stack
    fn write_base<S: Stackable>(&mut self, offset: isize, val: S) {
        unsafe {
            let ptr = self.base_pointer.offset(offset);

            // TODO: Alignment check, bounds check

            ptr.cast::<S>().write(val);
        }
    }

    fn eval_binary_int_op(&mut self, op: IntegerBinaryOperation, size: IntSize) {
        match size {
            IntSize::OneByte => self.eval_binary_int_op_impl::<u8, i8>(op),
            IntSize::TwoByte => self.eval_binary_int_op_impl::<u16, i16>(op),
            IntSize::FourByte => self.eval_binary_int_op_impl::<u32, i32>(op),
            IntSize::EightByte => self.eval_binary_int_op_impl::<u64, i64>(op),
        }
    }

    fn eval_unary_int_op(&mut self, op: IntegerUnaryOperation, size: IntSize) {
        match size {
            IntSize::OneByte => self.eval_unary_int_op_impl::<u8, i8>(op),
            IntSize::TwoByte => self.eval_unary_int_op_impl::<u16, i16>(op),
            IntSize::FourByte => self.eval_unary_int_op_impl::<u32, i32>(op),
            IntSize::EightByte => self.eval_unary_int_op_impl::<u64, i64>(op),
        }
    }

    fn duplicate<T: Stackable>(&mut self) {
        let val = T::pop(self);
        T::push(val, self);
        T::push(val, self);
    }

    fn eval_binary_int_op_impl<U: RuntimeInt, S: RuntimeInt>(
        &mut self,
        op: IntegerBinaryOperation,
    ) {
        let right = U::pop(self);
        let left = U::pop(self);

        let result = match op {
            IntegerBinaryOperation::UnsignedAddition => left + right,
            IntegerBinaryOperation::SignedAddition => {
                reinterpret::<S, U>(reinterpret::<U, S>(left) + reinterpret::<U, S>(right))
            }
            IntegerBinaryOperation::UnsignedSubtraction => left - right,
            IntegerBinaryOperation::SignedSubtraction => {
                reinterpret::<S, U>(reinterpret::<U, S>(left) - reinterpret::<U, S>(right))
            }
            IntegerBinaryOperation::UnsignedMultiplication => left * right,
            IntegerBinaryOperation::SignedMultiplication => {
                reinterpret::<S, U>(reinterpret::<U, S>(left) * reinterpret::<U, S>(right))
            }
            IntegerBinaryOperation::UnsignedDivision => {
                assert!(right != U::zero(), "Critical Runtime Error: Division by Zero");

                left / right
            }
            IntegerBinaryOperation::SignedDivision => {
                let s_left = reinterpret::<U, S>(left);
                let s_right = reinterpret::<U, S>(right);

                assert!(s_right != S::zero(), "Critical Runtime Error: Division by Zero");

                reinterpret::<S, U>(s_left / s_right)
            }
            IntegerBinaryOperation::UnsignedModulus => {
                assert!(right != U::zero(), "Critical Runtime Error: Modulus by Zero");

                left % right
            }
            IntegerBinaryOperation::SignedModulus => {
                let s_left = reinterpret::<U, S>(left);
                let s_right = reinterpret::<U, S>(right);

                assert!(s_right != S::zero(), "Critical Runtime Error: Modulus by Zero");

                reinterpret::<S, U>(s_left % s_right)
            }
        };

        U::push(result, self);
    }

    fn eval_unary_int_op_impl<U: RuntimeInt, S: Signed>(&mut self, op: IntegerUnaryOperation) {
        let val = U::pop(self);

        let result = match op {
            IntegerUnaryOperation::NegateSigned => reinterpret::<S, U>(-reinterpret::<U, S>(val)),
        };

        U::push(result, self);
    }

    fn eval_int_comparison(
        &mut self,
        comparison: crate::instructions::Comparison,
        size: IntSize,
        signed: bool,
    ) {
        match (signed, size) {
            (true, IntSize::OneByte) => self.eval_int_comparison_impl::<u8>(comparison),
            (true, IntSize::TwoByte) => self.eval_int_comparison_impl::<u16>(comparison),
            (true, IntSize::FourByte) => self.eval_int_comparison_impl::<u32>(comparison),
            (true, IntSize::EightByte) => self.eval_int_comparison_impl::<u64>(comparison),
            (false, IntSize::OneByte) => self.eval_int_comparison_impl::<i8>(comparison),
            (false, IntSize::TwoByte) => self.eval_int_comparison_impl::<i16>(comparison),
            (false, IntSize::FourByte) => self.eval_int_comparison_impl::<i32>(comparison),
            (false, IntSize::EightByte) => self.eval_int_comparison_impl::<i64>(comparison),
        }
    }

    fn eval_int_comparison_impl<R: RuntimeInt>(&mut self, comparison: Comparison) {
        let right = R::pop(self);
        let left = R::pop(self);

        let result = match comparison {
            Comparison::Equals => left == right,
            Comparison::NotEquals => left != right,
            Comparison::LessEquals => left <= right,
            Comparison::GreaterEquals => left >= right,
            Comparison::Less => left < right,
            Comparison::Greater => left > right,
        };

        if result {
            u8::push(1, self);
        } else {
            u8::push(0, self);
        }
    }
}

impl Drop for Runtime {
    fn drop(&mut self) {
        unsafe { dealloc(self.stack_bottom.cast_mut(), self.stack_layout) }
    }
}

/* Type shenanigans */

/* Marker trait representing something we might push and pull from the stack. */
trait Stackable: Copy {
    /* These operations assume but also check alignment. If alignment is wrong, they panic. */
    fn push(val: Self, runtime: &mut Runtime);
    fn pop(runtime: &mut Runtime) -> Self;
}

/* We always think about items on the stack as unsized integers, even if in reality
 * they are floats, or booleans, or pointers etc. */
impl Stackable for u8 {
    #[allow(clippy::cast_ptr_alignment, clippy::int_plus_one)]
    fn push(val: Self, runtime: &mut Runtime) {
        // Remember to use pointer::offset() to actually get the next pointer.
        // pointer::offset is UB if it goes outside of the allocation though, hence
        // the checks above being done in usize.

        assert!(
            runtime.stack_pointer as usize + 1 <= runtime.stack_bottom as usize + STACK_SIZE,
            "Critical Runtime Error: Stack Overflow"
        );

        // Skip alignment check.

        unsafe {
            runtime.stack_pointer.write(val);
            runtime.stack_pointer = runtime.stack_pointer.add(1);
        }
    }

    #[allow(clippy::int_plus_one)]
    fn pop(runtime: &mut Runtime) -> Self {
        assert!(
            runtime.stack_pointer as usize - 1 >= runtime.stack_bottom as usize,
            "Consumed whole stack!"
        );

        // Skip alignment check

        unsafe {
            runtime.stack_pointer = runtime.stack_pointer.sub(1);
            runtime.stack_pointer.read()
        }
    }
}

impl Stackable for u16 {
    #[allow(clippy::cast_ptr_alignment)]
    fn push(val: Self, runtime: &mut Runtime) {
        assert!(
            runtime.stack_pointer as usize + 2 <= runtime.stack_bottom as usize + STACK_SIZE,
            "Critical Runtime Error: Stack Overflow"
        );

        assert!(
            runtime.stack_pointer as usize % 2 == 0,
            "[Instruction {}] Stack pointer misaligned",
            runtime.instruction_index - 1
        );

        unsafe {
            runtime.stack_pointer.cast::<u16>().write(val);
            runtime.stack_pointer = runtime.stack_pointer.add(2);
        }
    }

    #[allow(clippy::cast_ptr_alignment)]
    fn pop(runtime: &mut Runtime) -> Self {
        assert!(
            runtime.stack_pointer as usize - 2 >= runtime.stack_bottom as usize,
            "Consumed whole stack!"
        );

        assert!(
            runtime.stack_pointer as usize % 2 == 0,
            "[Instruction {}] Stack pointer misaligned",
            runtime.instruction_index - 1
        );

        unsafe {
            runtime.stack_pointer = runtime.stack_pointer.sub(2);
            runtime.stack_pointer.cast::<u16>().read()
        }
    }
}

impl Stackable for u32 {
    #[allow(clippy::cast_ptr_alignment)]
    fn push(val: Self, runtime: &mut Runtime) {
        assert!(
            runtime.stack_pointer as usize + 4 <= runtime.stack_bottom as usize + STACK_SIZE,
            "Critical Runtime Error: Stack Overflow"
        );

        assert!(
            runtime.stack_pointer as usize % 4 == 0,
            "[Instruction {}] Stack pointer misaligned",
            runtime.instruction_index - 1
        );

        unsafe {
            runtime.stack_pointer.cast::<u32>().write(val);
            runtime.stack_pointer = runtime.stack_pointer.add(4);
        }
    }

    #[allow(clippy::cast_ptr_alignment)]
    fn pop(runtime: &mut Runtime) -> Self {
        assert!(
            runtime.stack_pointer as usize - 4 >= runtime.stack_bottom as usize,
            "Consumed whole stack!"
        );

        assert!(
            runtime.stack_pointer as usize % 4 == 0,
            "[Instruction {}] Stack pointer misaligned",
            runtime.instruction_index - 1
        );

        unsafe {
            runtime.stack_pointer = runtime.stack_pointer.sub(4);
            runtime.stack_pointer.cast::<u32>().read()
        }
    }
}

impl Stackable for u64 {
    #[allow(clippy::cast_ptr_alignment)]
    fn push(val: Self, runtime: &mut Runtime) {
        assert!(
            runtime.stack_pointer as usize + 8 <= runtime.stack_bottom as usize + STACK_SIZE,
            "Critical Runtime Error: Stack Overflow"
        );

        assert!(
            runtime.stack_pointer as usize % 8 == 0,
            "[Instruction {}] Stack pointer misaligned",
            runtime.instruction_index - 1
        );

        unsafe {
            runtime.stack_pointer.cast::<u64>().write(val);
            runtime.stack_pointer = runtime.stack_pointer.add(8);
        }
    }

    #[allow(clippy::cast_ptr_alignment)]
    fn pop(runtime: &mut Runtime) -> Self {
        assert!(
            runtime.stack_pointer as usize - 8 >= runtime.stack_bottom as usize,
            "Consumed whole stack!"
        );

        assert!(
            runtime.stack_pointer as usize % 8 == 0,
            "[Instruction {}] Stack pointer misaligned",
            runtime.instruction_index - 1
        );

        unsafe {
            runtime.stack_pointer = runtime.stack_pointer.sub(8);
            runtime.stack_pointer.cast::<u64>().read()
        }
    }
}

trait RuntimeInt:
    std::ops::Add<Output = Self>
    + std::ops::Sub<Output = Self>
    + std::ops::Mul<Output = Self>
    + std::ops::Div<Output = Self>
    + std::ops::Rem<Output = Self>
    + Copy
    + std::fmt::Display
    + Stackable
    + Eq
    + Ord
{
    fn as_i128(self) -> i128;
    fn from_i128(val: i128) -> Self;
    fn zero() -> Self;
}

trait Signed: RuntimeInt + std::ops::Neg<Output = Self> {}

impl RuntimeInt for u8 {
    fn as_i128(self) -> i128 {
        i128::from(self)
    }
    fn from_i128(val: i128) -> Self {
        val as Self
    }
    fn zero() -> u8 {
        0
    }
}
impl RuntimeInt for u16 {
    fn as_i128(self) -> i128 {
        i128::from(self)
    }
    fn from_i128(val: i128) -> Self {
        val as Self
    }
    fn zero() -> u16 {
        0
    }
}
impl RuntimeInt for u32 {
    fn as_i128(self) -> i128 {
        i128::from(self)
    }
    fn from_i128(val: i128) -> Self {
        val as Self
    }
    fn zero() -> u32 {
        0
    }
}
impl RuntimeInt for u64 {
    fn as_i128(self) -> i128 {
        i128::from(self)
    }
    fn from_i128(val: i128) -> Self {
        val as Self
    }
    fn zero() -> u64 {
        0
    }
}
impl RuntimeInt for i8 {
    fn as_i128(self) -> i128 {
        i128::from(self)
    }
    fn from_i128(val: i128) -> Self {
        val as Self
    }
    fn zero() -> i8 {
        0
    }
}
impl RuntimeInt for i16 {
    fn as_i128(self) -> i128 {
        i128::from(self)
    }
    fn from_i128(val: i128) -> Self {
        val as Self
    }
    fn zero() -> i16 {
        0
    }
}
impl RuntimeInt for i32 {
    fn as_i128(self) -> i128 {
        i128::from(self)
    }
    fn from_i128(val: i128) -> Self {
        val as Self
    }
    fn zero() -> i32 {
        0
    }
}
impl RuntimeInt for i64 {
    fn as_i128(self) -> i128 {
        i128::from(self)
    }
    fn from_i128(val: i128) -> Self {
        val as Self
    }
    fn zero() -> i64 {
        0
    }
}

impl Stackable for i8 {
    fn push(val: Self, runtime: &mut Runtime) {
        u8::push(reinterpret(val), runtime);
    }

    fn pop(runtime: &mut Runtime) -> Self {
        reinterpret(u8::pop(runtime))
    }
}

impl Stackable for i16 {
    fn push(val: Self, runtime: &mut Runtime) {
        u16::push(reinterpret(val), runtime);
    }

    fn pop(runtime: &mut Runtime) -> Self {
        reinterpret(u16::pop(runtime))
    }
}

impl Stackable for i32 {
    fn push(val: Self, runtime: &mut Runtime) {
        u32::push(reinterpret(val), runtime);
    }

    fn pop(runtime: &mut Runtime) -> Self {
        reinterpret(u32::pop(runtime))
    }
}

impl Stackable for i64 {
    fn push(val: Self, runtime: &mut Runtime) {
        u64::push(reinterpret(val), runtime);
    }

    fn pop(runtime: &mut Runtime) -> Self {
        reinterpret(u64::pop(runtime))
    }
}

impl Signed for i8 {}
impl Signed for i16 {}
impl Signed for i32 {}
impl Signed for i64 {}
