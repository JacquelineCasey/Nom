
use crate::instructions::{Instruction, IntegerBinaryOperation, IntegerUnaryOperation, IntSize};

use std::alloc::{Layout, alloc, dealloc};


const STACK_SIZE: usize = 1048576;  // In terms of u8 units. This exactly a megabyte.


pub struct Runtime {
    instructions: Vec<Instruction>,
    instruction_pointer: usize,  // Really just an index
    stack_pointer: *mut u8,  // Current location of the top of the stack, i.e. no value lives here.
    frame_pointer: *mut u8,  // Current location of bottom of the frame. Locals are available, as well as return value and previous frame pointer.
    stack_bottom: *mut u8,
    stack_layout: Layout,
    running: bool,
}


impl Runtime {
    pub fn new(instructions: Vec<Instruction>) -> Runtime {
        let stack_layout = Layout::array::<u64>(STACK_SIZE / 8).expect("Memory should be allocated");
        let stack = unsafe { alloc(stack_layout) };
        
        // See Drop implementation

        Runtime { 
            instructions, 
            instruction_pointer: 0, 
            stack_pointer: stack, 
            stack_bottom: stack, 
            frame_pointer: stack, 
            stack_layout, 
            running: false 
        }   
    }
    
    pub fn run(mut self) {
        self.running = true;

        while self.running {
            let instruction = self.instructions[self.instruction_pointer];
            self.instruction_pointer += 1;  // Might be overriden by running a jump

            self.eval_instruction(instruction);
        } 
    }

    fn eval_instruction(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::IntegerBinaryOperation(op, size) => {
                self.eval_binary_int_op(op, size);
            }
            Instruction::UnaryOperation(_, _) => {
                todo!();
            },
            Instruction::AdvanceStackPtr(amount) => {
                self.stack_pointer = unsafe { self.stack_pointer.add(amount) };
            },
            Instruction::RetractStackPtr(amount) => {
                self.stack_pointer = unsafe { self.stack_pointer.sub(amount) };
            },
            Instruction::DebugPrintUnsigned(size) => {
                self.eval_instruction(Instruction::Duplicate(size));
                match size {
                    IntSize::OneByte => {
                        println!("{}", u8::pop(self));
                    }
                    IntSize::TwoByte => {
                        println!("{}", u16::pop(self));
                    }
                    IntSize::FourByte => {
                        println!("{}", u32::pop(self));
                    }
                    IntSize::EightByte => {
                        println!("{}", u64::pop(self));
                    }
                }
            }
            Instruction::Duplicate(size) => {
                match size {
                    IntSize::OneByte => self.duplicate::<u8>(),
                    IntSize::TwoByte => self.duplicate::<u16>(),
                    IntSize::FourByte => self.duplicate::<u32>(),
                    IntSize::EightByte => self.duplicate::<u64>(),
                }
            }
            Instruction::Exit => {
                self.running = false;
            }
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

    fn eval_binary_int_op_impl<U: Stackable, S: Number>(&mut self, op: IntegerBinaryOperation) {
        let right = U::pop(self);
        let left = U::pop(self);

        let result = match op {
            IntegerBinaryOperation::UnsignedAddition =>
                left + right,
            IntegerBinaryOperation::SignedAddition => 
                reinterpret::<S, U>(reinterpret::<U, S>(left) + reinterpret::<U, S>(right)),
            IntegerBinaryOperation::UnsignedSubtraction => 
                left - right,
            IntegerBinaryOperation::SignedSubtraction => 
                reinterpret::<S, U>(reinterpret::<U, S>(left) + reinterpret::<U, S>(right)),
            IntegerBinaryOperation::UnsignedMultiplication => 
                left * right,
            IntegerBinaryOperation::SignedMultiplication => 
                reinterpret::<S, U>(reinterpret::<U, S>(left) * reinterpret::<U, S>(right)),
            IntegerBinaryOperation::UnsignedDivision => 
                left / right,
            IntegerBinaryOperation::SignedDivision => 
                reinterpret::<S, U>(reinterpret::<U, S>(left) / reinterpret::<U, S>(right)),
        };

        U::push(result, self);
    }

    fn eval_unary_int_op_impl<U: Stackable, S: Signed>(&mut self, op: IntegerUnaryOperation) {
        let val = U::pop(self);
        
        let result = match op {
            IntegerUnaryOperation::NegateSigned => {
                reinterpret::<S, U>(- reinterpret::<U, S>(val))
            }
        };

        U::push(result, self);        
    }
}

impl Drop for Runtime {
    fn drop(&mut self) {
        unsafe { dealloc(self.stack_bottom, self.stack_layout) }
    }
}


/* Type shenanigans */

/* Marker trait representing something we might push and pull from the stack. */
trait Stackable: Number {
    /* These operations assume but also check alignment. If alignment is wrong, they panic. */
    fn push(val: Self, runtime: &mut Runtime); 
    fn pop(runtime: &mut Runtime) -> Self; 
}

/* We always think about items on the stack as unsized integers, even if in reality
 * they are floats, or booleans, or pointers etc. */
impl Stackable for u8 {
    fn push(val: Self, runtime: &mut Runtime) {
        // Remember to use pointer::offset() to actually get the next pointer.
        // pointer::offset is UB if it goes outside of the allocation though, hence
        // the checks above being done in usize.

        if runtime.stack_pointer as usize + 1 > runtime.stack_bottom as usize + STACK_SIZE {
            panic!("Out of stack memory");
        }

        // Skip alignment check.

        unsafe { 
            runtime.stack_pointer.write(val); 
            runtime.stack_pointer = runtime.stack_pointer.add(1);
        }
    }

    fn pop(runtime: &mut Runtime) -> Self {
        if runtime.stack_pointer as usize - 1 < runtime.stack_bottom as usize {
            panic!("Consumed whole stack!");
        }

        // Skip alignment check

        unsafe {
            runtime.stack_pointer = runtime.stack_pointer.sub(1);
            runtime.stack_pointer.read()
        }
    }
}

impl Stackable for u16 {
    fn push(val: Self, runtime: &mut Runtime) {
        if runtime.stack_pointer as usize + 2 > runtime.stack_bottom as usize + STACK_SIZE {
            panic!("Out of stack memory");
        }
        
        if runtime.stack_pointer as usize % 2 != 0 {
            panic!("Stack pointer misaligned");
        }

        unsafe { 
            (runtime.stack_pointer as *mut u16).write(val); 
            runtime.stack_pointer = runtime.stack_pointer.add(2);
        }
    }

    fn pop(runtime: &mut Runtime) -> Self {
        if runtime.stack_pointer as usize - 2 < runtime.stack_bottom as usize {
            panic!("Consumed whole stack!");
        }

        if runtime.stack_pointer as usize % 2 != 0 {
            panic!("Stack pointer misaligned");
        }

        unsafe {
            runtime.stack_pointer = runtime.stack_pointer.sub(2);
            (runtime.stack_pointer as *mut u16).read()
        }
    }
}

impl Stackable for u32 {
    fn push(val: Self, runtime: &mut Runtime) {
        if runtime.stack_pointer as usize + 4 > runtime.stack_bottom as usize + STACK_SIZE {
            panic!("Out of stack memory");
        }
        
        if runtime.stack_pointer as usize % 4 != 0 {
            panic!("Stack pointer misaligned");
        }

        unsafe { 
            (runtime.stack_pointer as *mut u32).write(val); 
            runtime.stack_pointer = runtime.stack_pointer.add(4);
        }
    }

    fn pop(runtime: &mut Runtime) -> Self {
        if runtime.stack_pointer as usize - 4 < runtime.stack_bottom as usize {
            panic!("Consumed whole stack!");
        }

        if runtime.stack_pointer as usize % 4 != 0 {
            panic!("Stack pointer misaligned");
        }

        unsafe {
            runtime.stack_pointer = runtime.stack_pointer.sub(4);
            (runtime.stack_pointer as *mut u32).read()
        }
    }
}

impl Stackable for u64 {
    fn push(val: Self, runtime: &mut Runtime) {
        if runtime.stack_pointer as usize + 8 > runtime.stack_bottom as usize + STACK_SIZE {
            panic!("Out of stack memory");
        }
        
        if runtime.stack_pointer as usize % 8 != 0 {
            panic!("Stack pointer misaligned");
        }

        unsafe { 
            (runtime.stack_pointer as *mut u64).write(val); 
            runtime.stack_pointer = runtime.stack_pointer.add(8);
        }
    }

    fn pop(runtime: &mut Runtime) -> Self {
        if runtime.stack_pointer as usize - 8 < runtime.stack_bottom as usize {
            panic!("Consumed whole stack!");
        }

        if runtime.stack_pointer as usize % 8 != 0 {
            panic!("Stack pointer misaligned");
        }

        unsafe {
            runtime.stack_pointer = runtime.stack_pointer.sub(8);
            (runtime.stack_pointer as *mut u64).read()
        }
    }
}

trait Number : 
    std::ops::Add<Output = Self> 
    + std::ops::Sub<Output = Self> 
    + std::ops::Mul<Output = Self> 
    + std::ops::Div<Output = Self> 
    + Copy
{ } 

trait Signed : Number + std::ops::Neg<Output = Self> { }

impl Number for u8 { }
impl Number for u16 { }
impl Number for u32 { }
impl Number for u64 { }
impl Number for i8 { }
impl Number for i16 { }
impl Number for i32 { }
impl Number for i64 { }

impl Signed for i8 { }
impl Signed for i16 { }
impl Signed for i32 { }
impl Signed for i64 { }


/* Refer to: https://users.rust-lang.org/t/transmuting-a-generic-array/45645/5 */
fn reinterpret<In: Number, Out: Number>(i: In) -> Out {
    let ptr = &i as *const In as *const Out;
    unsafe { *ptr }
}
