/* Generated instructions can be obviously inefficient. In fact, there are even
 * no-ops, such as AdvanceStackPtr(0) */

use super::PseudoInstruction;
use crate::instructions::Instruction;

/* Optimizes the code of a single function */
pub fn optimize(instructions: Vec<PseudoInstruction>) -> Vec<PseudoInstruction> {
    use Instruction as I;
    use PseudoInstruction as PI;

    // Combine nearby stack_ptr moves
    let mut final_instructions = vec![];
    let mut next_move: isize = 0;

    for instr in instructions {
        match instr {
            PI::Actual(I::AdvanceStackPtr(i)) => {
                next_move += i as isize;
            }
            PI::Actual(I::RetractStackPtr(i)) => {
                next_move -= i as isize;
            }
            PI::Actual(I::Return) => {
                next_move = 0; // Stack ptr movement does not matter here
                final_instructions.push(PI::Actual(I::Return));
            }
            PI::Actual(I::RetractMoving(0, _)) => {
                continue;
            }
            other => {
                if next_move > 0 {
                    final_instructions.push(PI::Actual(I::AdvanceStackPtr(next_move as usize)));
                } else if next_move < 0 {
                    final_instructions.push(PI::Actual(I::RetractStackPtr((-next_move) as usize)));
                }
                next_move = 0;

                final_instructions.push(other);
            }
        }
    }

    final_instructions
}
