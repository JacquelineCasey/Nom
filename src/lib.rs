
// I use `cargo clippy -- -D clippy::pedantic`
#![allow(
    clippy::missing_errors_doc,  // Docs? Lol.
    clippy::missing_panics_doc,  // Docs? Lmao.
    clippy::must_use_candidate,  // What?
    clippy::module_name_repetitions,  // Maybe a little weird but I'm bad at naming things.
    clippy::cast_sign_loss,  // I know
    clippy::cast_possible_truncation,  // I know
    clippy::cast_possible_wrap,  // I know
    clippy::if_not_else,  // Actually I like this, its the gaurd pattern
    clippy::upper_case_acronyms,  // Deal with it
    clippy::match_wildcard_for_single_variants,  // I do this on purpose
    clippy::comparison_chain,  // I think this is silly.
)]


mod token;  // Tokenize, instruct parsley how tokens work
mod ast;  // Generate AST from parsley's concrete syntax tree
mod analysis;  // Analyze an AST, checking correctness and learning type info.
mod instructions;  // Define the instruction set of the VM.
mod generate;  // Traverses an AST and returns instructions and other data.
pub mod runtime;  // Runs generated instructions

mod util;  // Utility functions, etc.
mod error;  // Error types


use std::collections::{VecDeque, HashSet, HashMap};

use error::CompileError;
pub use instructions::Instruction;


static PARSER_DEFINITION: &str = include_str!("grammar.parsley");  // Drops the string right into the binary.


// Holds all the information about the compiled program, as it is discovered / decided
// by completing compilation goals.
struct CompilationEnvironment {
    parser: parsley::Parser<token::Token>,
    queue: CompilationQueue,
    functions: HashMap<String, analysis::Function>,
    types: HashMap<analysis::types::Type, analysis::types::TypeInfo>,
    type_index: HashMap<u32, analysis::types::Type>,  // Maps expressions (by id) to types. Filled in by type_check goals
}

impl CompilationEnvironment {
    fn new() -> Self {
        CompilationEnvironment {
            parser: parsley::define_parser::<token::Token>(PARSER_DEFINITION).expect("Parser definition should be valid"),
            queue: CompilationQueue::new(),
            functions: HashMap::new(),
            types: analysis::types::get_default_types(),
            type_index: HashMap::new(),
        }
    }

    fn process_goals(&mut self) -> Result<(), CompileError> {
        while !self.queue.is_empty() {
            let goal = self.queue.next_goal().expect("known exists");

            match &goal {
                CompilationGoal::ImportFile { file, define_all } => self.import_file(file, *define_all)?,
                CompilationGoal::ScopeCheck(function_name) => self.scope_check(function_name)?,
                CompilationGoal::TypeCheck(function_name) => self.type_check(function_name)?,
            }
            
            self.queue.finalize_goal(goal);
        }

        Ok(())
    }

    // Locates data associated with the file, tokenizes and parses it, and generates
    // data about the declarations in the file. All declarations are parsed and stored, 
    // but definitions may or may not be created depending on if they are needed.
    // If define_all is true, goals will be added to define every declaration.
    fn import_file(&mut self, file: &FileOrString, define_all: bool) -> Result<(), CompileError> {
        let (path, input) = match file {
            FileOrString::File(path) => 
                (path, std::fs::read_to_string(path).map_err(|_| "Could not open file")?),
            FileOrString::String(path, data) => 
                (path, data.clone()),
        };

        let tokens = token::tokenize(&input, path)?;

        let syntax_tree = self.parser.parse_tokens(&tokens, "Program")?;
        let mut ast = ast::build_ast(&syntax_tree)?;
        analysis::desugar(&mut ast);

        for decl in ast.declarations {
            match decl {
                ast::DeclarationAST::Variable { .. } => {
                    return Err("Cannot yet process global variables".into());
                }
                ast::DeclarationAST::Function { name, params, block, node_data: _, return_type } => {
                    if self.functions.contains_key(&name) {
                        return Err("Double declaration".into());
                    }
                    
                    // Expects all types in the file to be processed first.
                    self.functions.insert(name.clone(), analysis::Function::new(self, block, params, return_type));

                    if define_all {
                        self.queue.add_goal(CompilationGoal::ScopeCheck(name));
                    }
                }
                ast::DeclarationAST::Struct { .. } => {
                    todo!("Processing structs is not yet implemented")
                }
            }
        }
        
        Ok(())
    }

    // Confirms that all variables in the function obey scope rules, and const
    // rules. Ensures that uses of external objects (functions, varaibles) can be
    // resolved, and adds goals to define them if needed. Builds a list of local variables. 
    // When completed, adds type_check as a goal for the same function.
    fn scope_check(&mut self, function_name: &str) -> Result<(), CompileError> {
        analysis::scope_check(self, function_name)?;

        self.queue.add_goal(CompilationGoal::TypeCheck(function_name.to_string()));
        
        Ok(())
    }

    // Determines the type of every expression in the AST.
    fn type_check(&mut self, function_name: &str) -> Result<(), CompileError> {
        // TODO: permit additions of type conversion to AST.
        analysis::type_check(self, function_name)?;

        Ok(())
    }
}


// A queue of compilation goals, along with associated information.
// Each goal has the restriction that, when processed, it can be executed to completion.
// It can add additional CompilationGoals, but they will not run right away.
// This is primarily for borrow checker reasons...
// A goal can be enqueued several times, but will be ignored after the first copy 
// is processed.
struct CompilationQueue {
    queue: VecDeque<CompilationGoal>,
    processed: HashSet<CompilationGoal>
}

impl CompilationQueue {
    fn new() -> Self {
        CompilationQueue { 
            queue: VecDeque::new(),
            processed: HashSet::new(),
        }
    }

    fn add_goal(&mut self, goal: CompilationGoal) {
        self.queue.push_back(goal);
        // For debugging / verbose output reasons, we will enqueue the goal even if it has already been processed
    }

    // Returns the next goal the needs completion.
    fn next_goal(&mut self) -> Option<CompilationGoal> {
        if !self.is_empty() {
            Some (self.queue.pop_front().expect("known exists"))
        }
        else {
            None
        }
    }

    // Marks a goal a processed, so the same goal will not be popped again.
    fn finalize_goal(&mut self, goal: CompilationGoal) {
        self.processed.insert(goal);
    }

    // Returns true if there are no more goals that needs completion. Completed goals
    // are removed.
    fn is_empty(&mut self) -> bool {
        self.clear_completed_goals();

        self.queue.is_empty()
    }

    // Implementation Details

    fn clear_completed_goals(&mut self) {
        while !self.queue.is_empty() {
            let goal = self.queue.front().expect("Known exists");

            if self.processed.contains(goal) {
                _ = self.queue.pop_front();
            }
            else {
                return
            }
        }
    }
}

// See the functions that implement each goal for detailed documentation.
#[derive(PartialEq, Eq, Hash, Debug)]
enum CompilationGoal {
    ImportFile {file: FileOrString, define_all: bool},  // Depending on define_all, may start definition of these declarations.
    ScopeCheck (String),  // Upon completion, always enques type check. Should enqueue dependencies first (e.g. called functions in other files).
    TypeCheck (String),  // Upon completion, the function is ready to be passed to the code generator.
}

#[derive(PartialEq, Eq, Hash, Debug)]
enum FileOrString {
    File (String),  // Path
    String (String, String)  // A "Fake Path" for diagnostics, and string with data
}


fn compile(file: FileOrString) -> Vec<instructions::Instruction> {
    let mut env = CompilationEnvironment::new();
    env.queue.add_goal(CompilationGoal::ImportFile { file, define_all: true });
    env.process_goals().expect("Goals should complete");

    let generator = generate::CodeGenerator::new();
    
    generator.generate(&env).expect("Code should generate successfully")
}

pub fn compile_file(path: String) -> Vec<instructions::Instruction> {
    compile(FileOrString::File(path))
}

pub fn compile_string(input: String) -> Vec<instructions::Instruction> {
    compile(FileOrString::String("<input>".to_string(), input))
}
