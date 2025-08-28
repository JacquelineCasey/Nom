//! Top level module which organizes compiling items in the program in the correct order.
//!
//! Organizes compilation, ensuring items are processed in the
//! right order. Provides a type `CompilationEnvironment` that tracks information
//! learned from the program.
//!
//! See README.md for documentation for the project as a whole, as well as for the
//! Nom programming language.

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

mod analysis;
mod ast;
mod generate;
mod instructions;
pub mod runtime;
mod token;

mod error;
mod util;

use std::collections::{HashMap, HashSet, VecDeque};

use error::CompileError;
pub use instructions::Instruction;

/// Nom's grammar, represented in parsley's definition language (similar to Backus-Naur form).
///
/// See `src/grammar.parsley` for this grammar.
static PARSER_DEFINITION: &str = include_str!("grammar.parsley"); // Drops the string right into the binary.

/// `CompilationEnviornment` tracks all learned information about the compiling program.
///
/// It should be understood that this represents all of the *currently known* information,
/// so at different stages of compilation, different fields should be complete and accurate.
struct CompilationEnvironment {
    /// The parser object constructed out of the grammar in `grammar.parsley`
    parser: parsley::Parser<token::Token>,
    /// Tracks which compilation goals need to be completed, and in what order.
    queue: CompilationQueue,
    /// Associates function names to information about the function (return type,
    /// arguments, etc.)
    functions: HashMap<String, analysis::Function>,
    /// Associates a type to all the information associated with it.
    types: HashMap<analysis::types::Type, analysis::types::TypeInfo>,
    /// Associates expressions (by id) to their type. This is filled out during
    /// the `type_check` goal.
    type_index: HashMap<u32, analysis::types::Type>,
}

impl CompilationEnvironment {
    /// Constructs a new, empty `CompilationEnvironment`.
    fn new() -> Self {
        CompilationEnvironment {
            parser: parsley::define_parser::<token::Token>(PARSER_DEFINITION)
                .expect("Parser definition should be valid"),
            queue: CompilationQueue::new(),
            functions: HashMap::new(),
            types: analysis::types::get_default_types(),
            type_index: HashMap::new(),
        }
    }

    /// Continuously processes compilation goals until the queue is empty.
    ///
    /// Before calling this, ensure that at least one compilation goal is placed in the queue.
    /// Note that finishing one goal may trigger the addition of follow up goals.
    fn process_goals(&mut self) -> Result<(), CompileError> {
        while !self.queue.is_empty() {
            let goal = self.queue.next_goal().expect("known exists");

            match &goal {
                CompilationGoal::ImportFile(file) => self.import_file(file)?,
                CompilationGoal::ScopeCheck(function_name) => self.scope_check(function_name)?,
                CompilationGoal::TypeCheck(function_name) => self.type_check(function_name)?,
            }

            self.queue.finalize_goal(goal);
        }

        Ok(())
    }

    /// Processes an import file goal, handling parsing and tracking declarations.
    ///
    /// Locates data associated with the file, tokenizes and parses it, and generates
    /// data about the declarations in the file. All declarations are parsed and stored,
    /// but definitions may or may not be created depending on if they are needed.
    ///
    /// Adds a goal to scope check any imported functions.
    fn import_file(&mut self, file: &FileOrString) -> Result<(), CompileError> {
        let (path, input) = match file {
            FileOrString::File(path) => (path, std::fs::read_to_string(path).map_err(|_| "Could not open file")?),
            FileOrString::String(path, data) => (path, data.clone()),
        };

        let tokens = token::tokenize(&input, path)?;

        let syntax_tree =
            self.parser.parse_tokens(&tokens, "Program").map_err(|err| CompileError::ParseError(err, tokens))?;
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

                    self.queue.add_goal(CompilationGoal::ScopeCheck(name));
                }
                ast::DeclarationAST::Struct { name, members, .. } => {
                    analysis::types::add_struct_type(self, &name, members)?;
                }
            }
        }

        Ok(())
    }

    /// Processes a scope check goal, ensuring basic rules are followed and named
    /// items exist.
    ///
    /// Confirms that all variables in the function obey scope rules, and const
    /// rules. Ensures that uses of external objects (functions, varaibles) can be
    /// resolved, and adds goals to define them if needed. Builds a list of local variables.
    ///
    /// Adds a goal to type check the function upon completion.
    fn scope_check(&mut self, function_name: &str) -> Result<(), CompileError> {
        analysis::scope_check(self, function_name)?;

        self.queue.add_goal(CompilationGoal::TypeCheck(function_name.to_string()));

        Ok(())
    }

    /// Processes a type check goal, ensuring type rules are followed and determining
    /// the type of all expressions.
    ///
    /// After this goal completes, the function is ready to be passed to the code
    /// generator.
    fn type_check(&mut self, function_name: &str) -> Result<(), CompileError> {
        // TODO: permit additions of type conversion to AST.
        analysis::type_check(self, function_name)?;

        Ok(())
    }
}

/// A queue of compilation goals, along with associated information.
///
/// Each goal has the restriction that, when processed, it can be executed to completion.
/// It can add additional `CompilationGoals`, but they will not run right away.
/// A goal can be enqueued several times, but will be ignored after the first copy
/// is processed.
///
/// The queue should be thought of as a list of uncompleted goals. Inserted completed
/// goals will have no visible effect, since those goals will never be popped later.
struct CompilationQueue {
    /// The list of goals in the order they should be executed. Goals might be placed
    /// either at the front or the back depending on when it should be executed.
    queue: VecDeque<CompilationGoal>,
    /// A set of completed goals, for use to ensure that a given goal is not completed
    /// twice.
    processed: HashSet<CompilationGoal>,
}

impl CompilationQueue {
    /// Creates an empty `CompliationQueue`.
    fn new() -> Self {
        CompilationQueue { queue: VecDeque::new(), processed: HashSet::new() }
    }

    /// Adds a goal at the end of the queue.
    ///
    /// If it has already been completed by the time it is processed, it will be ignored (i.e. it will not ever be popped).
    fn add_goal(&mut self, goal: CompilationGoal) {
        self.queue.push_back(goal);
        // For debugging / verbose output reasons, we will enqueue the goal even if it has already been processed
    }

    /// Returns the next goal to be completed, should one exist.
    ///
    /// If a goal in the queue has already been completed, then it will not be returned here.
    fn next_goal(&mut self) -> Option<CompilationGoal> {
        if !self.is_empty() {
            // Also clears already completed goals.
            Some(self.queue.pop_front().expect("known exists"))
        } else {
            None
        }
    }

    /// Marks a goal a processed, so the same goal will not be popped again.
    fn finalize_goal(&mut self, goal: CompilationGoal) {
        self.processed.insert(goal);
    }

    /// Returns true if and only if there exists a not yet completed goal in the
    /// queue.
    ///
    /// Also clears completed goals from the front.
    fn is_empty(&mut self) -> bool {
        self.clear_completed_goals();

        self.queue.is_empty()
    }

    /// Clears completed goals at the front of the queue.
    fn clear_completed_goals(&mut self) {
        while !self.queue.is_empty() {
            let goal = self.queue.front().expect("Known exists");

            if self.processed.contains(goal) {
                _ = self.queue.pop_front();
            } else {
                return;
            }
        }
    }
}

/// A `CompliationGoal` expresses a single step of the compilation process.
///
/// We divide these steps so that they can be completed independently as needed for
/// different files, or even items within files. For instance, we may only need to
/// compile some functions from a file, if the others are not used, so we will only
/// scope check those functions.  
///
/// See the functions that implement each of these goals above.
#[derive(PartialEq, Eq, Hash, Debug)]
enum CompilationGoal {
    /// Imports and performs initial processing of a file (or a string). At time
    /// of writing, this defines all declared items, but in the future it might
    /// be more selective about only defining needed items. Completion enqueues
    /// a scope check goal for every function defined.
    ImportFile(FileOrString),
    /// Ensures that variables in the function are declared, and other functions
    /// exist, and so on. Checks the rules of the language that can be checked without
    /// type information.
    /// Should enqueue goals to set other dependencies first, and then one goal
    /// to type check this function.
    ScopeCheck(String),
    /// Type checks a function, and determines the type of every expression within
    /// the function. After this runs, the function is ready to be passed to the
    /// code generator.
    TypeCheck(String),
}

/// A simple type wrapping either a file input, or a direct string input.
#[derive(PartialEq, Eq, Hash, Debug)]
enum FileOrString {
    /// Represents input via a file, given by a string.
    File(String),
    /// Represents direct string input. The first string is a "Fake Path" for use
    /// in diagnostics, such as "\<input\>". The second string is the full program.
    String(String, String),
}

/// Given input, generates a list of instructions.
///
/// Errors are printed to the screen, and cause panics.
///
/// Works by placing a single goal to import the input into the compilation queue.
/// Then completes all goals (goals can spawn more goals) until none remain.
fn compile(file: FileOrString) -> Vec<instructions::Instruction> {
    let mut env = CompilationEnvironment::new();
    env.queue.add_goal(CompilationGoal::ImportFile(file));

    match env.process_goals() {
        Ok(()) => (),
        Err(err) => {
            println!("{}\n", error::pretty_error_msg(&env, &err));
            panic!("Compilation Failed.")
        }
    }

    let generator = generate::CodeGenerator::new();

    generator.generate(&env).expect("Code should generate successfully")
}

/// Given a path to a file, tokenizes, parses, analyzes, and generates code for
/// that file.
///
/// Returns a list of instructions.
///
/// If an error occurs, it will print to the screen, and the function will panic.
pub fn compile_file(path: String) -> Vec<instructions::Instruction> {
    compile(FileOrString::File(path))
}

/// Given a string input, tokenizes, parses, analyzes, and generates code for
/// that input.
///
/// Returns a list of instructions.
///
/// If an error occurs, it will print to the screen, and the function will panic.
pub fn compile_string(input: String) -> Vec<instructions::Instruction> {
    compile(FileOrString::String("<input>".to_string(), input))
}
