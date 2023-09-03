
fn main() {
    // For tests
    build_deps::rerun_if_changed_paths( "samples/**/*" ).unwrap();

    // // Adding the parent directory "res" to the watch-list will capture new-files being added
    // build_deps::rerun_if_changed_paths( "res/*" ).unwrap();
}