fn main() {
    // For tests
    build_deps::rerun_if_changed_paths("samples/**/*.nom").unwrap();

    // Adding the parent directory to the watch-list will capture new-files being added
    build_deps::rerun_if_changed_paths("samples/").unwrap();
}
