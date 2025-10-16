use std::process::Command;

/// Expects to be run on a unix-like system (I'm on Git Bash for Windows)
pub fn current_git_commit() -> String {
    // git rev-parse --short HEAD
    let output = Command::new("git")
        .arg("rev-parse")
        .arg("--short")
        .arg("HEAD")
        .output()
        .expect("failure to read current git commit");
    String::from_utf8_lossy(&output.stdout[..]).trim().to_owned()
}

/// Expects to be run on a unix-like system (I'm on Git Bash for Windows)
pub fn current_datetime() -> String {
    let output = Command::new("date")
        .output()
        .expect("failure to read current datetime");
    String::from_utf8_lossy(&output.stdout[..]).trim().to_owned()
}

pub fn current_revision_summary() -> String {
    format!("[git:{} -- {}]", current_git_commit(), current_datetime())
}
