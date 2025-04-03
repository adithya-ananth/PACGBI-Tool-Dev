from issues import get_github_issues
import re

def extract_file_paths(issue_text):
    """Extract file paths enclosed in backticks (` `) or written in separate lines."""
    file_paths = re.findall(r"`([^`]+)`", issue_text) 
    lines = issue_text.split("\n")  
    for line in lines:
        if line.strip().endswith(".cbl"):  
            file_paths.append(line.strip())
    return file_paths

if __name__ == "__main__":
    issues = get_github_issues()

    for issue in issues:
        issue_text = issue.get("body", "")  
        file_paths = extract_file_paths(issue_text)

        for path in file_paths:
            print(f"Extracted File Path: {path}")  
