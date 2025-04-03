from github import Github, GithubException
import git

def commit_and_push(issue, code):
    """Commit changes and create a PR"""
    branch_name = f"fix-issue-{issue.number}"
    repo_path = "/github/workspace"

    # Clone repo
    git.Repo.clone_from(repo.clone_url, repo_path, branch=repo.default_branch)
    local_repo = git.Repo(repo_path)
    git_origin = local_repo.remotes.origin

    # Create new branch
    new_branch = local_repo.create_head(branch_name)
    new_branch.checkout()

    # Apply fix
    file_path = os.path.join(repo_path, "fix.py")
    with open(file_path, "w") as f:
        f.write(code)

    # Commit & Push
    local_repo.git.add(update=True)
    local_repo.index.commit(f"Fix: {issue.title}")
    git_origin.push(refspec=f"{branch_name}:{branch_name}")

    # Create PR
    repo.create_pull(
        title=f"Fix: {issue.title}",
        body=f"Auto-generated fix for {issue.title}\n\nCloses #{issue.number}",
        base=repo.default_branch,
        head=branch_name
    )