from github import Github
from github.InputGitAuthor import InputGitAuthor

g = Github("")

# Get the repository
repo = g.get_repo("AnirudhArrepu/PACGBI-Tool-Dev")  

source_branch = "feature-branch"  
target_branch = "main"  

# Define the pull request title and body
pr_title = "Add feature X"
pr_body = "This PR adds feature X to the project."

# Create the pull request
pr = repo.create_pull(
    title=pr_title,
    body=pr_body,
    head=source_branch,
    base=target_branch
)

print(f"Pull request created: {pr.html_url}")
