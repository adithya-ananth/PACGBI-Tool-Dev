import json
import os
import subprocess
import requests
from dotenv import load_dotenv

load_dotenv()

# Load config and input
with open("config.json", "r") as f:
    config = json.load(f)

with open("input.json", "r") as f:
    data = json.load(f)

# File path and data
file_path = os.path.abspath(data["file_path"])
function_start = data["function_start"]
function_end = data["function_end"]
new_body = data["new_body"].strip()

# Config info
GITHUB_TOKEN = os.environ.get("GITHUB_TOKEN")
print(GITHUB_TOKEN)
print(GITHUB_TOKEN)
REPO_OWNER = config["repo_owner"]
REPO_NAME = config["repo_name"]
base_branch = config["base_branch"]
branch_name = config["branch_name"]
commit_message = config["commit_message"]
pr_title = config["pr_title"]
pr_body = config["pr_body"]
repo_full = f"{REPO_OWNER}/{REPO_NAME}"

# Step 0: Stash local changes if any
print("🔍 Checking for uncommitted changes...")
result = subprocess.run(["git", "status", "--porcelain"], stdout=subprocess.PIPE, text=True)
if result.stdout.strip() != "":
    print("🔁 Stashing local changes...")
    subprocess.run(["git", "stash"], check=True)

# Step 1: Checkout base branch and pull
print(f"📦 Switching to base branch: {base_branch}")
subprocess.run(["git", "checkout", base_branch], check=True)
subprocess.run(["git", "pull"], check=True)

# Step 2: Create or switch to branch
print(f"🌿 Checking for branch: {branch_name}")
branch_check = subprocess.run(["git", "branch", "--list", branch_name], stdout=subprocess.PIPE, text=True)
if branch_check.stdout.strip() == "":
    subprocess.run(["git", "checkout", "-b", branch_name], check=True)
    print(f"✅ Created new branch: {branch_name}")
else:
    subprocess.run(["git", "checkout", branch_name], check=True)
    print(f"🔀 Switched to existing branch: {branch_name}")

# Step 3: Apply stashed changes if any
stash_list = subprocess.run(["git", "stash", "list"], stdout=subprocess.PIPE, text=True).stdout
if stash_list.strip() != "":
    print("📦 Applying stashed changes...")
    subprocess.run(["git", "stash", "pop"], check=True)

# Step 4: Replace function body
print(f"✍️ Replacing function body in: {file_path}")

if not os.path.exists(file_path):
    raise FileNotFoundError(f"❌ File does not exist: {file_path}")

with open(file_path, "r") as f:
    lines = f.readlines()

start_index = end_index = -1
for i, line in enumerate(lines):
    if function_start.strip() in line.strip():
        start_index = i
    elif function_end.strip() in line.strip() and start_index != -1:
        end_index = i
        break

if start_index == -1 or end_index == -1:
    raise Exception("❌ Couldn't find function boundaries!")

# Replace the lines between start and end
lines[start_index + 1:end_index] = [f"{line}\n" for line in new_body.splitlines()]
with open(file_path, "w") as f:
    f.writelines(lines)

print("✅ Function body replaced.")

# Step 5: Commit and push
print("📤 Committing changes...")
subprocess.run(["git", "add", file_path], check=True)
subprocess.run(["git", "commit", "-m", commit_message], check=True)
subprocess.run(["git", "push", "--set-upstream", "origin", branch_name], check=True)

# Step 6: Create Pull Request
print("🚀 Creating pull request...")
headers = {
    "Authorization": f"token {GITHUB_TOKEN}",
    "Accept": "application/vnd.github+json"
}

payload = {
    "title": pr_title,
    "head": branch_name,
    "base": base_branch,
    "body": pr_body
}

response = requests.post(
    f"https://api.github.com/repos/{repo_full}/pulls",
    headers=headers,
    json=payload
)

if response.status_code == 201:
    print("✅ Pull request created!")
    print(response.json()["html_url"])
else:
    print(f"❌ PR Failed: {response.status_code} - {response.text}")
