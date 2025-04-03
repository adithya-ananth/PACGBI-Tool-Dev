import requests
from dotenv import load_dotenv
import os

# Load environment variables
load_dotenv()

TOKEN = os.environ.get("GITHUB_TOKEN")
OWNER = "AnirudhArrepu"
REPO = "PACGBI-Tool-Dev"

HEADERS = {
    "Accept": "application/vnd.github+json",
    "Authorization": f"Bearer {TOKEN}",
    "X-GitHub-Api-Version": "2022-11-28"
}

def get_github_issues():
    """Fetch all issues from GitHub and return them as a list."""
    url = f"https://api.github.com/repos/{OWNER}/{REPO}/issues"
    response = requests.get(url, headers=HEADERS)

    if response.status_code == 200:
        return response.json()  # Return list of issues
    else:
        print(f"Error fetching issues: {response.status_code}, {response.text}")
        return []  # Return empty list if request fails
