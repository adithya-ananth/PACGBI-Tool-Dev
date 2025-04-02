import requests
from dotenv import load_dotenv
import os

load_dotenv()

TOKEN = os.environ.get("GITHUB_TOKEN")
OWNER = "AnirudhArrepu"
REPO = "PACGBI-Tool-Dev"

url = f"https://api.github.com/repos/{OWNER}/{REPO}/issues"
headers = {
    "Accept": "application/vnd.github+json",
    "Authorization": f"Bearer {TOKEN}",
    "X-GitHub-Api-Version": "2022-11-28"
}

response = requests.get(url, headers=headers)

if response.status_code == 200:
    print(response.json())  # Prints the list of issues
else:
    print(f"Error: {response.status_code}, {response.text}")
