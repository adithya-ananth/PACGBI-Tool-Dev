# PACGBI-Tool-Dev

**PACGBI (Pipeline for Automated COBOL Generation from Backlog Items)** is an AI-powered tool that automates debugging and maintenance of COBOL systems by translating GitHub issues into code-level fixes and documentation.
By integrating graph theory, and large language models (LLMs), PACGBI locates affected COBOL functions, generates patches, creates UML diagrams, and opens ready-to-merge pull requests—all following a fully automated GitHub-based workflow.

---

## Features

- **LLM-based Fix Generation** - using Mistral API
- **Graph-based Code Analysis** - for function relevance scoring
- **GitHub Issue Integration** - using the REST API
- **UML Diagram Generation** - for legacy code visualization
- **Automatic Pull Requests** - with commit-ready patches
- **GitHub CLI Extension** -  to easily run the tool from your terminal

---

## Getting Started

Follow these steps to set up and run the project locally:

1. **Clone the Repository**:
    ```bash
    git clone https://github.com/AnirudhArrepu/PACGBI-Tool-Dev.git
    cd PACGBI-Tool-Dev
    ```

2. **Install Required Dependencies**:
    ```bash
    pip install -r requirements.txt
    ```

3. **Set Environment Variables**:
    ```bash
    export MISTRAL_API_KEY=<your_mistral_api_key>
    export GITHUB_TOKEN=<your_github_token>
    ```

4. **Run the Pipeline**:
    ```bash
    python3 pipeline.py
    ```

Alternatively, install and run as a GitHub extension:
```bash
pip install git+https://github.com/AnirudhArrepu/PACGBI-Tool-Dev.git
pacgbi
```

If you find this tool helpful in your research or projects, please cite: <br>
_PACGBI: A Pipeline for Automated COBOL Generation from Backlog Items_.<br>
**Adithya Ananth, Anirudh Arrepu, Dhyanam Janardhana, Gadepalli Srirama Surya Ashish, Srikar Vilas Donur,** and **Sudhanva Bharadwaj BM**.

Mentored by:
_Dr. Sridhar Chimalakonda
IIT Tirupati_
