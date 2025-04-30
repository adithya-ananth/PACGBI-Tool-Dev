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
## Repo Structure
Papers - Refernce Paper
Tool - The files containing the helper classes used in the pipeline.
cobal-test - Contains the test files on which the model was evaluated

## Getting Started

Follow these steps to set up and run the project locally:

1. **Clone the Repository**:
    ```bash
    git clone https://github.com/AnirudhArrepu/PACGBI-Tool-Dev.git
    cd PACGBI-Tool-Dev
    ```

2. Install required dependencies in a new virtual environmnet:
    ```
    python -m venv env
    ./env/Scripts/activate
    pip install -r requirements.txt
    ```
    
3. Run the pipeline.py file in the Tool Directory to execute the code via the terminal:
    ```
    cd Tool
    python pipeline.py
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
_Dr. Sridhar Chimalakonda,
IIT Tirupati_
