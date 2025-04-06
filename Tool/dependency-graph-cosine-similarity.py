import re
from collections import defaultdict

from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity

def parse_cobol_functions_and_calls(cobol_code):
    # Get the PROCEDURE DIVISION
    procedure_div = re.split(r'^\s*PROCEDURE DIVISION.*$', cobol_code, flags=re.IGNORECASE | re.MULTILINE)
    if len(procedure_div) < 2:
        return {}, {}

    code = procedure_div[1]

    # Identify paragraphs and sections
    function_pattern = re.compile(r'^\s*([\w-]+)\s+(SECTION\.)?', re.MULTILINE)
    perform_pattern = re.compile(r'PERFORM\s+([\w-]+)', re.IGNORECASE)

    functions = []
    calls = defaultdict(list)

    matches = list(function_pattern.finditer(code))

    for i, match in enumerate(matches):
        name = match.group(1)
        start = match.end()
        end = matches[i+1].start() if i+1 < len(matches) else len(code)
        body = code[start:end]

        functions.append(name)
        for call in perform_pattern.findall(body):
            calls[name].append(call)

    return functions, dict(calls)


def extract_functions(code):
    # Extract PROCEDURE DIVISION
    procedure_div = re.split(r'^\s*PROCEDURE DIVISION.*$', code, flags=re.IGNORECASE | re.MULTILINE)
    if len(procedure_div) < 2:
        return {}

    code = procedure_div[1]

    # Match paragraphs/sections
    function_pattern = re.compile(r'^\s*([\w-]+)\s+(SECTION\.)?', re.MULTILINE)
    matches = list(function_pattern.finditer(code))

    functions = {}

    for i, match in enumerate(matches):
        name = match.group(1)
        start = match.end()
        end = matches[i+1].start() if i+1 < len(matches) else len(code)
        body = code[start:end].strip()
        functions[name] = body

    return functions


def get_cosine_similarity_of_functions(issue, functions):
    # Combine issue with function bodies for vectorization
    documents = [issue] + list(functions.values())
    names = list(functions.keys())

    vectorizer = TfidfVectorizer()
    vectors = vectorizer.fit_transform(documents)

    similarities = cosine_similarity(vectors[0:1], vectors[1:]).flatten()

    return similarities