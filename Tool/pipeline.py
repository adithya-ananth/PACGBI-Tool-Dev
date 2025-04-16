#first retrieve issue 
#retrieve file path from issue
#open file and generate graph
#perform semantic score
#perform semantic rescore based on graph
#input function to model and then get it updated
#change function and make a commit

from retrieve_issues import retrieve_file_paths
from dependency_graph_cosine_similarity import extract_functions_body, parse_cobol_functions_and_calls, get_cosine_similarity_of_functions
from semantic_rescore import recalculate_semantic_scores
#write model_automation functions import
#write function to make a commit - import
from model_automation import model_pipeline

import networkx as nx
import json
import os

def build_call_graph(all_functions, calls_dict):
    G = nx.DiGraph()

    # Add all functions as nodes
    for func in all_functions:
        G.add_node(func)

    # Add edges based on calls
    for caller, callees in calls_dict.items():
        for callee in callees:
            G.add_edge(caller, callee)

    return G


file_path_issue = str(retrieve_file_paths())
print(file_path_issue)

# issue = get_issue #write this code

with open(f"../{file_path_issue}", 'r') as f:
    code = f.read()

user_defined_functions, calls = parse_cobol_functions_and_calls(code)
print("user defined functions: ", user_defined_functions)
print("calls: ", calls)

functions_body = extract_functions_body(code, only_functions=user_defined_functions)
print("functions body: ", functions_body)

similarities = get_cosine_similarity_of_functions("addition gives an error", functions_body)
print(similarities)

G = build_call_graph(user_defined_functions, calls)
print("graph: ", G)

function_names = list(functions_body.keys())
similarities = {name: score for name, score in zip(function_names, similarities)}
print(similarities)

similarities = recalculate_semantic_scores(G, similarities)
print("recalculated similarities: ", similarities)

top_functions = sorted(similarities.items(), key=lambda x: x[1], reverse=True)
print("highly relevant function: ", [f for f in top_functions])

file_path_issue = f"../{file_path_issue}"

modified_code = model_pipeline(file_path_issue, top_functions[0][0])
print("modified code: ", modified_code)

data = {
    "file_path": file_path_issue,
    "function_start": top_functions[0][0],
    "function_end": "STOP RUN",
    "new_body": modified_code,
}
with open("input.json", "w") as f:
    f.write(json.dumps(data))

os.system("python fixer.py")
