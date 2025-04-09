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
def build_call_graph(calls_dict):
    G = nx.DiGraph()
    for caller, callees in calls_dict.items():
        for callee in callees:
            G.add_edge(caller, callee)
    return G

file_path_issue = str(retrieve_file_paths())

# issue = get_issue #write this code

with open(f"../{file_path_issue}", 'r') as f:
    code = f.read()

user_defined_functions, calls = parse_cobol_functions_and_calls(code)
functions_body = extract_functions_body(code, only_functions=user_defined_functions)

similarities = get_cosine_similarity_of_functions("some text", functions_body)

G = build_call_graph(calls)

similarities = recalculate_semantic_scores(G, similarities)

top_functions = sorted(similarities, key=lambda x: x[1], reverse=True)
print("highly relevant function: ", [f[0] for f in top_functions])

modified_code = model_pipeline(file_path_issue, top_functions[0][0])

return modified_code
