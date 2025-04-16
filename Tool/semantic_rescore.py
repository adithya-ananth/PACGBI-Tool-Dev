import networkx as nx

#if function a is related to function b, then function b's semantic score is added with a factor of 0.2 to the semantic score of function a
def alpha_dependency_score(graph, function_scores, alpha=0.02):
    new_scores = function_scores.copy()
    for node in function_scores:
        for neighbor in graph.successors(node):
            new_scores[neighbor] += alpha * function_scores[node]
    return new_scores

#a functions centrality should highly influence an issue, hence the centrality score is added with a factor of 0.2 to teh re-calculated semantic scores
def beta_centrality_score(graph, function_scores, beta=0.02):
    new_scores = function_scores.copy()
    centrality = nx.betweenness_centrality(graph)
    for node in function_scores:
        new_scores[node] += beta * centrality[node]
    return new_scores

#pipeline to re-calculate the semantic scores of the functions
def recalculate_semantic_scores(graph, scores, alpha=0.2, beta=0.2):
    new_scores = alpha_dependency_score(graph, scores, alpha)
    new_scores = beta_centrality_score(graph, new_scores, beta)
    return new_scores


