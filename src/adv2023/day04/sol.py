import json
from functools import reduce
from itertools import chain

def lines(filename):
    with open(filename, 'r') as file:
        res = file.readlines()
    return res

input_lines = [x[10:].strip().split(' | ') for x in lines("input.txt")]

def parse_list(l):
    return set([int(x) for x in l if x != ''])

def parse(parts):
    return tuple(map(parse_list, [x.split(" ") for x in parts]))

def score(n):
    return int(2 ** (n - 1))

def win_count(cards):
    guesses, winners = cards
    return len(guesses.intersection(winners))

def score_game(cards):
    return score(win_count(cards))

data = [parse(line) for line in input_lines]

## PART 1
print("PART 1", sum(map(score_game, data))) # => 20107

def next_indexes(idx, count):
    next_idx = idx + 1
    return range(next_idx, next_idx + count)

graph = [next_indexes(idx, win_count(data[idx])) for idx in range(0, len(data))]

memoize = dict()

def depth_first_traversal(data, node):
    if(str(node) in memoize):
        return memoize[str(node)]
    else:
        travs = [depth_first_traversal(data, x) for x in data[node]]
        res = [node] + list(chain(*travs))
        memoize[str(node)] = res
        return res

res = list(chain(*[depth_first_traversal(graph, x) for x in range(0, len(graph))]))
print("PART 2", len(res))

