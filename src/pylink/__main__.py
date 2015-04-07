# This is very incomplete, please have patience.

from . import grammar

import sys

def foo(*args):
    return grammar.List(reversed(args))

def eval_mma_tree(tree):
    if tree.body is not None:
        tree.body = map(eval_mma_tree, tree.body)

    if tree.head == grammar.MSymbol("PyCall"):
        fn_name = tree.body[0]
        args = tree.body[1].body
        kwargs = tree.body[2]

        fn = globals()[str(fn_name)]
        tree.body = [fn(*args)]

    return tree

cmd = eval(raw_input())
try:
    res = grammar.parse(cmd.strip(), grammar.MExpression)
    res = eval_mma_tree(res)
    print res
except Exception as ex:
    print 'Message[PyCall::pyexc, "{}", "{}""]'.format(type(ex), ex.message)
