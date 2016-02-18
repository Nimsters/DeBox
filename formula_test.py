symbols = {
    '&' : ("and", "c1, t, c2"),
    '\/' : ("or",  "c1, t, c2"),
    '->' : ("implies", "t, c"),
    '~'  : ("not",     "t, c"),
    'F'  : ("a contradiction", ""),
    'atom': ("**", "atom"),
    'NO SYMBOL': ("Error: No symbol", ""),
    }

def unparse(root):
    (text, structure) = symbols[root['s']]
    return text
