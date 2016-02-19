def unparse(root):
    def bracket(t):
        return "("+t+")"
    
    def infix(t, subtree):
        if len(subtree)!=2:
            return "**error: \""+t+"\" not given two subtrees**"
        (c1,c2) = (subtree[0].get('s',"!"),subtree[1].get('s',"!"))
        (first, second) = (unparse(subtree[0]),unparse(subtree[1]))
        if c1 not in ['*','~']:
            first = bracket(first)
        if c2 not in ['*','~']:
            second = bracket(second)
        return first + t + second

    def prefix(t, subtree):
        if len(subtree)!=1:
            return "**error: \""+t+"\" not given one subtree**"
        sub = unparse(subtree[0])
        c = subtree[0].get('s',"!")
        if c not in ['*', 'not']:
            string = t+" "+bracket(sub)
        else:
            string = t +"-"+sub
        return string

    def implies(t, subtree):
        if len(subtree)!=2:
            return "**error: \""+t+"\" not given two subtrees**"
        (c1,c2) = (subtree[0].get('s',"!"),subtree[1].get('s',"!"))
        if (c1 == "*" and c2=="*"):
            string = unparse(subtree[0]) + t + unparse(subtree[1])
        else:
            mod1 = " does not hold " if c1=='~' else " holds "
            mod2 = " does not hold" if c2=='~' else " holds"
            if c2 == "F": mod2 = " is implied"
            string = ("if "   + unparse(subtree[0]) + mod1 +
                      "then " + unparse(subtree[1]) + mod2)
        return string
    
    def const(t, subtree):
        return t

    symbols = {
        '&' : (" and ", infix),
        '/' : (" or ", infix),
        '>' : (" implies ", implies),
        '~' : ("not", prefix),
        'F' : ("a contradiction", const),
        '*' : (root.get('atom', "**error: atom not specified**"), const),
         1  : ("**error:symbol not specified**", const),
    }

    (text, function) = symbols.get(root.get('s', 1), 
                                   ("**error:undefined symbol**",const))
    
    return function(text, root.get('subtree',[]))

if __name__ == "__main__":
    p = {'s':"*", 'atom': "p"}
    q = {'s':"*", 'atom': "q"}
    tree1 = {'s':"~", 'subtree':[p]}
    tree2 = {'s':"&", 'subtree':[tree1, q]}
    tree3 = {'s':"~", 'subtree':[{'s':"*", 'atom':"r"}]}
    tree4 = {'s':"/", 'subtree':[q,tree3]}
    tree5 = {'s':"&", 'subtree':[p,tree4]}
    tree6 = {'s':">", 'subtree':[tree2,tree4]}
    tree7 = {'s':">", 'subtree':[{'s':"&", 'subtree':[p,tree1]},{'s':"F"}]}
   
    for test in [p,tree1, tree2, tree4, tree5, tree6, tree7]:
        print ("The parse tree ")
        print (test)
        print ("is rendered as follows:\n")
        print (unparse(test)+"\n\n-----\n")
