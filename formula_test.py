def unparse(root):
    def infix(t, subtree):
        if len(subtree)!=2:
            string = "**error: \""+t+"\" not given two subtrees**"
        else:
            string = "("+unparse(subtree[0]) + t + unparse(subtree[1])+")"
        return string

    def prefix(t, subtree):
        if len(subtree)!=1:
            string = "**error: \""+t+"\" not given one subtree**"
        else:
            string = t + "(" + unparse(subtree[0]) + ")"
        return string

    def implies(t, subtree):
        if len(subtree)!=2:
            string = "**error: \""+t+"\" not given two subtrees**"
        else:
            (c1,c2) = (subtree[0].get('s',"!"),subtree[1].get('s',"!"))
            if (c1 == "*" and c2=="*"):
                string = unparse(subtree[0]) + t + unparse(subtree[1])
            else:
                mod1 = " does not hold " if c1=='~' else " holds "
                mod2 = " does not hold" if c1=='~' else " holds"
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
        '~' : ("not ", prefix),
        'F' : ("a contradiction", const),
        '*' : (root.get('atom', "**error: atom not specified**"), const),
         1  : ("**error**", const),
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
   
    for test in [p,tree1, tree2, tree3, tree4, tree5, tree6, tree7]:
        print ("The parse tree ")
        print (test)
        print ("is rendered as follows:\n")
        print (unparse(test)+"\n\n-----\n")
