# BA
BA project on natural language natural deduction

####Development to-do list:
- [ ] Formulation of natural language (NL) syntax for natural deduction proofs
    - [x] Formulation of NL grammar for propositions
    - [x] Implementation of concrete NL syntax for propositions
        - [x] Datatype definitions
        - [x] Implementation of string conversion
    - [x] Formulation of restrictive context-free grammar for natural deduction proofs in NL
    - [x] Formulation of error-inclusive extention of above mentioned grammar
    - [ ] Implementation of string conversion
        - [x] Basic string conversion
        - [ ] Indentation and layout
    - [ ] Implementation of Lexer
    - [ ] Implementation of parser
- [ ] Formulation of formal syntax and corresponding abstract syntax for natural deduction proofs in propositional logic
    - [x] Datatype definitions 
    - [ ] Implementation of conversion to BoxProver syntax
    - [ ] Implementation of validation
        - [ ] Validity checks for steps
            - [ ] Rule reference requirement matches given number of references
            - [ ] References exist
            - [ ] Rule pattern requirement matches the referenced formula(e)
            - [ ] References are acessible (i.e. not in a closed box)
            - [ ] The reference id given to the step is unique
        - [ ] Validity checks for proof
            - [ ] All assumption discharged
            - [ ] The conclusion of the last line is identical to the goal
        - [ ] Error feedback for all failed checks

####Report to-do list:
- [ ] Resumé
- [ ] Abstract
- [ ] Introduction
- [ ] Analysis and theory
    - [ ] Logical formulae in natural language (article)
    - [ ] Context-free grammars (theory)
    - [ ] Restrictive grammar for proofs
        - [ ] Limitations
    - [ ] Feedback argumentation
    - [ ] Extenstion of grammar for proofs
- [ ] Implementation
    - [ ] Choice of programming language
    - [ ] Overall design
    - [ ] Decisions on specific implementation details
        - [ ] Boxes
- [ ] Testing and results
- [ ] Conclusion
