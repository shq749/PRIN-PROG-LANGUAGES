from prolog_structures import Rule, RuleBody, Term, Function, Variable, Atom, Number
from typing import List
from functools import reduce

import sys
import random

class Not_unifiable(Exception):
    pass

'''
Please read prolog_structures.py for data structures
that represent Prolog terms, rules, and goals.
'''
class Interpreter:
    def __init__(self):
        pass

    '''
	Example
	occurs_check (v, t) where v is of type Variable, t is of type Term.
	occurs_check (v, t) returns true if the Prolog Variable v occurs in t.
	Please see the lecture note Control in Prolog to revisit the concept of
	occurs-check.
	'''
    def occurs_check (self, v : Variable, t : Term) -> bool:
        if isinstance(t, Variable):
            return v == t
        elif isinstance(t, Function):
            for t in t.terms:
                if self.occurs_check(v, t):
                    return True
            return False
        return False


    '''
	Problem 1
	variables_of_term (t) where t is of type Term.
	variables_of_clause (c) where c is of type Rule.

	The function should return the Variables contained in a term or a rule
	using Python set.

	The result must be saved in a Python set. The type of each element (a Prolog Variable)
	in the set is Variable.
    '''
    def variables_of_term (self, t : Term) -> set :
        res=set()
        if isinstance(t, Variable):
            res.add(t)
            return res
        elif isinstance(t, Function): 
            for i in t.terms:
                if isinstance(i, Variable):
                    res.add(i)
                else: 
                    pass
            return res
        else: 
            return res
            

    def variables_of_clause (self, c : Rule) -> set :
        rul1=set()
        rul2=set()
        head=c.head.terms
        body=c.body.terms
        for h in head:
            if isinstance(h,Variable):
                rul1.add(h)
            elif isinstance(h, Function):
                for i in h.terms: 
                    if isinstance(i, Variable):
                        rul1.add(i)
                    else: 
                        pass
            else: 
                pass
        for b in body:
            if isinstance(b, Variable):
                rul2.add(body)
            elif isinstance(b, Function): 
                for i in b.terms: 
                    if isinstance(i, Variable):
                        rul2.add(i)
                    else: 
                        pass
            else: 
                pass
        return rul1.union(rul2)
    '''
	Problem 2
	substitute_in_term (s, t) where s is of type dictionary and t is of type Term
	substitute_in_clause (s, t) where s is of type dictionary and c is of type Rule,

	The value of type dict should be a Python dictionary whose keys are of type Variable
	and values are of type Term. It is a map from variables to terms.

	The function should return t_ obtained by applying substitution s to t.

	Please use Python dictionary to represent a subsititution map.
	'''
    def substitute_in_term (self, s : dict, t : Term) -> Term:
        keys=s.keys()
        if isinstance(t,Function):
            new_t=[]
            for i in t.terms:
                if isinstance(i, Function):
                    i=self.substitute_in_term(s,i)
                elif isinstance(i, Variable):
                    for key in keys:
                        if i==key:
                            i=s[key]
                        else:
                            pass
                else:
                    pass
                new_t.append(i)
            return Function(t.relation, new_t)
        else:
            if isinstance(t,Variable):
                new_t=t
                for key in keys:
                    if t==key:
                        new_t=s[key]
                    else:
                        pass
                return new_t
            else: 
                return t
    def substitute_in_clause (self, s : dict, c : Rule) -> Rule:
        hd=c.head
        hdterm=c.head.terms
        body=c.body
        bdterm=c.body.terms
        head=[]
        if isinstance(hd, Variable):
            c.head=self.substitute_in_term(s, hd)
        elif isinstance(hd, Function):
            c.head=self.substitute_in_term(s, hd)
        else: 

            for i in hdterm:
                if isinstance(i, Variable):
                    i=self.substitute_in_term(s, i)
                elif isinstance(i, Function):
                    i=self.substitute_in_term(s, i)
                head.append(i)
            c.head.terms=head
            body=[]
            for i in bdterm:
                if isinstance(i, Variable):
                    i=self.substitute_in_term(s, i)
                elif isinstance(i, Function):
                    i=self.substitute_in_term(s, i)
                body.append(i)
            c.body.terms=body
        return c
    '''
	Problem 3
	unify (t1, t2) where t1 is of type term and t2 is of type Term.
	The function should return a substitution map of type dict,
	which is a unifier of the given terms. You may find the pseudocode
	of unify in the lecture note Control in Prolog useful.

	The function should raise the exception raise Not_unfifiable (),
	if the given terms are not unifiable.

	Please use Python dictionary to represent a subsititution map.
	'''
    def unify (self, t1: Term, t2: Term) -> dict:
        res=self.unify_2(t1, t2, {})
        return res
    
    def deter(self, a, b):
        z1=False
        z2=False
        if isinstance(a, Function):
            if b not in a.terms:
                z1=True
            else:
                pass
        elif a!=b:
            z2=True
        return z1, z2

    def replace(self, unifer):
        for key in unifer:
            for key2 in unifer:
                if key==unifer[key2]:
                    unifer[key2]=unifer[key]
        return unifer
                        

    def unify_2(self, X, Y, unifer):
        x=self.substitute_in_term(unifer,X)
        y=self.substitute_in_term(unifer,Y)
        z1, z2=self.deter(y,x)
        z3, z4=self.deter(x,y)
        if isinstance(x, Variable) and (z1 or z2):
            i=False
            for key in unifer:
                if key==x:
                    i=True
                    unifer[key]=y
                    break
                else:
                    pass
            if i==False:
                unifer[x]=y

            else: 
                pass
            unifer=self.replace(unifer)
            return unifer
        elif isinstance(y, Variable) and (z3 or z4):
            i=False
            for key in unifer:
                if key==y:
                    i=True
                    unifer[key]=x
                else:
                    pass
            if i==False:
                unifer[y]=x
            else:
                pass
            unifer=self.replace(unifer)
            return unifer
        elif x==y:
            return unifer
        elif isinstance(x, Function) and isinstance(y, Function) and len(x.terms) == len(y.terms):
            pre_lst=zip(x.terms, y.terms)
            f_list=list(pre_lst)
            c=reduce((lambda unifer, a: self.unify_2(a[0], a[1], unifer)), f_list, unifer)
            c=self.replace(c)
            return c
        else:
            raise Not_unifiable()
            


    fresh_counter = 0
    def fresh(self) -> Variable:
        self.fresh_counter += 1
        return Variable("_G" + str(self.fresh_counter))
    def freshen(self, c: Rule) -> Rule:
        c_vars = self.variables_of_clause(c)
        theta = {}
        for c_var in c_vars:
            theta[c_var] = self.fresh()

        return self.substitute_in_clause(theta, c)


    '''
	Problem 4
	Following the Abstract interpreter pseudocode in the lecture note Control in Prolog to implement
	a nondeterministic Prolog interpreter.

	nondet_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of Terms (results), which is an instance of the original goal and is
	a logical consequence of the program. See the tests cases (in src/main.py) as examples.
	'''
    def nondet_query (self, program : List[Rule], pgoal : List[Term]) -> List[Term]:
        resolve=pgoal[:]
        g=self.nondet_2(program, pgoal, resolve, {})
        count=0
        return g

    def nondet_2(self, program, goals, resolve, unifer):
        while resolve:
            random_int=random.randint(0, len(resolve)-1)
            A=resolve[random_int]
            rules=[]
            resolve_2=[]
            for rule in program:
                if isinstance(A, RuleBody):
                    for term in A.terms:
                        if rule.head.relation==term.relation:
                            rules.append(rule)
                elif rule.head.relation==A.relation:
                    rules.append(rule)
                else:
                    pass
            if rules==[]:
                return goals
            random_rule=random.randint(0, len(rules)-1)
            chosen_rule=rules[random_rule]
            chosen_rule=self.freshen(chosen_rule)
            try:
                unifer=self.unify_2(A, chosen_rule.head,unifer)
            except:
                self.nondet_query(program, goals)

            resolve.remove(A)
            
            for b in chosen_rule.body.terms:
                b=self.substitute_in_term(unifer, b)
                resolve.append(b)
            go=[]
            for i in goals:
                i=self.substitute_in_term(unifer, i)
                go.append(i)
            if not resolve:
                return go


    '''
	Challenge Problem

	det_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of term lists (results). Each of these results is
	an instance of the original goal and is a logical consequence of the program.
	If the given goal is not a logical consequence of the program, then the result
	is an empty list. See the test cases (in src/main.py) as examples.
	'''
    def dfs(self, goal, resolve, resu, program): 
        if not resolve:
            if goal not in resu:
                resu.append(goal)
            return True
        while resolve:
            for i in resolve:
                our_goal=i
                break
            resolve.remove(our_goal)
            searched=False
            for p in program:
                res=[]
                goalp=[]
                rel1=p.head.relation
                rel2=our_goal.relation
                if rel1==rel2:
                    p=self.freshen(p)
                    try:
                        unifer=self.unify(our_goal, p.head)
                    except:
                        return resu
                    new_resolve=resolve[:]
                    n_goal=goal[:]
                    body=p.body
                    for b in body.terms:
                        new_resolve.append(b)
                    for re in new_resolve:
                        re=self.substitute_in_term(unifer, re)
                        res.append(re)
                    for re in n_goal:
                        re=self.substitute_in_term(unifer, re)
                        goalp.append(re)
                    new_resolve=res
                    n_goal=goalp

                    r=self.dfs(n_goal, new_resolve, resu, program)
                    searched=r or searched
                    if r==resu[::-1]:
                        return r
                else: 
                    if searched==True:
                        return resu[::-1]
        if not searched:
            return

    def det_query (self, program : List[Rule], pgoal : List[Term]) -> List[List[Term]]:
        resolve=pgoal[:]
        c=self.dfs(pgoal, resolve, [], program)
        return c
