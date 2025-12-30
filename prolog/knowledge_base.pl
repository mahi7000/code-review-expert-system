:- module(knowledge_base, [rule/4, explanation/2]).

% rule(RuleID, Category, Severity, Description)
rule(rule8, performance, medium, "range(len()) usage").
rule(rule9, performance, medium, "String concat in loops").
rule(rule10, performance, medium, "type() vs isinstance()").
rule(rule11, performance, high, "Mutable default args").
rule(rule12, performance, low, "List built via for-loop").
rule(rule13, performance, medium, "open() without with").
rule(rule14, performance, high, "Empty except clause").

% rule(rule1, style, low, "Line length >79 chars").
% rule(rule2, style, medium, "Missing docstrings").
% rule(rule3, style, medium, "Naming conventions").
% rule(rule4, style, medium, "Missing type hints").
% rule(rule5, style, medium, "Unused imports").
% rule(rule6, style, low, "TODO/FIXME comments").
% rule(rule7, style, medium, "Magic numbers").

% rule(rule15, security, high, "eval() usage").
% rule(rule16, security, high, "pickle.load() usage").
% rule(rule17, security, high, "Shell injection risk").
% rule(rule18, security, high, "Hardcoded credentials").
% rule(rule19, security, medium, "Missing if __name__ guard").
% rule(rule20, security, high, "Bare except clause").

% Suggestions
explanation(rule8, "Use enumerate() instead of range(len())").
explanation(rule9, "Use join() for string concatenation in loops").
explanation(rule10, "Use isinstance() instead of type()").
explanation(rule11, "Avoid mutable default arguments").
explanation(rule12, "Use list comprehensions instead of append in loops").
explanation(rule14, "Avoid bare except clauses").
