:- module(knowledge_base, [rule/4, explanation/2]).

% rule(RuleID, Category, Severity, Description)


% ---------------- PERFORMANCE RULES ----------------
rule(rule8,  performance, medium, "range(len()) usage").
rule(rule9,  performance, medium, "String concat in loops").
rule(rule10, performance, medium, "type() vs isinstance()").
rule(rule11, performance, high,   "Mutable default args").
rule(rule12, performance, low,    "List built via for-loop").
rule(rule13, performance, medium, "open() without with").
rule(rule14, performance, high,   "Empty except clause").

% ---------------- STYLE RULES ----------------
rule(rule1, style, low,    "Line length > 79 characters").
rule(rule2, style, medium, "Missing docstrings").
rule(rule3, style, medium, "Naming conventions").
rule(rule4, style, medium, "Missing type hints").
rule(rule5, style, medium, "Unused imports").
rule(rule6, style, low,    "TODO/FIXME comments").
rule(rule7, style, medium, "Magic numbers").

% ---------------- SECURITY RULES ----------------
rule(rule15, security, high,   "eval() usage").
rule(rule16, security, high,   "pickle.load() usage").
rule(rule17, security, high,   "Shell injection risk").
rule(rule18, security, high,   "Hardcoded credentials").
rule(rule19, security, medium, "Missing if __name__ == '__main__' guard").
rule(rule20, security, high,   "Bare except clause").


% explanation(RuleID, Suggestion)

% -------- PERFORMANCE --------
explanation(rule8,  "Use enumerate() instead of range(len())").
explanation(rule9,  "Use join() for string concatenation in loops").
explanation(rule10, "Use isinstance() instead of type()").
explanation(rule11, "Avoid mutable default arguments").
explanation(rule12, "Use list comprehensions instead of append in loops").
explanation(rule13, "Use with open(...) to ensure file is closed").
explanation(rule14, "Catch specific exceptions instead of using bare except").

% -------- STYLE --------
explanation(rule1, "Split the line to be at most 79 characters").
explanation(rule2, "Add a docstring immediately after function or class definition").
explanation(rule3, "Use snake_case for functions/variables and CamelCase for classes").
explanation(rule4, "Add type hints to function parameters and return values").
explanation(rule5, "Remove unused imports").
explanation(rule6, "Resolve or remove TODO/FIXME comments").
explanation(rule7, "Replace magic numbers with named constants").

% -------- SECURITY --------
explanation(rule15, "Use ast.literal_eval() or safer parsing alternatives").
explanation(rule16, "Avoid pickle on untrusted data; use JSON instead").
explanation(rule17, "Use subprocess.run() with argument lists instead of shell strings").
explanation(rule18, "Store secrets in environment variables or secure vaults").
explanation(rule19, "Wrap executable code inside if __name__ == '__main__'").
explanation(rule20, "Catch specific exception types, not a bare except").
