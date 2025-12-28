:- [prolog/utils/file_loader].

% Load all style rules
:- [prolog/rules/style/rule1_line_length].
:- [prolog/rules/style/rule2_docstring].
:- [prolog/rules/style/rule3_naming].
:- [prolog/rules/style/rule4_type_hints].
:- [prolog/rules/style/rule5_unused_imports].
:- [prolog/rules/style/rule6_todo].
:- [prolog/rules/style/rule7_magic_numbers].

% Load all security rules
:- [prolog/rules/security/rule15_eval].
:- [prolog/rules/security/rule16_pickle].
:- [prolog/rules/security/rule17_shell_injection].
:- [prolog/rules/security/rule18_creds].
:- [prolog/rules/security/rule19_name_main].
:- [prolog/rules/security/rule20_exception].

test_all :-
    format('=== PERSON 1: STYLE & SECURITY RULES TEST ===~n~n', []),
    
    test_file('python_examples/bad_examples.py'),
    test_file('python_examples/rule_tests/security_tests.py').

test_file(File) :-
    format('~n========== Testing: ~w ==========~n~n', [File]),
    
    % Style Rules
    format('STYLE RULES:~n', []),
    run_rule(check_line_length, 'Line Length', File),
    run_rule(check_docstrings, 'Docstrings', File),
    run_rule(check_naming, 'Naming', File),
    run_rule(check_type_hints, 'Type Hints', File),
    run_rule(check_unused_imports, 'Unused Imports', File),
    run_rule(check_todo_comments, 'TODO Comments', File),
    run_rule(check_magic_numbers, 'Magic Numbers', File),
    
    format('~nSECURITY RULES:~n', []),
    % Security Rules
    run_rule(check_eval, 'eval()', File),
    run_rule(check_pickle, 'pickle', File),
    run_rule(check_shell_injection, 'Shell Injection', File),
    run_rule(check_hardcoded_creds, 'Credentials', File),
    run_rule(check_name_main, 'name == main', File),
    run_rule(check_bare_except, 'Bare Except', File),
    
    format('~n').

run_rule(Predicate, RuleName, File) :-
    (call(Predicate, File, Violations) ->
        (Violations = [] ->
            true
        ;
            format('  [~w]: ', [RuleName]),
            length(Violations, Count),
            format('~w violations~n', [Count])
        )
    ;
        format('  [~w]: ERROR~n', [RuleName])
    ).

% Run tests
:- initialization(test_all, main).