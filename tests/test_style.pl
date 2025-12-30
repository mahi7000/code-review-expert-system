
:- [prolog/utils/file_loader1].
:- [prolog/rules/style/rule1_line_length].
:- [prolog/rules/style/rule2_docstring].
:- [prolog/rules/style/rule3_naming].
:- [prolog/rules/style/rule4_type_hints].
:- [prolog/rules/style/rule5_unused_imports].
:- [prolog/rules/style/rule6_todo].
:- [prolog/rules/style/rule7_magic_numbers].

test_all :-
    File = 'python_examples/bad_examples.py',
    
    format('Testing all style rules on ~w~n~n', [File]),
    
    test_rule(check_line_length, 'Line Length Check', File),
    test_rule(check_docstrings, 'Missing Docstrings', File),
    test_rule(check_naming, 'Naming Conventions', File),
    test_rule(check_type_hints, 'Missing Type Hints', File),
    test_rule(check_unused_imports, 'Unused Imports', File),
    test_rule(check_todo_comments, 'TODO Comments', File),
    test_rule(check_magic_numbers, 'Magic Numbers', File).

test_rule(Predicate, RuleName, File) :-
    format('=== ~w ===~n', [RuleName]),
    (call(Predicate, File, Violations) ->
        (Violations = [] ->
            format('  No violations found.~n~n', [])
        ;
            forall(member(V, Violations),
                   (V = violation(_, Line, Msg),
                    format('  Line ~w: ~w~n', [Line, Msg]))),
            format('~n', [])
        )
    ;
        format('  ERROR: Rule failed to run.~n~n', [])
    ).

% Run tests
:- initialization(test_all, main).