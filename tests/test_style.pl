:- use_module(prolog/utils/file_loader).

:- use_module(prolog/rules/style/rule1_line_length).
:- use_module(prolog/rules/style/rule2_docstring).
:- use_module(prolog/rules/style/rule3_naming).
:- use_module(prolog/rules/style/rule4_type_hints).
:- use_module(prolog/rules/style/rule5_unused_imports).
:- use_module(prolog/rules/style/rule6_todo).
:- use_module(prolog/rules/style/rule7_magic_numbers).

test_all :-
    File = 'python_examples/bad_examples.py',

    format('Testing all STYLE rules on ~w~n~n', [File]),

    test_rule(check_line_length, 'Line length', File),
    test_rule(check_docstrings, 'Missing docstrings', File),
    test_rule(check_naming, 'Naming conventions', File),
    test_rule(check_type_hints, 'Missing type hints', File),
    test_rule(check_unused_imports, 'Unused imports', File),
    test_rule(check_todo_comments, 'TODO / FIXME comments', File),
    test_rule(check_magic_numbers, 'Magic numbers', File).

test_rule(Predicate, RuleName, File) :-
    format('=== ~w ===~n', [RuleName]),
    call(Predicate, File, Violations),
    (   Violations == []
    ->  format('  No violations found.~n~n', [])
    ;   forall(
            member(
                violation(Rule, Line, Message, Category, Severity, _),
                Violations
            ),
            format(
                '  [~w|~w] Line ~w: ~w~n',
                [Category, Severity, Line, Message]
            )
        ),
        nl
    ).

:- initialization(test_all, main).
