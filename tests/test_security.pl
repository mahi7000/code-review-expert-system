:- use_module(prolog/utils/file_loader).

:- use_module(prolog/rules/security/rule15_eval).
:- use_module(prolog/rules/security/rule16_pickle).
:- use_module(prolog/rules/security/rule17_shell_injection).
:- use_module(prolog/rules/security/rule18_creds).
:- use_module(prolog/rules/security/rule19_name_main).
:- use_module(prolog/rules/security/rule20_exception).

test_all :-
    File = 'python_examples/rule_tests/security_tests.py',

    format('Testing all SECURITY rules on ~w~n~n', [File]),

    test_rule(check_eval, 'eval() usage', File),
    test_rule(check_pickle, 'pickle.load() usage', File),
    test_rule(check_shell_injection, 'Shell injection risk', File),
    test_rule(check_hardcoded_creds, 'Hardcoded credentials', File),
    test_rule(check_name_main, 'Missing __main__ guard', File),
    test_rule(check_bare_except, 'Bare except clauses', File).

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
