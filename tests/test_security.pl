:- [prolog/utils/file_loader1].
:- [prolog/rules/security/rule15_eval].
:- [prolog/rules/security/rule16_pickle].
:- [prolog/rules/security/rule17_shell_injection].
:- [prolog/rules/security/rule18_creds].
:- [prolog/rules/security/rule19_name_main].
:- [prolog/rules/security/rule20_exception].

test_all :-
    File = 'python_examples/rule_tests/security_tests.py',
    
    format('Testing all security rules on ~w~n~n', [File]),
    
    test_rule(check_eval, 'eval() Usage', File),
    test_rule(check_pickle, 'pickle.load() Usage', File),
    test_rule(check_shell_injection, 'Shell Injection', File),
    test_rule(check_hardcoded_creds, 'Hardcoded Credentials', File),
    test_rule(check_name_main, 'Missing if __name__ guard', File),
    test_rule(check_bare_except, 'Bare Except Clauses', File).

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