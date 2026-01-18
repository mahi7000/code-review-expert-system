:- use_module(prolog/rule_engine).

:- initialization(main, main).

main :-
    banner,
    test_file('python_examples/rule_tests/performance_tests.py'),
    nl,
    test_file('python_examples/good_examples.py'),
    nl,
    halt.

banner :-
    nl,
    write('============================================'), nl,
    write('     CODE REVIEW EXPERT SYSTEM TEST'), nl,
    write('============================================'), nl, nl.

test_file(File) :-
    format('Testing file: ~w~n', [File]),
    run_all_rules(File, Violations),
    show_results(Violations),
    summary(Violations).

show_results(Violations) :-
    nl,
    forall(
        rule_desc(Rule, Desc),
        show_rule(Rule, Desc, Violations)
    ).

show_rule(Rule, Desc, Violations) :-
    findall(
        Line-Msg,
        member(violation(Rule, Line, Msg, _, _, _), Violations),
        Found
    ),
    format('~n~w~n', [Desc]),
    (   Found == []
    ->  write('  No violations'), nl
    ;   forall(
            member(Line-Msg, Found),
            format('  Line ~d: ~w~n', [Line, Msg])
        )
    ).

summary(Violations) :-
    length(Violations, Count),
    nl,
    format('TOTAL VIOLATIONS: ~d~n', [Count]).

% === Rule descriptions ===
rule_desc(rule8,  'range(len()) usage').
rule_desc(rule9,  'String concat in loops').
rule_desc(rule10, 'type() vs isinstance()').
rule_desc(rule11, 'Mutable default args').
rule_desc(rule12, 'List built via for-loop').
rule_desc(rule13, 'open() without with').
rule_desc(rule14, 'Empty except').
