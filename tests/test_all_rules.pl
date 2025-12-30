:- use_module(prolog/utils/file_loader).
:- use_module(prolog/rule_engine).

:- initialization(main, main).

main :-
    banner,
    test_file('python_examples/rule_tests/performance_tests.py'),
    nl,
    test_file('python_examples/good_examples.py'),
    nl,
    summary,
    halt.

banner :-
    nl,
    write('============================================'), nl,
    write('     CODE REVIEW EXPERT SYSTEM TEST'), nl,
    write('============================================'), nl, nl.

test_file(File) :-
    format('Testing file: ~w~n', [File]),
    retractall(violation(_,_,_)),
    load_python_file(File),
    set_current_file(File),
    run_all_rules,
    show_results.

show_results :-
    nl,
    forall(
        rule_desc(Rule, Desc),
        show_rule(Rule, Desc)
    ).

show_rule(Rule, Desc) :-
    findall(Line-Msg, violation(Rule, Line, Msg), V),
    format('~n~w~n', [Desc]),
    (   V == []
    ->  write('  No violations')
    ;   forall(
            member(Line-Msg, V),
            format('  Line ~d: ~w~n', [Line, Msg])
        )
    ).

summary :-
    findall(_, violation(_,_,_), All),
    length(All, Count),
    nl,
    format('TOTAL VIOLATIONS: ~d~n', [Count]).

rule_desc(rule8,  'range(len()) usage').
rule_desc(rule9,  'String concat in loops').
rule_desc(rule10, 'type() vs isinstance()').
rule_desc(rule11, 'Mutable default args').
rule_desc(rule12, 'List built via for-loop').
rule_desc(rule13, 'open() without with').
rule_desc(rule14, 'Empty except').