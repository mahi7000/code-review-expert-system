:- module(main, [analyze/2, analyze_and_print_json/1]).
:- use_module(rule_engine).

% Run all rules
analyze(File, Violations) :-
    run_all_rules(File, Violations).

% JSON for API
analyze_and_print_json(File) :-
    analyze(File, Violations),
    write('['),
    write_violations(Violations),
    write(']'),
    nl.

write_violations([]).
write_violations([V]) :-
    write_violation(V).
write_violations([V|Rest]) :-
    write_violation(V),
    write(','),
    write_violations(Rest).

write_violation(
    violation(Rule, Line, Message, Category, Severity, Suggestion)
) :-
    format(
        '{"rule":"~w","line":~d,"message":"~w","category":"~w","severity":"~w","suggestion":"~w"}',
        [Rule, Line, Message, Category, Severity, Suggestion]
    ).