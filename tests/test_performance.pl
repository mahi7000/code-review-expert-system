% test_performance.pl - Fixed version
% Run with: swipl -s tests/test_performance.pl

:- use_module(prolog/utils/file_loader).
:- use_module(prolog/rule_engine).

% Simple main predicate - only test performance rules
main :-
    format('~n===============================================~n', []),
    format('        PERFORMANCE RULES TEST - DIRECT         ~n', []),
    format('===============================================~n~n', []),
    
    File = 'python_examples/rule_tests/performance_tests.py',
    format('Analyzing file: ~w~n~n', [File]),
    
    % Clear ALL violations first
    retractall(violation(_, _, _)),
    
    % Load the file
    load_python_file(File),
    
    % Run ONLY performance rules (not all rules)
    run_performance_rules,
    
    % Print results
    format('=== PERFORMANCE RULES ANALYSIS ===~n~n', []),
    
    print_rule_results(rule8, 'range(len(...)) pattern'),
    print_rule_results(rule9, 'String concatenation in loops'),
    print_rule_results(rule10, 'type() vs isinstance()'),
    print_rule_results(rule11, 'Mutable default arguments'),
    print_rule_results(rule12, 'List construction with for loop'),
    print_rule_results(rule13, 'open() without with statement'),
    print_rule_results(rule14, 'Empty except clause'),
    
    % Show total count
    findall(_, violation(_, _, _), AllViolations),
    length(AllViolations, Total),
    format('~n===============================================~n', []),
    format('Total performance violations found: ~w~n', [Total]),
    format('===============================================~n', []),
    
    halt.

% Helper to print rule results
print_rule_results(Rule, Desc) :-
    findall(Line-Message, violation(Rule, Line, Message), Violations),
    format('=== ~w ===~n', [Desc]),
    (   Violations \= [] ->
        foreach(member(Line-Message, Violations),
                format('  Line ~d: ~w~n', [Line, Message]))
    ;   format('  No violations found.~n')
    ),
    nl.

:- initialization(main, main).