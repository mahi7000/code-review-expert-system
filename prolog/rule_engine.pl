:- module(rule_engine, [run_all_rules/2]).

:- use_module(rules/performance/rule8_range_len).
:- use_module(rules/performance/rule9_string_concat).
:- use_module(rules/performance/rule10_type_vs_isinstance).
:- use_module(rules/performance/rule11_mutable_default).
:- use_module(rules/performance/rule12_list_comprehension).
:- use_module(rules/performance/rule13_open_without_with).
:- use_module(rules/performance/rule14_empty_except).

run_all_rules(File, AllViolations) :-
    check_range_len(File, V8),
    check_string_concat(File, V9),
    check_type_vs_isinstance(File, V10),
    check_mutable_default(File, V11),
    check_list_comprehension(File, V12),
    check_open_without_with(File, V13),
    check_empty_except(File, V14),

    append(
        [V8, V9, V10, V11, V12, V13, V14],
        AllViolations
    ).