:- module(rule_engine, [run_all_rules/2]).

:- use_module(rules/style/rule1_line_length).
:- use_module(rules/style/rule2_docstring).
:- use_module(rules/style/rule3_naming).
:- use_module(rules/style/rule4_type_hints).
:- use_module(rules/style/rule5_unused_imports).
:- use_module(rules/style/rule6_todo).
:- use_module(rules/style/rule7_magic_numbers).

:- use_module(rules/performance/rule8_range_len).
:- use_module(rules/performance/rule9_string_concat).
:- use_module(rules/performance/rule10_type_vs_isinstance).
:- use_module(rules/performance/rule11_mutable_default).
:- use_module(rules/performance/rule12_list_comprehension).
:- use_module(rules/performance/rule13_open_without_with).
:- use_module(rules/performance/rule14_empty_except).

:- use_module(rules/security/rule15_eval).
:- use_module(rules/security/rule16_pickle).
:- use_module(rules/security/rule17_shell_injection).
:- use_module(rules/security/rule18_creds).
:- use_module(rules/security/rule19_name_main).
:- use_module(rules/security/rule20_exception).

run_all_rules(File, AllViolations) :-
    % -------- STYLE --------
    check_line_length(File, V1),
    % check_docstrings(File, V2),
    check_naming(File, V3),
    check_type_hints(File, V4),
    check_unused_imports(File, V5),
    check_todo_comments(File, V6),
    check_magic_numbers(File, V7),

    % -------- PERFORMANCE --------
    check_range_len(File, V8),
    check_string_concat(File, V9),
    check_type_vs_isinstance(File, V10),
    check_mutable_default(File, V11),
    check_list_comprehension(File, V12),
    check_open_without_with(File, V13),
    check_empty_except(File, V14),

    % -------- SECURITY --------
    % check_eval(File, V15),
    check_pickle(File, V16),
    % check_shell_injection(File, V17),
    % check_hardcoded_creds(File, V18),
    % check_name_main(File, V19),
    check_bare_except(File, V20),

    append(
        [
            V1, V2, V3, V4, V5, V6, V7,
            V8, V9, V10, V11, V12, V13, V14,
            V15, V16, V17, V18, V19, V20
        ],
        AllViolations
    ).