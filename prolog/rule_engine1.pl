# :- module(rule_engine, [
#     run_all_rules/0,
#     assert_violation/3,
#     violation/3,
#     set_current_file/1,
#     current_file/1
# ]).

# :- dynamic violation/3.
# :- dynamic current_file/1.

# % =========================================================
# % STATE
# % =========================================================

# set_current_file(File) :-
#     retractall(current_file(_)),
#     assertz(current_file(File)).

# assert_violation(Rule, Line, Message) :-
#     assertz(violation(Rule, Line, Message)).

# % =========================================================
# % ENTRY POINT
# % =========================================================

# run_all_rules :-
#     retractall(violation(_,_,_)),
#     run_performance_rules,
#     run_style_rules,
#     run_security_rules.

# % =========================================================
# % PERFORMANCE RULES (MODULE BASED, DISTINCT PREDICATES)
# % =========================================================

# run_performance_rules :-
#     write('Running performance rules...'), nl,
#     forall(
#         performance_rule(Rule, Module, Predicate, Path),
#         run_performance_rule(Rule, Module, Predicate, Path)
#     ).

# run_performance_rule(Rule, Module, Predicate, Path) :-
#     (   exists_file(Path)
#     ->  ensure_loaded(Path),
#         Goal =.. [Predicate],
#         call(Module:Goal),
#         report(Rule)
#     ;   format('  ✗ ~w (file not found)~n', [Rule])
#     ).

# % rule(RuleId, Module, Predicate, FilePath)
# performance_rule(rule8,  rule8_range_len,           check_range_len,
#                  'prolog/rules/performance/rule8_range_len.pl').

# performance_rule(rule9,  rule9_string_concat,       check_string_concat,
#                  'prolog/rules/performance/rule9_string_concat.pl').

# performance_rule(rule10, rule10_type_vs_isinstance, check_type_vs_isinstance,
#                  'prolog/rules/performance/rule10_type_vs_isinstance.pl').

# performance_rule(rule11, rule11_mutable_default,    check_mutable_default,
#                  'prolog/rules/performance/rule11_mutable_default.pl').

# performance_rule(rule12, rule12_list_comprehension, check_list_comprehension,
#                  'prolog/rules/performance/rule12_list_comprehension.pl').

# performance_rule(rule13, rule13_open_without_with,  check_open_without_with,
#                  'prolog/rules/performance/rule13_open_without_with.pl').

# performance_rule(rule14, rule14_empty_except,       check_empty_except,
#                  'prolog/rules/performance/rule14_empty_except.pl').

# % =========================================================
# % STYLE RULES (PLAIN FILES)
# % =========================================================

# run_style_rules :-
#     current_file(File),
#     write('Running style rules...'), nl,
#     forall(
#         style_rule(Rule, Predicate, Path),
#         run_plain_rule(Rule, Predicate, Path, File)
#     ).

# style_rule(rule1, check_line_length,     'prolog/rules/style/rule1_line_length.pl').
# style_rule(rule2, check_docstrings,      'prolog/rules/style/rule2_docstring.pl').
# style_rule(rule3, check_naming,          'prolog/rules/style/rule3_naming.pl').
# style_rule(rule4, check_type_hints,      'prolog/rules/style/rule4_type_hints.pl').
# style_rule(rule5, check_unused_imports,  'prolog/rules/style/rule5_unused_imports.pl').
# style_rule(rule6, check_todo_comments,   'prolog/rules/style/rule6_todo.pl').
# style_rule(rule7, check_magic_numbers,   'prolog/rules/style/rule7_magic_numbers.pl').

# % =========================================================
# % SECURITY RULES (PLAIN FILES)
# % =========================================================

# run_security_rules :-
#     current_file(File),
#     write('Running security rules...'), nl,
#     forall(
#         security_rule(Rule, Predicate, Path),
#         run_plain_rule(Rule, Predicate, Path, File)
#     ).

# security_rule(rule15, check_eval,            'prolog/rules/security/rule15_eval.pl').
# security_rule(rule16, check_pickle,          'prolog/rules/security/rule16_pickle.pl').
# security_rule(rule17, check_shell_injection, 'prolog/rules/security/rule17_shell_injection.pl').
# security_rule(rule18, check_hardcoded_creds, 'prolog/rules/security/rule18_hardcoded_creds.pl').
# security_rule(rule19, check_name_main,       'prolog/rules/security/rule19_name_main.pl').
# security_rule(rule20, check_bare_except,     'prolog/rules/security/rule20_exception.pl').

# % =========================================================
# % COMMON EXECUTION FOR NON-MODULE RULES
# % =========================================================

# run_plain_rule(Rule, Predicate, Path, File) :-
#     (   exists_file(Path)
#     ->  consult(Path),
#         Goal =.. [Predicate, File, Violations],
#         (   call(Goal),
#             Violations \= []
#         ->  add_violations(Rule, Violations)
#         ;   true
#         ),
#         report(Rule)
#     ;   format('  ✗ ~w (file not found)~n', [Rule])
#     ).

# add_violations(_, []).
# add_violations(Rule, [violation(_, Line, Msg)|Rest]) :-
#     assert_violation(Rule, Line, Msg),
#     add_violations(Rule, Rest).

# % =========================================================
# % REPORTING
# % =========================================================

# report(Rule) :-
#     findall(_, violation(Rule, _, _), Violations),
#     length(Violations, Count),
#     (   Count > 0
#     ->  format('  ✓ ~w: ~d violation(s)~n', [Rule, Count])
#     ;   format('  ✓ ~w: no violations~n', [Rule])
#     ).

:- module(rule_engine, [run_all_rules/2]).
:- use_module(rules/performance/rule8_range_len).
:- use_module(rules/performance/rule9_string_concat).
:- use_module(rules/performance/rule10_type_vs_isinstance).
:- use_module(rules/performance/rule11_mutable_default).
:- use_module(rules/performance/rule12_list_comprehension).
:- use_module(rules/performance/rule14_empty_except).

run_all_rules(File, AllViolations) :-
    check_range_len(File, V8),
    check_string_concat(File, V9),
    check_type_vs_isinstance(File, V10),
    check_mutable_default(File, V11),
    check_list_comprehension(File, V12),
    check_empty_except(File, V14),
    append([V8, V9, V10, V11, V12, V14], AllViolations).