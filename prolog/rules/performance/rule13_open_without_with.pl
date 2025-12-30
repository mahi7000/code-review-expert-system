:- module(rule13_open_without_with, [check_open_without_with/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

check_open_without_with(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule13, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-LineText, Lines),
            sub_string(LineText, _, _, _, "open("),
            \+ sub_string(LineText, _, _, _, "with "),

            rule(rule13, Category, Severity, _),
            explanation(rule13, Suggestion),
            format(atom(Message), '~w', [LineText])
        ),
        Violations
    ).
