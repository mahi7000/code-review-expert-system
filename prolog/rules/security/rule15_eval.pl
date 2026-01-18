:- module(rule15_eval, [check_eval/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

check_eval(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule15, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-LineText, Lines),
            sub_string(LineText, _, _, _, "eval("),

            rule(rule15, Category, Severity, _),
            explanation(rule15, Suggestion),
            format(atom(Message), '~w', [LineText])
        ),
        Violations
    ).
