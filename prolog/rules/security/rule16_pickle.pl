:- module(rule16_pickle, [check_pickle/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

check_pickle(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule16, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-LineText, Lines),

            sub_string(LineText, _, _, _, "pickle"),
            (   sub_string(LineText, _, _, _, "load(")
            ;   sub_string(LineText, _, _, _, "loads(")
            ),

            rule(rule16, Category, Severity, _),
            explanation(rule16, Suggestion),
            format(atom(Message), '~w', [LineText])
        ),
        Violations
    ).
