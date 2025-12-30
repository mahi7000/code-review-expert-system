:- module(rule9_string_concat, [check_string_concat/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

check_string_concat(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule9, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-LineText, Lines),

            sub_string(LineText, _, _, _, "+="),
            (sub_string(LineText, _, _, _, "\"") ; sub_string(LineText, _, _, _, "'")),

            rule(rule9, Category, Severity, _),
            explanation(rule9, Suggestion),
            format(atom(Message), '~w', [LineText])
        ),
        Violations
    ).