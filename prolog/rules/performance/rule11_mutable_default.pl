:- module(rule11_mutable_default, [check_mutable_default/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

check_mutable_default(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule11, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-LineText, Lines),
            is_mutable_default(LineText),

            rule(rule11, Category, Severity, _),
            explanation(rule11, Suggestion),
            format(atom(Message), '~w', [LineText])
        ),
        Violations
    ).

is_mutable_default(Line) :-
    sub_string(Line, _, _, _, "def "),
    (
        sub_string(Line, _, _, _, "=[]")
    ;   sub_string(Line, _, _, _, "={}")
    ;   sub_string(Line, _, _, _, "=set()")
    ).