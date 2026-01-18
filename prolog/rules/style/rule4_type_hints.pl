:- module(rule4_type_hints, [check_type_hints/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

check_type_hints(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule4, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-Line, Lines),
            sub_string(Line, 0, _, _, "def "),
            \+ sub_string(Line, _, _, _, "->"),
            \+ sub_string(Line, _, _, _, ":"),

            rule(rule4, Category, Severity, _),
            explanation(rule4, Suggestion),
            Message = "Public function missing type hints"
        ),
        Violations
    ).
