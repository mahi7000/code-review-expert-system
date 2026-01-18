:- module(rule3_naming, [check_naming/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

snake_case(Name) :-
    string_lower(Name, Name),
    \+ sub_string(Name, _, _, _, "__"),
    \+ sub_string(Name, 0, 1, _, "_"),
    \+ sub_string(Name, _, 1, 0, "_").

camel_case(Name) :-
    sub_string(Name, 0, 1, _, C),
    string_upper(C, C),
    \+ sub_string(Name, _, _, _, "_").

check_naming(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule3, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-Line, Lines),

            (
                ( sub_string(Line, 0, _, _, "def "),
                  split_string(Line, " (", "", [_,""|_]),
                  split_string(Line, " (", "", [_|[Name|_]]),
                  \+ snake_case(Name),
                  Message = "Function name should be snake_case"
                )
              ; ( sub_string(Line, 0, _, _, "class "),
                  split_string(Line, " (:", "", [_|[Name|_]]),
                  \+ camel_case(Name),
                  Message = "Class name should be CamelCase"
                )
            ),

            rule(rule3, Category, Severity, _),
            explanation(rule3, Suggestion)
        ),
        Violations
    ).
