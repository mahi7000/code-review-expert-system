:- module(rule2_docstring, [check_docstrings/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

missing_docstring(Lines, LineNum, Name) :-
    member(LineNum-Line, Lines),
    (   sub_string(Line, 0, _, _, "def ")
    ;   sub_string(Line, 0, _, _, "class ")
    ),
    split_string(Line, " (:", "", [_|[Name|_]]),
    Next is LineNum + 1,
    member(Next-NextLine, Lines),
    \+ sub_string(NextLine, _, _, _, "\"\"\"").

check_docstrings(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule2, LineNum, Message, Category, Severity, Suggestion),
        (
            missing_docstring(Lines, LineNum, Name),
            rule(rule2, Category, Severity, _),
            explanation(rule2, Suggestion),
            format(atom(Message),
                   'Function/Class "~w" missing docstring',
                   [Name])
        ),
        Violations
    ).
