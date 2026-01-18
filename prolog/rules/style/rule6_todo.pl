:- module(rule6_todo, [check_todo_comments/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

check_todo_comments(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule6, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-Line, Lines),
            ( sub_string(Line, _, _, _, "TODO")
            ; sub_string(Line, _, _, _, "FIXME")
            ),

            rule(rule6, Category, Severity, _),
            explanation(rule6, Suggestion),
            Message = "TODO/FIXME comment without issue reference"
        ),
        Violations
    ).
