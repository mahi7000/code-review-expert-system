:- module(rule19_name_main, [check_name_main/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

has_name_main_guard(Lines) :-
    member(_-Line, Lines),
    sub_string(Line, _, _, _, "__name__"),
    sub_string(Line, _, _, _, "__main__").

top_level_code(Line) :-
    Line \= "",
    \+ sub_string(Line, 0, _, _, "def "),
    \+ sub_string(Line, 0, _, _, "class "),
    \+ sub_string(Line, 0, _, _, "import "),
    \+ sub_string(Line, 0, _, _, "from "),
    \+ sub_string(Line, _, _, _, "#").

check_name_main(File, Violations) :-
    load_lines(File, Lines),
    \+ has_name_main_guard(Lines),
    findall(
        violation(rule19, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-LineText, Lines),
            top_level_code(LineText),
            LineNum > 5,

            rule(rule19, Category, Severity, _),
            explanation(rule19, Suggestion),
            format(atom(Message), '~w', [LineText])
        ),
        Violations
    ).
