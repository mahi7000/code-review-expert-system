:- module(rule1_line_length, [check_line_length/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

check_line_length(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule1, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-LineText, Lines),
            string_length(LineText, Length),
            Length > 79,

            rule(rule1, Category, Severity, _),
            explanation(rule1, Suggestion),
            format(atom(Message),
                   'Line ~w has ~w characters (max 79)',
                   [LineNum, Length])
        ),
        Violations
    ).
