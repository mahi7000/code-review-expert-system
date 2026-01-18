:- module(rule7_magic_numbers, [check_magic_numbers/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

magic_number(Line, Num) :-
    split_string(Line, " ", "", Words),
    member(W, Words),
    number_string(Num, W),
    Num > 10,
    \+ member(Num, [100, 1000]).

check_magic_numbers(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule7, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-Line, Lines),
            magic_number(Line, Num),

            rule(rule7, Category, Severity, _),
            explanation(rule7, Suggestion),
            format(atom(Message), 'Magic number used: ~w', [Num])
        ),
        Violations
    ).
