:- module(rule12_list_comprehension, [check_list_comprehension/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

check_list_comprehension(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule12, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-LineText, Lines),
            sub_string(LineText, _, _, _, ".append("),

            rule(rule12, Category, Severity, _),
            explanation(rule12, Suggestion),
            format(atom(Message), '~w', [LineText])
        ),
        Violations
    ).
