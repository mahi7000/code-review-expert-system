:- module(rule10_type_vs_isinstance, [check_type_vs_isinstance/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

check_type_vs_isinstance(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule10, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-LineText, Lines),
            sub_string(LineText, _, _, _, "type("),
            sub_string(LineText, _, _, _, ") =="),

            rule(rule10, Category, Severity, _),
            explanation(rule10, Suggestion),
            format(atom(Message), '~w', [LineText])
        ),
        Violations
    ).