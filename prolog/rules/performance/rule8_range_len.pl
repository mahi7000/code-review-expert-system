:- module(rule8_range_len, [check_range_len/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

check_range_len(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule8, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-LineText, Lines),
            sub_string(LineText, _, _, _, "range(len("),

            rule(rule8, Category, Severity, _),
            explanation(rule8, Suggestion),
            format(atom(Message), '~w', [LineText])
        ),
        Violations
    ).