:- module(rule14_empty_except, [check_empty_except/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

check_empty_except(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule14, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-LineText, Lines),
            sub_string(LineText, _, _, _, "except:"),

            rule(rule14, Category, Severity, _),
            explanation(rule14, Suggestion),
            format(atom(Message), '~w', [LineText])
        ),
        Violations
    ).