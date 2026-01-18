:- module(rule20_bare_except, [check_bare_except/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

check_bare_except(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule20, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-LineText, Lines),
            sub_string(LineText, 0, _, _, "except:"),

            rule(rule20, Category, Severity, _),
            explanation(rule20, Suggestion),
            format(atom(Message), '~w', [LineText])
        ),
        Violations
    ).
