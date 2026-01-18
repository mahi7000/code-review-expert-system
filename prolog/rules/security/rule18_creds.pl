:- module(rule18_hardcoded_creds, [check_hardcoded_creds/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

credential_keyword(Line) :-
    member(Key, ["password", "passwd", "secret", "token", "key", "auth"]),
    sub_string(Line, _, _, _, Key).

safe_source(Line) :-
    sub_string(Line, _, _, _, "os.getenv(");
    sub_string(Line, _, _, _, "environ.get(");
    sub_string(Line, _, _, _, "input(").

check_hardcoded_creds(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule18, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-LineText, Lines),
            sub_string(LineText, _, _, _, "="),
            credential_keyword(LineText),
            \+ safe_source(LineText),

            rule(rule18, Category, Severity, _),
            explanation(rule18, Suggestion),
            format(atom(Message), '~w', [LineText])
        ),
        Violations
    ).
