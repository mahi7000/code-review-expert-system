:- module(rule17_shell_injection, [check_shell_injection/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

dangerous_shell_call(Line) :-
    member(Func, [
        "os.system(",
        "os.popen(",
        "subprocess.call(",
        "subprocess.Popen("
    ]),
    sub_string(Line, _, _, _, Func).

check_shell_injection(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule17, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-LineText, Lines),
            dangerous_shell_call(LineText),
            sub_string(LineText, _, _, _, "+"),

            rule(rule17, Category, Severity, _),
            explanation(rule17, Suggestion),
            format(atom(Message), '~w', [LineText])
        ),
        Violations
    ).
