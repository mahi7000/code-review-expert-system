:- module(rule5_unused_imports, [check_unused_imports/2]).

:- use_module(prolog/knowledge_base).
:- use_module(prolog/utils/file_loader).

import_line(Line, Module) :-
    (   sub_string(Line, 0, _, _, "import "),
        split_string(Line, " ", "", [_|[Module|_]])
    ;   sub_string(Line, 0, _, _, "from "),
        split_string(Line, " ", "", [_|[Module|_]])
    ).

used_elsewhere(Lines, Module, ImportLine) :-
    member(LineNum-Line, Lines),
    LineNum \= ImportLine,
    sub_string(Line, _, _, _, Module).

check_unused_imports(File, Violations) :-
    load_lines(File, Lines),
    findall(
        violation(rule5, LineNum, Message, Category, Severity, Suggestion),
        (
            member(LineNum-Line, Lines),
            import_line(Line, Module),
            \+ used_elsewhere(Lines, Module, LineNum),

            rule(rule5, Category, Severity, _),
            explanation(rule5, Suggestion),
            format(atom(Message), 'Unused import: ~w', [Module])
        ),
        Violations
    ).
