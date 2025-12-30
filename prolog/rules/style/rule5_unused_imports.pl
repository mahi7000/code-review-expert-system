% Rule 5: Check for unused imports
:- module(rule5_unused_imports, [check_unused_imports/2]).

check_unused_imports(File, Violations) :-
    read_file_lines(File, Lines),
    findall(violation(unused_import, LineNum, Message),
            (nth1(LineNum, Lines, Line),
             trim_string(Line, TrimmedLine),
                        ((string_concat('import ', Module, TrimmedLine)) ;
                         (string_concat('from ', Rest, TrimmedLine),
                            split_string(Rest, ' ', '', [Module|_]))),
             \+ is_import_used(Lines, Module, LineNum),
             format(string(Message), 'Unused import: ~w', [TrimmedLine])),
            Violations).

is_import_used(Lines, Module, ImportLine) :-
    find_module_name(Module, ModuleName),
    nth1(OtherLine, Lines, OtherLineContent),
    OtherLine \= ImportLine,
    contains(OtherLineContent, ModuleName).

find_module_name(Module, ModuleName) :-
    split_string(Module, ' ', '', Parts),
    (Parts = [First|_] -> 
        split_string(First, '.', '', [ModuleName|_])
    ;
        ModuleName = Module
    ).