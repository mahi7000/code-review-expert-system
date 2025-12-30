% Rule 3: Check for non-PEP8 naming conventions
:- module(rule3_naming, [check_naming/2]).

check_naming(File, Violations) :-
    read_file_lines(File, Lines),
    findall(violation(naming_convention, LineNum, Message),
            check_line_for_naming(Lines, LineNum, Message),
            Violations).

check_line_for_naming(Lines, LineNum, Message) :-
    nth1(LineNum, Lines, Line),
    trim_string(Line, TrimmedLine),
    (check_function_naming(TrimmedLine, Message)
    ; check_class_naming(TrimmedLine, Message)
    ; check_variable_naming(TrimmedLine, Message)
    ).

check_function_naming(Line, Message) :-
    string_concat('def ', Rest, Line),
    split_string(Rest, '(', '', [FuncName|_]),
    \+ is_snake_case(FuncName),
    % Skip ALL magic methods (start and end with __)
    \+ (sub_string(FuncName, 0, 2, _, '__'),
        sub_string(FuncName, _, 2, 0, '__')),
    format(string(Message), 'Function "~w" should be snake_case', [FuncName]).
check_class_naming(Line, Message) :-
    string_concat('class ', Rest, Line),
    split_string(Rest, '(:', '', [ClassName|_]),
    \+ is_camel_case(ClassName),
    format(string(Message), 'Class "~w" should be CamelCase', [ClassName]).

check_variable_naming(Line, Message) :-
    % Only check actual variable assignments (not comments, not function definitions)
    \+ contains(Line, '#'),
    \+ string_concat('def ', _, Line),
    \+ string_concat('class ', _, Line),
    \+ string_concat('import ', _, Line),
    \+ string_concat('from ', _, Line),
    split_string(Line, '=', '', Parts),
    length(Parts, Length),
    Length >= 2,  % Has at least one = sign
    nth0(0, Parts, Left),
    trim_string(Left, VarName),
    VarName \= '',
    \+ is_snake_case(VarName),
    \+ member(VarName, ['i', 'j', 'k', 'x', 'y', 'z']),
    format(string(Message), 'Variable "~w" should be snake_case', [VarName]).
is_snake_case(String) :-
    string_lower(String, Lower),
    String == Lower,
    \+ contains(String, '__'),
    \+ sub_string(String, _, 1, 0, '_'),
    \+ sub_string(String, 0, 1, _, '_').

is_camel_case(String) :-
    sub_string(String, 0, 1, _, FirstChar),
    string_upper(FirstChar, Upper),
    FirstChar == Upper,
    \+ contains(String, '_').