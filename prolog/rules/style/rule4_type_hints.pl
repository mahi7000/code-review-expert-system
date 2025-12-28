% Rule 4: Check for missing type hints in functions

check_type_hints(File, Violations) :-
    read_file_lines(File, Lines),
    findall(violation(missing_type_hint, LineNum, Message),
            (find_function_without_type_hints(Lines, LineNum, Name),
             format(string(Message), 'Function "~w" missing type hints', 
                    [Name])),
            Violations).

find_function_without_type_hints(Lines, LineNum, Name) :-
    nth1(LineNum, Lines, Line),
    trim_string(Line, TrimmedLine),
    string_concat('def ', Rest, TrimmedLine),
    split_string(Rest, '(', '', [Name|_]),
    % Check if function has type hints
    \+ contains(Line, '->'),
    \+ contains(Line, ': int'),
    \+ contains(Line, ': str'),
    \+ contains(Line, ': float'),
    \+ contains(Line, ': bool'),
    \+ contains(Line, ': list'),
    \+ contains(Line, ': dict'),
    \+ contains(Line, ': tuple'),
    % Skip if it's a magic method
    \+ is_magic_method(Name).

is_magic_method(Name) :-
    sub_string(Name, 0, 2, _, '__'),
    sub_string(Name, _, 2, 0, '__').