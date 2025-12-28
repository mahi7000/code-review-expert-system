% Rule 2: Check for missing docstrings in functions/classes


check_docstrings(File, Violations) :-
    read_file_lines(File, Lines),
    findall(violation(missing_docstring, LineNum, Message),
            (find_function_without_docstring(Lines, LineNum, Name),
             format(string(Message), 'Function/Class "~w" missing docstring', 
                    [Name])),
            Violations).

find_function_without_docstring(Lines, LineNum, Name) :-
    nth1(LineNum, Lines, Line),
    (string_concat('def ', Rest, Line); string_concat('class ', Rest, Line)),
    split_string(Rest, "(:", "", [Name|_]),
    NextLineNum is LineNum + 1,
    nth1(NextLineNum, Lines, NextLine),
    \+ string_concat('    """', _, NextLine),
    \+ string_concat('\t"""', _, NextLine).