% Rule 20: Check for bare except statements


check_bare_except(File, Violations) :-
    read_file_lines(File, Lines),
    findall(violation(bare_except, LineNum, Message),
            (nth1(LineNum, Lines, Line),
             trim_string(Line, TrimmedLine),
             string_concat('except:', _, TrimmedLine),
             format(string(Message), 'Bare except clause - catches all exceptions including KeyboardInterrupt!', [])),
            Violations).