% Rule 1: Check for lines longer than 79 characters (PEP 8)
:- module(rule1_line_length, [check_line_length/2]).

check_line_length(File, Violations) :-
    read_file_lines(File, Lines),
    findall(violation(line_length, LineNum, Message), 
            (nth1(LineNum, Lines, Line),
             string_length(Line, Length),
             Length > 79,
             format(string(Message), 'Line ~w is ~w characters (max 79)', 
                    [LineNum, Length])),
            Violations).