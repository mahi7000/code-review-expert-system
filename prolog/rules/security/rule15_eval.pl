% Rule 15: Check for eval() usage - Security Risk!

check_eval(File, Violations) :-
    read_file_lines(File, Lines),
    findall(violation(eval_usage, LineNum, Message),
            (nth1(LineNum, Lines, Line),
             contains(Line, 'eval('),
             format(string(Message), 'eval() found - security risk! Use ast.literal_eval() instead', [])),
            Violations).