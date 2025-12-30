% Rule 15: Check for eval() usage - Security Risk!
:- module(rule15_eval, [check_eval/2]).

check_eval(File, Violations) :-
    read_file_lines(File, Lines),
    findall(violation(eval_usage, LineNum, Message),
            (nth1(LineNum, Lines, Line),
             contains(Line, 'eval('),
             format(string(Message), 'eval() found - security risk! Use ast.literal_eval() instead', [])),
            Violations).