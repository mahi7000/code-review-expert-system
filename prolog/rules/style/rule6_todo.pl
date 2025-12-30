% Rule 6: Check for TODO comments
:- module(rule6_todo, [check_todo_comments/2]).

check_todo_comments(File, Violations) :-
    read_file_lines(File, Lines),
    findall(violation(todo_comment, LineNum, Message),
            (nth1(LineNum, Lines, Line),
             (contains(Line, 'TODO') ; contains(Line, 'FIXME')),
             contains(Line, '#'),
             format(string(Message), 'TODO/FIXME comment found', [])),
            Violations).