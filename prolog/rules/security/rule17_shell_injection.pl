
check_shell_injection(File, Violations) :-
    read_file_lines(File, Lines),
    findall(violation(shell_injection, LineNum, Message),
            (nth1(LineNum, Lines, Line),
             check_shell_function(Line, Function),
             contains(Line, '+'),  % String concatenation with variables
             format(string(Message), '~w with user input - shell injection risk! Use subprocess.run() with args list', [Function])),
            Violations).

check_shell_function(Line, Function) :-
    member(Function, ['os.system(', 'os.popen(', 'subprocess.call(', 'subprocess.Popen(']),
    contains(Line, Function).