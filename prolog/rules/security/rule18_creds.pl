% Rule 18: Check for hardcoded credentials

check_hardcoded_creds(File, Violations) :-
    read_file_lines(File, Lines),
    findall(violation(hardcoded_creds, LineNum, Message),
            (nth1(LineNum, Lines, Line),
             contains(Line, '='),
             contains_credential(Line),
             format(string(Message), 'Hardcoded credential found', [])),
            Violations).

contains_credential(Line) :-
    trim_string(Line, TrimmedLine),
    split_string(TrimmedLine, '=', '', [Left, Right]),
    trim_string(Left, Key),
    trim_string(Right, Value),
    (contains(Key, 'password') ;
     contains(Key, 'passwd') ;
     contains(Key, 'secret') ;
     contains(Key, 'key') ;
     contains(Key, 'token') ;
     contains(Key, 'auth')),
    \+ contains(Value, 'os.getenv('),
    \+ contains(Value, 'environ.get('),
    \+ contains(Value, 'input('),
    Value \= '""',
    Value \= "''".