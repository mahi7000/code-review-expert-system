% Rule 16: Check for pickle.load() usage - Security Risk!
:- module(rule16_pickle, [check_pickle/2]).

check_pickle(File, Violations) :-
    read_file_lines(File, Lines),
    findall(violation(pickle_usage, LineNum, Message),
            (nth1(LineNum, Lines, Line),
             (contains(Line, 'pickle.load(');
              contains(Line, '.loads(')),
             contains(Line, 'pickle'),
             format(string(Message), 'pickle.load() found - can execute arbitrary code!', [])),
            Violations).