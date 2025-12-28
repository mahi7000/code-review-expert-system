% Purpose: Read Python files line by line

:- dynamic read_file_lines/2.

read_file_lines(File, Lines) :-
    setup_call_cleanup(
        open(File, read, Stream),
        read_lines(Stream, Lines),
        close(Stream)
    ).

read_lines(Stream, []) :-
    at_end_of_stream(Stream), !.
read_lines(Stream, [Line|Lines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    read_lines(Stream, Lines).

% Helper: Check if string contains substring
contains(Line, Substring) :-
    sub_string(Line, _, _, _, Substring).

% Helper: Remove leading/trailing whitespace
trim_string(String, Trimmed) :-
    string_chars(String, Chars),
    trim(Chars, TrimmedChars),
    string_chars(Trimmed, TrimmedChars).

trim([], []).
trim([H|T], R) :-
    char_type(H, space),
    trim(T, R).
trim([H|T], [H|R]) :-
    \+ char_type(H, space),
    trim_rest(T, R).

trim_rest([], []).
trim_rest([H|T], [H|R]) :-
    char_type(H, space),
    trim_rest(T, R).
trim_rest([H|T], [H|R]) :-
    \+ char_type(H, space),
    trim_rest(T, R).