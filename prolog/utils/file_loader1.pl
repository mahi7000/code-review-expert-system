:- module(file_loader, [
    load_python_file/1,
    line/2,
    read_file_lines/2, 
    contains/2,
    trim_string/2
]).

:- dynamic line/2.

% Load file into dynamic database
load_python_file(File) :-
    retractall(line(_, _)),
    open(File, read, Stream),
    read_lines_to_db(Stream, 1),
    close(Stream).

read_lines_to_db(Stream, LineNum) :-
    read_line_to_string(Stream, Line),
    ( Line == end_of_file ->
        true
    ;
        assertz(line(LineNum, Line)),
        Next is LineNum + 1,
        read_lines_to_db(Stream, Next)
    ).

% Read file as list of lines
read_file_lines(File, Lines) :-
    setup_call_cleanup(
        open(File, read, Stream),
        read_lines_to_list(Stream, Lines),
        close(Stream)
    ).

read_lines_to_list(Stream, []) :-
    at_end_of_stream(Stream), !.
read_lines_to_list(Stream, [Line|Lines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    read_lines_to_list(Stream, Lines).


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


% Helper predicate if Person 1 needs to use line/2:
% Converts their list to dynamic facts temporarily
with_file_lines(File, Goal) :-
    read_file_lines(File, Lines),
    setup_call_cleanup(
        assert_lines(Lines, 1),
        call(Goal),
        retractall(line(_, _))
    ).

assert_lines([], _).
assert_lines([Line|Lines], N) :-
    assertz(line(N, Line)),
    Next is N + 1,
    assert_lines(Lines, Next).