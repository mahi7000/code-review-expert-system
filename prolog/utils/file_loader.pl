:- module(file_loader, [load_lines/2]).

load_lines(File, Lines) :-
    open(File, read, Stream),
    read_lines(Stream, 1, Lines),
    close(Stream).

read_lines(Stream, _, []) :-
    at_end_of_stream(Stream), !.
read_lines(Stream, N, [N-Line|Rest]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    N1 is N + 1,
    read_lines(Stream, N1, Rest).