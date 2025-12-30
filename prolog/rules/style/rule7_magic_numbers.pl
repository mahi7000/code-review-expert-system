% Rule 7: Check for magic numbers
:- module(rule7_magic_numbers, [check_magic_numbers/2]).

check_magic_numbers(File, Violations) :-
    read_file_lines(File, Lines),
    findall(violation(magic_number, LineNum, Message),
            (nth1(LineNum, Lines, Line),
             find_magic_number_in_line(Line, MagicNum),
             format(string(Message), 'Magic number: ~w', [MagicNum])),
            Violations).

find_magic_number_in_line(Line, MagicNum) :-
    trim_string(Line, TrimmedLine),
    \+ contains(TrimmedLine, '#'),
    split_string(TrimmedLine, ' ', '', Words),
    check_words_for_magic(Words, MagicNum).

check_words_for_magic([Word|_], MagicNum) :-
    extract_number_from_word(Word, Num),
    \+ is_common_number(Num),
    MagicNum = Num.
check_words_for_magic([_|Rest], MagicNum) :-
    check_words_for_magic(Rest, MagicNum).

extract_number_from_word(Word, Num) :-
    string_chars(Word, Chars),
    extract_digits(Chars, DigitChars),
    DigitChars \= [],
    number_chars(Num, DigitChars),
    integer(Num),
    Num > 10.

extract_digits([], []).
extract_digits([H|T], [H|Rest]) :-
    char_type(H, digit),
    extract_digits(T, Rest).
extract_digits([H|T], Rest) :-
    \+ char_type(H, digit),
    extract_digits(T, Rest).

is_common_number(0).
is_common_number(1).
is_common_number(2).
is_common_number(10).
is_common_number(100).
is_common_number(1000).