% Rule 19: Check for missing if __name__ == "__main__"

check_name_main(File, Violations) :-
    read_file_lines(File, Lines),
    findall(violation(missing_name_main, LineNum, Message),
            (has_top_level_code(Lines, LineNum),
             \+ has_name_main_guard(Lines),
             format(string(Message), 'Top-level code without if __name__ == "__main__" guard', [])),
            Violations).

has_top_level_code(Lines, LineNum) :-
    nth1(LineNum, Lines, Line),
    trim_string(Line, TrimmedLine),
    \+ string_concat('def ', _, TrimmedLine),
    \+ string_concat('class ', _, TrimmedLine),
    \+ string_concat('import ', _, TrimmedLine),
    \+ string_concat('from ', _, TrimmedLine),
    \+ contains(TrimmedLine, '#'),
    TrimmedLine \= '',
    \+ contains(TrimmedLine, 'if __name__'),
    LineNum > 5.  % Skip shebang and imports at top

has_name_main_guard(Lines) :-
    member(Line, Lines),
    contains(Line, '__name__'),
    contains(Line, '__main__').