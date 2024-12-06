:- use_module(library(pcre)). 

% extremely useful debugging utility
:- op(920, fy, *).
* _.

remove_accent('ί', 'ι').
remove_accent('Ἰ', 'Ι').
remove_accent('ὶ', 'ι').
remove_accent('ἰ', 'ι').
remove_accent('ῖ', 'ι').
remove_accent('ἶ', 'ι').
remove_accent('ΐ', 'ι').
remove_accent('ἴ', 'ι').
remove_accent('ἱ', 'ι').
remove_accent('ἵ', 'ι').

remove_accent('ό', 'ο').
remove_accent('ὸ', 'ο').
remove_accent('ὁ', 'ο').
remove_accent('ὅ', 'ο').
remove_accent('ὄ', 'ο').

remove_accent('ἐ', 'ε').
remove_accent('ὲ', 'ε').
remove_accent('έ', 'ε').
remove_accent('ἑ', 'ε').
remove_accent('ἕ', 'ε').
remove_accent('ἔ', 'ε').

remove_accent('ώ', 'ω').
remove_accent('ὼ', 'ω').
remove_accent('ῶ', 'ω').
remove_accent('ῷ', 'ω').
remove_accent('ὡ', 'ω').
remove_accent('ὧ', 'ω').
remove_accent('ῴ', 'ω').

remove_accent('ύ', 'υ').
remove_accent('ὺ', 'υ').
remove_accent('ῦ', 'υ').
remove_accent('ὖ', 'υ').
remove_accent('ὐ', 'υ').
remove_accent('ὑ', 'υ').
remove_accent('ὗ', 'υ').
remove_accent('ὕ', 'υ').

remove_accent('ἀ', 'α').
remove_accent('ά', 'α').
remove_accent('ἄ', 'α').
remove_accent('ὰ', 'α').
remove_accent('Ἀ', 'Α').
remove_accent('ἁ', 'α').
remove_accent('ἅ', 'α').
remove_accent('ᾶ', 'α').

remove_accent('ὴ', 'η').
remove_accent('ῆ', 'η').
remove_accent('ή', 'η').
remove_accent('ἡ', 'η').
remove_accent('ἦ', 'η').
remove_accent('ῇ', 'η').
remove_accent('ἠ', 'η').

remove_accent(X, X).

% Remove accents from input string
remove_accents(Input, LowerNoAccents) :-
    atom_chars(Input, CodesIn),
    maplist(remove_accent, CodesIn, CodesOut),
    atom_chars(Output, CodesOut),
    downcase_atom(Output, LowerNoAccents).

markdown_table(Header, Rows, Markdown) :-
    atomic_list_concat(Header, " | ", HeaderRow),
    maplist(row_to_string, Rows, RowStrings),
    atomic_list_concat(RowStrings, '\n', RowsMarkdown),
    length(Header, ColCount),
    findall('---', between(1, ColCount, _), Dashes),
    atomic_list_concat(Dashes, " | ", SeparatorRow),
    atomic_list_concat(["| ", HeaderRow, " |\n| ", SeparatorRow, " |\n", RowsMarkdown], Markdown).

row_to_string(Row, RowString) :-
    atomic_list_concat(Row, " | ", RowData),
    atomic_list_concat(["| ", RowData, " |"], RowString).

zip_lists(L1, L2, ZippedList) :-
    maplist({}/[E1, E2, E3]>>(E3 = [E1, E2]), L1, L2, ZippedList).

sample(N, List, Sample) :-
    permutation(List, PermutedList),
    length(Sample, N),
    append(Sample, _, PermutedList).

is_empty('').
is_empty("").
is_empty(S) :-
    re_match("^\\s+$", S).

strip(S, S1) :-
    normalize_space(string(S1), S).

empty_markdown_row(MarkdownRow) :-
    atomics_to_string(Pieces, '|', MarkdownRow),
    exclude(is_empty, Pieces, []).

markdown_row_to_list(MarkdownRow, List) :-
    atomics_to_string(Pieces, '|', MarkdownRow),
    Pieces = [_ | RestPieces],
    reverse(RestPieces, [_ | RevPieces]),
    reverse(RevPieces, Bits),
    maplist(strip, Bits, List).

markdown_table_to_lists(MarkdownTable, Lists) :-
    % Split the input into lines
    atomics_to_string(AllLines, '\n', MarkdownTable),
    % Remove any empty or whitespace-only lines
    exclude(is_empty, AllLines, NonEmptyLines),

    % Expect at least a header row and a separator row
    (   NonEmptyLines = [HeaderLine, SeparatorLine | DataLines]
    ->  % Filter out empty or invalid rows among the data lines
        exclude(empty_markdown_row, DataLines, ValidDataLines),
        % Convert each valid data line into a list of cells
        maplist(markdown_row_to_list, ValidDataLines, Lists)
    ;   % If there aren't enough lines to form a table, return []
        Lists = []
    ).


markdown_table_to_lists(MarkdownTable, []) :-
    atomics_to_string(MarkdownRows, '\n', MarkdownTable),
    MarkdownRows = [_].

remove_quotes(QuotedString, UnquotedString) :-
    (   string(QuotedString); atom(QuotedString) ),
    re_matchsub("^\"(.*)\"$", QuotedString, Sub),
    UnquotedString = Sub.1, !.
remove_quotes(X, X).
    

