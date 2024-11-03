remove_accent('ί', 'ι').
remove_accent('Ἰ', 'Ι').
remove_accent('ὶ', 'ι').
remove_accent('ἰ', 'ι').
remove_accent('ῖ', 'ι').
remove_accent('ἶ', 'ι').
remove_accent('ΐ', 'ι').
remove_accent('ἴ', 'ι').
remove_accent('ἱ', 'ι').
remove_accent('ἴ', 'ι').

remove_accent('ό', 'ο').
remove_accent('ὸ', 'ο').
remove_accent('ὁ', 'ο').
remove_accent('ὅ', 'ο').
remove_accent('ὄ', 'ο').

remove_accent('ἐ', 'ε').
remove_accent('ὲ', 'ε').
remove_accent('έ', 'ε').
remove_accent('ἐ', 'ε').
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
remove_accent('Ἀ', 'α').
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
    

