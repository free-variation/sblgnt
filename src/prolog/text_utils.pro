process_char('ί', 'ι').
process_char('Ἰ', 'Ι').
process_char('ὶ', 'ι').
process_char('ἰ', 'ι').
process_char('ῖ', 'ι').
process_char('ἶ', 'ι').
process_char('ΐ', 'ι').
process_char('ἴ', 'ι').
process_char('ἱ', 'ι').
process_char('ἴ', 'ι').

process_char('ό', 'ο').
process_char('ὸ', 'ο').
process_char('ὁ', 'ο').
process_char('ὅ', 'ο').
process_char('ὄ', 'ο').

process_char('ἐ', 'ε').
process_char('ὲ', 'ε').
process_char('έ', 'ε').
process_char('ἐ', 'ε').
process_char('ἕ', 'ε').
process_char('ἔ', 'ε').

process_char('ώ', 'ω').
process_char('ὼ', 'ω').
process_char('ῶ', 'ω').
process_char('ῷ', 'ω').
process_char('ὡ', 'ω').
process_char('ὧ', 'ω').
process_char('ῴ', 'ω').

process_char('ύ', 'υ').
process_char('ὺ', 'υ').
process_char('ῦ', 'υ').
process_char('ὖ', 'υ').
process_char('ὐ', 'υ').
process_char('ὑ', 'υ').
process_char('ὗ', 'υ').
process_char('ὕ', 'υ').

process_char('ἀ', 'α').
process_char('ά', 'α').
process_char('ἄ', 'α').
process_char('ὰ', 'α').
process_char('Ἀ', 'α').
process_char('ἁ', 'α').
process_char('ἅ', 'α').
process_char('ᾶ', 'α').

process_char('ὴ', 'η').
process_char('ῆ', 'η').
process_char('ή', 'η').
process_char('ἡ', 'η').
process_char('ἦ', 'η').
process_char('ῇ', 'η').
process_char('ἠ', 'η').

process_char(X, X).

% Remove accents from input string
remove_accents(Input, LowerNoAccents) :-
    atom_chars(Input, CodesIn),
    maplist(process_char, CodesIn, CodesOut),
    atom_chars(Output, CodesOut),
    downcase_atom(Output, LowerNoAccents).
