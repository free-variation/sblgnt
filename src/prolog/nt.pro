:- ['src/prolog/text_utils.pro'].

book_name(1, matthew, matthew).
book_name(2, mark, mark).
book_name(3, luke, luke).
book_name(4, john, john).
book_name(5, acts, luke).
book_name(6, romans, paul).
book_name(7, i_corinthians, paul).
book_name(8, ii_corintians, paul).
book_name(9, galatians, paul).
book_name(10, ephesians, paul).
book_name(11, philippians, paul).
book_name(12, colossians, paul).
book_name(13, i_thessalonians, paul).
book_name(14, ii_thessalonians, paul).
book_name(15, i_timothy, paul).
book_name(16, ii_timothy, paul).
book_name(17, titus, paul).
book_name(18, philemon, paul).
book_name(19, hebrews, paul).
book_name(20, james, james).
book_name(21, i_peter, peter).
book_name(22, ii_peter, peter).
book_name(23, i_john, john2).
book_name(24, ii_john, john3).
book_name(25, iii_john, john3).
book_name(26, jude, jude).
book_name(27, revelation, john4).


 book_chapter_verse(X, Book, Chapter, Verse) :-
    sub_atom(X, 0, 2, _, BookCode),
    sub_atom(X, 2, 2, _, ChapterCode),
    sub_atom(X, 4, 2, _, VerseCode),

    atom_number(BookCode, BookNumber),
    book_name(BookNumber, BookName, Author),
    atom_number(ChapterCode, Chapter),
    atom_number(VerseCode, Verse),

    Book = book(BookNumber, BookName, Author).

pos('A-', adjective).  
pos('C-', conjunction).  
pos('D-', adverb).  
pos('I-', interjection).  
pos('N-', noun).  
pos('P-', preposition).  
pos('RA', definite_article).  
pos('RD', demonstrative_pronoun).  
pos('RI', interrogative_indefinite_pronoun).  
pos('RP', personal_pronoun).  
pos('RR', relative_pronoun).  
pos('V-', verb).  
pos('X-', particle).  

person('-', []).
person('1', first).
person('2', second).
person('3', third).

tense('-', []).
tense('P', present).
tense('I', imperfect).
tense('F', future).
tense('A', aorist).
tense('X', perfect).
tense('Y', pluperfect).

voice('-', []).
voice('A', active).
voice('M', middle).
voice('P', passive).

mood('-', []).
mood('I', indicative). 
mood('D', imperative).
mood('S', subjunctive).
mood('O', optative).
mood('N', infinitive).
mood('P', participle).

case('-', []).
case('N', nominative).
case('A', accusative).
case('G', genitive).
case('D', dative).
case('V', vocative).

case('-', []).
case('S', singular).
case('P', plural).

gender('-', []).
gender('F', feminine).
gender('M', masculine).
gender('N', neuter).

degree('-', []).
degree('C', comparative).
degree('S', superlative).

parse_word(Word, ParsedWord) :-
    %print(Word), nl,
    atomics_to_string(Xs, ' ', Word),

    nth1(1, Xs, X1),
    book_chapter_verse(X1, Book, Chapter, Verse),

    nth1(2, Xs, X2),
    pos(X2, POS),

    nth1(3, Xs, X3),
    sub_atom(X3, 0, 1, _, PersonCode),
    person(PersonCode, Person),
    
    sub_atom(X3, 1, 1, _, TenseCode),
    tense(TenseCode, Tense),

    sub_atom(X3, 2, 1, _, VoiceCode),
    voice(VoiceCode, Voice),

    sub_atom(X3, 3, 1, _, MoodCode),
    mood(MoodCode, Mood),

    sub_atom(X3, 4, 1, _, CaseCode),
    case(CaseCode, Case),

    sub_atom(X3, 5, 1, _, NumberCode),
    case(NumberCode, Number),

    sub_atom(X3, 6, 1, _, GenderCode),
    gender(GenderCode, Gender),

    sub_atom(X3, 7, 1, _, DegreeCode),
    degree(DegreeCode, Degree),

    AllFeatures = [
        Person,
        Tense,
        Voice,
        Mood,
        Case,
        Number,
        Gender,
        Degree
    ],
    flatten(AllFeatures, Features),

    nth1(4, Xs, Text),
    nth1(5, Xs, WordAtom),
    nth1(6, Xs, NormalizedWord),
    nth1(7, Xs, Lemma),
    remove_accents(NormalizedWord, NormalizedWordNoAccents),
    remove_accents(Lemma, LemmaWithoutAccents),

    ParsedWord = word(
        Word, Book, Chapter, Verse, POS, Features,
        Text, WordAtom, NormalizedWord, Lemma, 
        NormalizedWordNoAccents, LemmaWithoutAccents
    ).

load_book(Filename, Book) :-
    read_file_to_string(Filename, BookString, []),
    atomics_to_string(AllWords, '\n', BookString),
    exclude({}/[Line]>>(Line = ''), AllWords, Words),
    maplist(parse_word, Words, Book).

