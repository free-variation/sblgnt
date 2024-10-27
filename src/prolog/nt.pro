:- use_module(library(yall)).
:- use_module(library(strings)).

:- [
    'src/prolog/text_utils.pro',
    'src/prolog/lp_utils.pro'
    ].

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
        Word, Book, Chapter, Verse, 
        POS, Features,
        Text, WordAtom, NormalizedWord, Lemma, 
        NormalizedWordNoAccents, LemmaWithoutAccents
    ).

load_book(Filename, ParsedWords) :-
    read_file_to_string(Filename, BookString, []),
    atomics_to_string(AllWords, '\n', BookString),
    exclude({}/[Line]>>(Line = ''), AllWords, Words),
    maplist(parse_word, Words, ParsedWords).

build_word(FullWord, WordNumber, Word) :-
    FullWord = word(
        _Word, _Book, _Chapter, _Verse, 
        POS, Features,
        Text, WordAtom, NormalizedWord, Lemma, 
        NormalizedWordNoAccents, LemmaWithoutAccents
    ),

    Word = word(WordNumber, Text, POS, Features, WordAtom, NormalizedWord, Lemma, NormalizedWordNoAccents, LemmaWithoutAccents).

build_verse(Words, VerseNumber, Verse) :-
    include({VerseNumber}/[Word]>>(arg(4, Word, VerseNumber)), Words, VerseWords),
    length(VerseWords, NumWords),
    findall(X, between(1, NumWords, X), WordNumbers),
    maplist(build_word, VerseWords, WordNumbers, VerseWords1),

    Verse = verse(VerseNumber, VerseWords1).

build_chapter(Words, ChapterNumber, Chapter) :-
    include({ChapterNumber}/[Word]>>(arg(3, Word, ChapterNumber)), Words, ChapterWords),
    maplist({}/[Word, Verse]>>(arg(4, Word, Verse)), ChapterWords, AllVerses),
    sort(AllVerses, VerseNumbers),
    maplist(build_verse(ChapterWords), VerseNumbers, Verses),

    Chapter = chapter(ChapterNumber, Verses).

build_book(Words, Book) :-
    maplist({}/[Word, Chapter]>>(arg(3, Word, Chapter)), Words, AllChapters),
    sort(AllChapters, ChapterNumbers),
    maplist(build_chapter(Words), ChapterNumbers, Chapters),
    
    Words = [Word1 | _],
    arg(2, Word1, book(BookNumber, BookName, Author)),
    Book = book(BookNumber, BookName, Author, Chapters).

build_nt(NT) :-
    expand_file_name('./*.txt', BookFiles),
    maplist(load_book, BookFiles, BookWords),
    maplist(build_book, BookWords, Books),
    flatten(BookWords, AllWords),
    NT = nt(Books, AllWords).

assert_verse(BookNumber, ChapterNumber, verse(VerseNumber, Words)) :-
    assertz(verse(BookNumber, ChapterNumber, VerseNumber, Words)).

assert_chapter(BookNumber, chapter(ChapterNumber, Verses)) :-
    maplist(assert_verse(BookNumber, ChapterNumber), Verses),
    assertz(chapter(ChapterNumber, Verses)).

assert_book(book(BookNumber, BookName, Author, Chapters)) :-
    maplist(assert_chapter(BookNumber), Chapters),
    assertz(book(BookNumber, BookName, Author, Chapters)).

assert_nt(nt(Books, Words)) :-
    maplist(assertz, Words),
    maplist(assert_book, Books).

format_verse(BookNumber, ChapterNumber, VerseNumber, FormattedVerse) :-
    verse(BookNumber, ChapterNumber, VerseNumber, Words),
    maplist({}/[Word, Text]>>(arg(2, Word, Text)), Words, Texts),
    atomics_to_string(Texts, ' ', FormattedVerse).

find(UnaccentedLemma, Hits, FormattedVerses) :-
    findall(
        hit(Text, BookNumber, Chapter, Verse, POS, Features, Lemma),
        word(_, book(BookNumber, _, _), Chapter, Verse, POS, Features, Text, _, _, Lemma, _, UnaccentedLemma),
        Hits),
    maplist({}/[hit(_, BookNumber, Chapter, Verse, _, _, _), FormattedVerse]>>
        (format_verse(BookNumber, Chapter, Verse, FormattedVerse)), Hits, FormattedVerses).

parse_verse_xbar(Model, BookNumber, ChapterNumber, VerseNumber, ParsedVerse) :-
    format_verse(BookNumber, ChapterNumber, VerseNumber, Verse),

    Prompt = {|string(Verse) ||

Given a Koine Greek sentence, generate its complete syntactic representation in Prolog following these specifications:

1. Represent the overall sentence structure as s(coord([...])) when coordination exists, or as s(...) for single clauses.

2. For each clause, show:
   - Full functional projection: comp/1 for complementizers
   - Full extended verbal projection: vp/2 or vp/3 including arguments
   - All determiner phrases as dp(det(D)) for pronouns or dp(det(D), n(N)) for full DPs
   - Preposition phrases as pp(p(P), dp(...))
   - Negation when present as neg(Word)
   
3. For coordination, use:
   - coord([...]) to list all coordinated elements
   - Include the coordinator in the structure

4. Maintain strict binary branching by:
   - Using only binary predicates or unary predicates 
   - Never allowing more than two arguments per phrase
   - Using embedded structures rather than flat lists

5. Include all lexical entries in the form:
```prolog
lex(Word, Category).
```
where Category must be one of:
- conj (coordinators)
- comp (complementizers)
- v (verbs)
- n (nouns)
- det (determiners/articles/pronouns)
- p (prepositions)
- adv (adverbs)
- neg (negation)

6. The complete output must contain:
   - The full sentence structure showing all syntactic relationships
   - Every word from the input sentence in its proper syntactic position
   - A lexical entry for every word

7. Example format:
```prolog
% Main sentence structure
s(coord([
    s(subord(comp(Word1), adv(Word2), 
        s(comp(Word3),
          vp(v(Word4), dp(det(Word5))),
          vp(v(Word6), dp(det(Word7), n(Word8))))
    )),
    s(vp(v(Word9))),
    s(vp(v(Word10), dp(det(Word11), n(Word12)))),
    s(vp(v(Word13))),
    % ... continue for all clauses
])).

% Lexical entries
lex(Word1, Category1, MorphosyntacticFeatures1).
lex(Word2, Category2, MorphosyntacticFeatures2).
lex(Word3, Category3, MorphosyntacticFeatures3).
% ... continue for all words
```

In identifying the morphosyntactic features of verbs, specify: voice, mood, aspect, tense, person, number.

8. Specific structural requirements:
   - All complement phrases must be on the right
   - All specifiers must be on the left
   - Movement traces must be shown with t(X) where relevant
   - Adjuncts must attach at the appropriate phrasal level
   - All pronouns are treated as determiners heading DPs

9. When processing any input:
   a. First identify all clausal boundaries
   b. Map the full functional sequence for each clause
   c. Position all arguments in their proper structural positions
   d. Add functional projections as needed
   e. Show coordination structure when present
   f. Include all lexical items

10. The output must be valid Prolog syntax that could be loaded into a Prolog interpreter.

Apply this format consistently to any input Koine Greek sentence, maintaining the same level of detail and structural representation shown in the format above.

Here is the sentence:
{Verse}
    
    |},

    run_CoT(Model, [Prompt], _, Responses),
    last(Responses, Response),
    response_content(Response, ParsedVerse).

system_message('You are an expert in Koine Greek and in analyzing and translating the New Testament').

cross({|string||

⡀⢂⠄⠠⠀⠄⠠⠀⠄⠠⠀⠄⠠⠀⠄⠠⠀⠄⠠⠀⠄⠠⢷⣗⣄⠀⠄⠠⠀⠄⠠⠀⠄⠠⠀⠄⠠⠀⠄⠠⠀⠄⣀⣶⡿⠁⠠⠀⠄⠠⠀⠄⠠⠀⠄⠠⠀⠄⠠⠀⠄⠠⠀⠄⠠⠀⠄
⡐⠠⢈⠀⠡⠈⠄⠡⠈⠄⠡⠈⠄⠡⠈⠄⠡⠈⠄⠡⢈⠐⠈⣿⣿⣵⣀⠡⢈⠠⠁⠌⠠⠁⠌⠠⠁⠌⡀⢁⢂⣼⣿⣿⠁⠠⠁⠌⠠⠁⠌⠠⠁⠌⠠⠁⠌⠠⠁⠌⠠⠁⠌⠠⠁⠌⡀
⠄⠡⠀⠌⠠⠁⠌⠠⠁⢂⠁⠂⠌⠠⢁⠈⠄⠡⢈⠐⠠⢈⠐⠸⣿⣿⣷⣵⣄⠠⢈⠠⠁⠌⠠⠁⠌⢀⢐⣴⣿⣿⣿⢇⠠⠁⠌⠠⠁⠌⠐⠠⠁⢂⠡⠈⡐⠠⢁⠈⡐⢈⠠⠁⠌⠐⡀
⠌⠠⢁⠈⠄⠡⠈⠄⡁⢂⠈⡐⠈⠄⢂⠈⠄⠡⠀⠌⠐⠠⠈⠄⢻⣿⣿⣿⣷⣶⢄⠐⠈⠠⠁⢂⢈⣴⣿⣿⣿⣿⡟⠀⡐⠈⠠⠁⠌⠠⢁⠂⢁⠂⠠⢁⠐⡀⢂⠐⡀⠂⠄⠡⢈⠐⡀
⠂⠡⠀⠌⠠⠁⠌⠐⡀⢂⠐⡀⢁⠂⠄⡈⠄⠡⠈⠄⡁⢂⠁⠂⠌⣿⣿⣿⣿⣿⣿⣖⢅⠂⢁⣶⣿⣿⣿⣿⣿⣿⠁⡐⢀⠁⢂⠡⠈⡐⠀⠌⡀⠂⡁⢂⠐⡀⢂⠐⡀⠡⢈⠐⡀⠂⠄
⡁⢂⠡⠈⡐⠈⠄⠡⢀⠂⡐⢀⠂⠌⠠⠐⠈⠄⡁⢂⠐⠠⢈⠐⠠⠹⣿⣿⣿⣿⣿⣿⣿⣵⣿⣿⣿⣿⣿⣿⣿⠇⢁⠀⠂⠌⠠⢀⠡⢀⠁⠂⠄⡁⠐⠠⠐⢀⠂⡐⠠⠁⠄⢂⠠⠁⠂
⡐⢀⠂⠁⠄⡈⠐⡀⢂⠐⠠⢀⠂⠌⡐⢈⠐⠠⠐⡀⠌⠐⠠⢈⠐⡀⢻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡟⠀⠄⡈⠐⡈⠐⡀⠂⠄⡈⠐⠠⢀⠁⢂⠁⢂⠐⢀⠂⡁⠂⠄⠂⠌⡀
⡄⢆⠈⡰⢠⢀⢡⠰⠀⡌⠰⡀⢆⠰⢠⠀⡌⢠⠁⡄⡈⡄⢡⢀⠆⠰⡈⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠁⡰⢠⢀⢡⢀⢡⠀⡁⠆⡄⡁⠆⡄⡈⡄⡈⡄⡈⡄⠰⢠⠁⡌⠰⢠⠀
⠆⡈⠆⢁⠃⡈⠆⡘⠰⠘⠰⠁⠎⠘⡀⢃⠘⡀⠃⠆⠱⠘⡀⠎⠘⠰⠁⢹⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡏⠰⠁⠆⡈⠆⡈⠆⠃⡁⠃⠆⢁⠃⠆⠱⢀⠱⠀⠱⠘⡀⠃⠆⡘⢀⠃⠆
⢂⠐⡈⢀⠂⡐⠠⢀⠁⠌⡐⠈⠄⡁⠄⢂⠐⡀⠡⠈⠄⡁⠄⡈⠄⠡⠈⠄⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠀⠄⡈⠐⠠⠐⡀⠌⠐⡀⠡⢈⠠⠈⠄⡁⢂⠠⠁⢂⠡⢀⠁⠂⠄⢂⠈⠄
⢀⠂⡐⢀⠂⠠⠁⠄⡈⠄⡐⢈⠠⠐⡀⠂⠄⠠⠁⠌⠐⡀⢂⠐⢈⠠⠁⢂⠘⣿⣿⣿⣿⣿⣿⣿⣿⣿⠃⠐⠠⢀⠁⢂⠁⠄⡈⠐⡀⢁⠂⠠⢁⠂⡐⠠⢀⠁⢂⠐⠠⠈⡐⠈⠠⠐⠀
⣲⣤⣔⣀⠄⠁⢂⠐⢀⠂⠄⠂⡐⠠⢀⠡⠈⠄⠡⢈⠐⠠⠀⠌⠠⠐⠈⠄⠠⢹⣿⣿⣿⣿⣿⣿⣿⡋⠀⠌⡐⠠⠈⠄⡈⠄⠠⢁⠐⠠⢈⠐⠠⠐⡀⠡⠀⠌⡀⠂⢁⠐⢀⣁⣢⣬⢶
⠈⠙⢻⣿⣿⣷⣶⣬⣄⣀⢂⠐⡀⠂⠄⠂⠡⠈⡐⠠⠈⠄⠡⢈⠐⡈⠐⡈⠐⡀⢿⣿⣿⣿⣿⣿⡿⡀⠡⠐⡀⢂⠁⠂⠄⡈⠐⠠⢈⠐⠠⠈⠄⠂⠐⡀⠡⣐⣠⣬⣶⣾⣿⣿⣟⠕⣉
⡁⠌⢀⠙⢿⣿⣿⣿⣿⣿⣿⣷⣶⣌⣤⣈⢀⠡⠐⢀⠁⠂⡁⠄⠂⠄⡁⠄⡁⠄⠸⣿⣿⣿⣿⣿⠃⢀⠂⠁⠄⠂⡈⠐⠠⢀⠁⠂⠄⡈⠐⣀⣢⣥⣶⣾⣿⣿⣿⣿⣿⣿⠿⠝⠀⠄⡀
⠐⡈⢀⠂⡀⠙⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣶⣤⣌⣐⠠⠐⠈⡀⠐⠠⠐⠈⠄⢹⣿⣿⣿⡏⠀⢂⠈⡐⠈⡐⠀⡁⠂⣄⣨⣴⣶⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠟⠛⢀⠐⢈⠠⠀
⡁⠐⠠⠐⡀⠂⠄⠙⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣶⣤⣅⣂⠁⠌⡐⠀⣿⣿⣿⠈⠐⡀⢂⣀⣡⣤⣶⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢟⠓⠁⠌⡀⠌⠠⢀⠁
⠠⢉⠀⠂⠄⠡⢈⠐⡀⠙⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣶⣤⣞⣿⣧⣤⣶⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡍⠁⠄⡁⠂⠄⠂⠡⢀⠂
⡐⠠⢈⠐⡈⠐⡀⠂⠄⣡⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠿⠛⢻⣿⡟⠛⠿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣮⡦⡐⠠⢀⠁⢂⠁⠂⠄⠂
⠠⠁⠄⢂⠠⠁⡀⣢⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠿⠛⠋⠉⢀⠀⠤⠐⣾⣿⣿⡀⠠⠀⠄⢈⠙⠛⠿⢿⡿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⡦⠀⠌⡀⠌⠐⡈⠄
⠡⠈⠄⠂⡀⣢⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠿⠛⠋⠉⢀⠀⠄⠂⡈⠐⡀⢂⠐⣸⣿⣿⣿⣇⠀⠡⢈⠀⠂⠌⡀⠄⡀⠉⡙⠛⠻⢿⡿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣢⠀⢂⠁⠄⠂
⠠⠁⢂⣡⣾⣿⣿⣿⣿⣿⣿⡿⠻⠛⠋⠉⢀⠠⠀⠄⠡⠈⠄⢂⠈⡐⢀⠡⠐⡀⢢⣿⣿⣿⣿⣿⡄⠡⠀⠌⡐⠠⠐⠠⢀⠡⠀⠌⠠⠀⠄⡉⠉⡛⠿⢿⣿⣿⣿⣿⣿⣿⣯⣢⡈⢀⠂
⢀⣡⣾⣿⣿⡿⠿⠛⠋⠉⡀⠠⠐⠠⠁⠌⢀⠂⠌⠠⢁⠂⠌⢀⠂⡐⠠⢀⠂⠐⣾⣿⣿⣿⣿⣿⣷⡀⠡⠐⡀⢂⠁⠂⠄⠂⡁⠂⠡⢈⠐⢀⠁⠄⠠⠀⠄⡉⠘⠛⠿⢿⣿⣿⣧⡤⡈
⠾⠟⠋⠉⡀⠠⢀⠂⠄⡁⠐⠠⠁⢂⠁⠌⡀⠂⠌⠐⠠⠐⡈⠠⠐⢀⠁⠂⠌⣸⣿⣿⣿⣿⣿⣿⣿⣇⠐⠠⠐⠠⢈⠐⡈⠐⠠⠁⠂⠄⡈⠄⡈⠄⠡⠈⠄⠠⠁⠂⠄⢂⠀⡉⠙⠛⠦
⠂⠄⡈⠐⡀⠡⠀⠌⡀⠄⡁⢂⠡⠀⠌⠐⡀⠡⠈⠄⡁⠂⠄⡁⠌⠀⠌⡈⢤⣿⣿⣿⣿⣿⣿⣿⣿⣿⡄⠁⠄⠡⢀⠂⠠⢁⠂⠁⠌⡐⢀⠐⠠⢈⠠⢁⠈⠄⠡⢈⠐⡀⠂⠄⡁⢂⠐
⡁⠆⡄⢡⠰⢠⠁⠆⡄⢆⠰⢠⢀⢡⠈⡰⢠⠁⡌⢠⢀⠁⢆⠰⢠⢁⠆⡄⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⡈⠰⢠⢈⠰⢠⢈⠰⢠⠰⢠⢈⠰⢠⠰⢠⠈⡄⢡⢀⠆⡰⠈⡄⡰⠀⠆
⢁⠃⡘⠀⠃⠆⢃⠘⠰⢈⠘⠰⠈⠆⠃⠱⢀⠃⡘⠰⢈⠘⡈⠘⡀⠎⠆⢸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⠁⠃⠆⡈⠆⢃⠈⡘⠰⠘⠰⢈⠘⠰⠘⡀⢃⠘⠰⢈⠰⢁⠘⠰⢁⠃⠆
⡀⠆⢀⠡⢈⠐⠠⢈⠐⠠⢈⠠⠁⢂⠁⠂⠄⠂⠄⠡⢀⠂⠄⠡⠀⠌⣀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡄⠡⠐⡀⠂⠄⠂⠄⡁⠂⠡⠀⠌⡐⠠⠐⠠⢈⠐⠠⠐⠠⠈⠄⢂⠐⡀
⠄⡈⠄⠂⠄⡈⠐⡀⠂⠡⢀⠂⠌⢀⠂⠡⢈⠐⢈⠐⡀⠂⠌⠐⣈⠠⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣦⠐⠠⢀⠁⢂⠡⠐⡀⢁⠂⢁⠂⠄⠂⡁⠂⠄⡈⠐⠠⢁⠂⠌⢀⠂⠄
⢂⠐⠠⠁⠂⠄⡁⠄⡁⠂⠄⡈⠐⠠⢈⠐⠠⢈⠀⠂⠄⠡⠈⠄⠠⢰⣿⣿⣿⣿⣿⣿⣿⠟⣿⣿⣿⣿⣿⣿⣿⡇⠐⡀⠌⡀⠐⠠⠐⠠⢈⠀⠂⠌⡐⢀⠁⠂⠄⡁⠂⠄⠂⠌⡀⠂⠄
⠂⠌⡀⠡⢈⠐⠠⠐⡀⢁⠂⠄⡁⠂⠄⡈⠐⠠⢈⠐⡈⠄⡁⠈⢄⣿⣿⣿⣿⣿⣿⠋⠋⡈⠈⠻⣿⣿⣿⣿⣿⣿⡀⠄⡐⠀⡁⢂⠁⠂⠄⡈⠐⠠⠐⡀⠌⡐⠠⢀⠡⢈⠐⠠⢀⠁⠂
⡁⠂⠄⢁⠂⠌⠠⠁⠄⢂⠈⠄⠠⢁⠂⠄⡁⠂⠄⠂⠄⠂⠄⠃⣼⣿⣿⣿⡿⡋⠊⡀⠂⠄⠡⢀⠈⠻⢿⣿⣿⣿⣧⠀⠄⠡⠐⠠⢈⠐⠠⢀⠁⢂⠡⢀⠐⡀⠡⢀⠂⠄⡈⠐⠠⠈⠄
⠠⠁⠌⢀⠂⠌⠐⡈⠄⠂⡈⠄⡁⠂⡐⠠⢀⠡⢈⠐⠈⡐⠈⣰⣿⣿⣿⡫⠉⡀⠂⠄⠡⢈⠐⠠⠈⠄⡈⠻⣿⣿⣿⡆⠈⠄⡈⠐⠠⢈⠐⠠⠈⠄⠂⠄⠂⠄⡁⢂⠐⠠⢀⠁⢂⠁⠂
⠄⠡⢈⠀⠂⠌⠐⡀⠂⡁⠐⠠⢀⠡⠐⢀⠂⡐⠠⢈⠐⠠⢁⣿⡿⠿⠂⠠⠐⢀⠡⠈⡐⠠⠈⠄⢁⠂⠐⡀⠈⠻⣿⣿⡀⠂⠄⢁⠂⠄⡈⠄⢁⠂⠡⠈⡐⠠⠐⠠⠈⡐⠠⠈⠄⡈⠄
⣈⡐⠠⠈⡐⠈⠄⠠⠁⠄⡁⠂⠄⠂⠁⠄⠂⠄⠁⢂⠈⠄⣼⡿⠋⠀⠄⡁⠌⢀⠂⠁⠄⠂⡁⠌⠀⠌⠐⠠⢁⡐⢈⣻⢧⠈⡐⠠⠈⠐⡀⠌⢀⠂⢁⠂⠄⠁⢂⠁⠂⠄⠁⠌⠐⢀⠂

|}).

init :-
    cross(Cross),
    format(Cross),
    init_api_key,
    set_prolog_flag(trace_lp, false).







