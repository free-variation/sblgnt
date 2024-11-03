:- use_module(library(yall)).
:- use_module(library(strings)).

:- [
    'src/prolog/text_utils.pro',
    'src/prolog/lp_utils.pro'
    ].

book_name(1, matthew, matthew, 'Matthew', 'Matt.').
book_name(2, mark, mark, 'Mark', 'Mark').
book_name(3, luke, luke, 'Luke', 'Luke').
book_name(4, john, john, 'John', 'John').
book_name(5, acts, luke, 'Acts', 'Acts').
book_name(6, romans, paul, 'Romans', 'Rom.').
book_name(7, i_corinthians, paul, '1 Corinthians', '1 Cor.').
book_name(8, ii_corintians, paul, '2 Corinthians', '2 Cor.').
book_name(9, galatians, paul, 'Galatians', 'Gal.').
book_name(10, ephesians, paul, 'Ephesians', 'Eph.').
book_name(11, philippians, paul, 'Philippians', 'Phil.').
book_name(12, colossians, paul, 'Colossians', 'Col.').
book_name(13, i_thessalonians, paul, '1 Thessalonians', '1 Thess.').
book_name(14, ii_thessalonians, paul, '2 Thessalonians', '2 Thess.').
book_name(15, i_timothy, paul, '1 Timothy', '1 Tim.').
book_name(16, ii_timothy, paul, '2 Timothy', '2 Tim.').
book_name(17, titus, paul, 'Titus', 'Titus').
book_name(18, philemon, paul, 'Philemon', 'Philem.').
book_name(19, hebrews, paul, 'Hebrews', 'Heb.').
book_name(20, james, james, 'James', 'Jas.').
book_name(21, i_peter, peter, '1 Peter', '1 Pet.').
book_name(22, ii_peter, peter, '2 Peter', '2 Pet.').
book_name(23, i_john, john2, '1 John', '1 John').
book_name(24, ii_john, john3, '2 John', '2 John').
book_name(25, iii_john, john3, '3 John', '3 John').
book_name(26, jude, jude, 'Jude', 'Jude').
book_name(27, revelation, john4, 'Revelation', 'Rev.').

book_chapter_verse(X, Book, Chapter, Verse) :-
    sub_atom(X, 0, 2, _, BookCode),
    sub_atom(X, 2, 2, _, ChapterCode),
    sub_atom(X, 4, 2, _, VerseCode),

    atom_number(BookCode, BookNumber),
    book_name(BookNumber, Book, _Author, _FullName, _Abbrev),
    atom_number(ChapterCode, Chapter),
    atom_number(VerseCode, Verse).

pos('A-', adjective, a).  
pos('C-', conjunction, '&').  
pos('D-', adverb, adv).  
pos('I-', interjection, int).  
pos('N-', noun, n).  
pos('P-', preposition, p).  
pos('RA', definite_article, def).  
pos('RD', demonstrative_pronoun, dem).  
pos('RI', interrogative_indefinite_pronoun, indef).  
pos('RP', personal_pronoun, pp).  
pos('RR', relative_pronoun, rp).  
pos('V-', verb, v).  
pos('X-', particle, part).  

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
    pos(X2, POS, _),

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
    arg(2, Word1, BookName),
    book_name(_BookNumber, BookName, Author, FullName, Abbrev),
    Book = book(BookName, Author, FullName, Abbrev, Chapters).

build_nt(NT) :-
    expand_file_name('./*.txt', BookFiles),
    maplist(load_book, BookFiles, BookWords),
    maplist(build_book, BookWords, Books),
    flatten(BookWords, AllWords),
    NT = nt(Books, AllWords).

assert_verse(BookName, ChapterNumber, verse(VerseNumber, Words)) :-
    maplist({}/[Word, WordText]>>(arg(7, Word, WordText)), Words, Texts),
    atomics_to_string(Texts, ' ', VerseText),
    assertz(verse(BookName, ChapterNumber, VerseNumber, Words, VerseText)).

assert_chapter(BookName, chapter(ChapterNumber, Verses)) :-
    maplist(assert_verse(BookName, ChapterNumber), Verses),
    assertz(chapter(ChapterNumber, Verses)).

assert_book(book(BookName, Author, FullName, Abbrev, Chapters)) :-
    maplist(assert_chapter(BookName), Chapters),
    assertz(book(BookName, Author, FullName, Abbrev, Chapters)).

assert_nt(nt(Books, Words)) :-
    maplist(assertz, Words),
    maplist(assert_book, Books).

format_word(TargetLemma, Word, FormattedWord) :-
    arg(2, Word, Text),
    arg(3, Word, POS),
    arg(9, Word, Lemma),
    pos(_, POS, ShortPOS),

    (   Lemma = TargetLemma
    ->  format(atom(FormattedWord), '**~w**/~w', [Text, ShortPOS])
    ;   format(atom(FormattedWord), '~w/~w', [Text, ShortPOS])
    ).

other_verse(BookName, ChapterNumber, VerseNumber, Displacement, OtherVerseNumber) :-
    OtherVerseNumber is VerseNumber + Displacement,
    verse(BookName, ChapterNumber, OtherVerseNumber, _Words, _VerseText).

verse_context(BookName, ChapterNumber, VerseNumber, Context) :-
    verse(BookName, ChapterNumber, VerseNumber, _, _), !,

    (   other_verse(BookName, ChapterNumber, VerseNumber, -1, VerseNumber1)
    ->  PreviousVerse = VerseNumber1
    ;   PreviousVerse = none
    ),
    (   other_verse(BookName, ChapterNumber, VerseNumber, 1, VerseNumber2)
    ->  FollowingVerse = VerseNumber2
    ;   FollowingVerse = none
    ),

    Context = context(BookName, ChapterNumber, VerseNumber, PreviousVerse, FollowingVerse).


format_verse(BookName, ChapterNumber, VerseNumber, TargetLemma, FormattedVerse) :-
    verse(BookName, ChapterNumber, VerseNumber, Words, _VerseText),
    maplist(format_word(TargetLemma), Words, Texts),
    atomics_to_string(Texts, ' ', FormattedVerse).

find(UnaccentedLemma, Hits, FormattedVerses) :-
    findall(
        hit(Text, BookName, ChapterNumber, VerseNumber, POS, Features, Lemma),
        word(_, BookName, ChapterNumber, VerseNumber, POS, Features, Text, _, _, Lemma, _, UnaccentedLemma),
        Hits),
    maplist({UnaccentedLemma}/[hit(_, BookName, ChapterNumber, VerseNumber, _, _, _), FormattedVerse]>>
        (format_verse(BookName, ChapterNumber, VerseNumber, UnaccentedLemma, FormattedVerse)), Hits, FormattedVerses).

expand_hit(hit(Text, BookName, ChapterNumber, VerseNumber, POS, Features, Lemma), FormattedVerse, ExpandedHit) :-
    book(BookName, _Author, _FullName, Abbrev, _Chapters),
    atomics_to_string(Features, ',', FeaturesString),
    format(string(Citation), '~w ~w.~w', [Abbrev, ChapterNumber, VerseNumber]),

    parse_verse_xbar(BookName, ChapterNumber, VerseNumber, ParsedVerse),

    asv(BookName, ChapterNumber, VerseNumber, AsvVerse),
    kjv(BookName, ChapterNumber, VerseNumber, KjvVerse),
    nasb(BookName, ChapterNumber, VerseNumber, NasbVerse),

    verse_context(BookName, ChapterNumber, VerseNumber, Context),

    ExpandedHit = [
        Citation, Text, Lemma, POS, FeaturesString, 
        FormattedVerse, ParsedVerse,
        AsvVerse, KjvVerse, NasbVerse,
        Context
    ].

markdown_hit(
    [Citation, Text, Lemma, POS, FeaturesString, 
    FormattedVerse, ParsedVerse,
    AsvVerse, KjvVerse, NasbVerse, _Context],
    MarkdownHit) :-

     MarkdownHit = {|string(
        Citation, Text, Lemma, POS, FeaturesString, 
        FormattedVerse, ParsedVerse,
        AsvVerse, KjvVerse, NasbVerse) ||

### {Citation}

| Word | Lemma | POS | Features |
| --- | --- | --- | --- |
| {Text} | {Lemma} | {POS} | {FeaturesString} |

{FormattedVerse}

```
{ParsedVerse}
```

| ASV |
| --- |
| {AsvVerse} |

| KJV |
| --- |
| {KjvVerse} |

| NASB | 
| --- |
| {NasbVerse} |
    |}.


find_results_to_markdown(Hits, Verses, MarkdownFile) :-
    concurrent_maplist(expand_hit, Hits, Verses, HitLists),
    maplist(markdown_hit, HitLists, MarkdownHits),
   
    tell(MarkdownFile),
    maplist(format, MarkdownHits),
    told.

parse_verse_xbar(BookName, ChapterNumber, VerseNumber, ParsedVerse) :-
    print([BookName, ChapterNumber, VerseNumber]),nl,
    verse(BookName, ChapterNumber, VerseNumber, _Words, Verse),
    Prompt = {|string(Verse) ||

Given a Koine Greek sentence, generate its complete syntactic representation in labeled bracketing notation following these specifications:

1. Represent the overall sentence structure as [S ...] when no coordination exists, or [S-COORD [...]] for coordinated sentences.

2. For each clause, show:
   - Full functional projection: [COMP complementizer]
   - Full extended verbal projection: [VP verb arguments]
   - All determiner phrases as [DP [D det] [N noun]] for full DPs or [DP [D pronoun]] for pronouns
   - Preposition phrases as [PP [P prep] [DP ...]]
   - Negation when present as [NEG word]
   
3. For coordination, use:
   - [COORD [...]] to list all coordinated elements
   - Include coordinators in the structure as [CONJ word]

4. Maintain strict binary branching by:
   - Using nested brackets appropriately
   - Never listing more than two main constituents at the same level
   - Showing hierarchical structure through proper nesting

5. The complete output must contain:
   - The full sentence structure showing all syntactic relationships
   - Every word from the input sentence in its proper syntactic position

6. Example format:
[S [COMP that] [VP [V said] [DP [D the] [N man]]]]

7. Specific structural requirements:
   - All complement phrases must be on the right
   - All specifiers must be on the left
   - Movement traces shown as [t]
   - Adjuncts attached at the appropriate phrasal level
   - All pronouns treated as determiners heading DPs

8. When processing any input:
   a. First identify all clausal boundaries
   b. Map the full functional sequence for each clause
   c. Position all arguments in their proper structural positions
   d. Add functional projections as needed
   e. Show coordination structure when present

Return only the bracketed notation representing the parse with appropriate indentation and line breaks for readability.
No extra fluff, no introductions, no discussion, no conclusions.  Just the tree.

Here is the sentence:
{Verse}
    
    |},

    run_CoT('claude-3-5-sonnet-20241022', [Prompt], _, Responses),
    last(Responses, Response),
    response_content(Response, ParsedVerseString),
    format(ParsedVerseString),nl,
    ParsedVerse = ParsedVerseString.

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

read_bible(asv) :-
    csv_read_file('bibles/asv.tsv', [ _ | Verses], [functor(verse)]),
    include({}/[verse(_, _, BookNumber, _, _, _)]>>(BookNumber > 39), Verses, NTVerses),
    maplist({}/[verse(_, _, FullBookNumber, ChapterNumber, VerseNumber, Verse)]>>(
        BookNumber is FullBookNumber - 39, 
        book_name(BookNumber, BookName, _, _, _), 
        assertz(asv(BookName, ChapterNumber, VerseNumber, Verse))),
    NTVerses).

read_bible(kjv) :-
    csv_read_file('bibles/kjv.tsv', [ _ | Verses], [functor(verse)]),
    include({}/[verse(_, _, BookNumber, _, _, _)]>>(BookNumber > 39), Verses, NTVerses),
    maplist({}/[verse(_, _, FullBookNumber, ChapterNumber, VerseNumber, Verse)]>>(
        BookNumber is FullBookNumber - 39, 
        book_name(BookNumber, BookName, _, _, _), 
        assertz(kjv(BookName, ChapterNumber, VerseNumber, Verse))),
    NTVerses).

read_bible(nasb) :-
    read_file_to_string('bibles/nasb.tsv', Book, []),
    atomics_to_string(VerseStrings, '\n', Book),
    VerseStrings = [ _Header | Rows],
    maplist({}/[Row, Columns]>>(atomics_to_string(Columns, '\t', Row)), Rows, VerseRows),
    maplist(nasb_row_to_verse, VerseRows, NASBVerses),
    maplist(assertz, NASBVerses).

nasb_row_to_verse([FullName, Chapter, Verse, VerseText], NASBVerse) :-
    book_name(_, BookName, _, FullName, _),
    atom_number(Chapter, ChapterNumber),
    atom_number(Verse, VerseNumber),
    NASBVerse = nasb(BookName, ChapterNumber, VerseNumber, VerseText).

init :-
    cross(Cross),
    format(Cross),
    init_api_key,
    set_prolog_flag(trace_lp, false),
    build_nt(NT),
    assert_nt(NT), 
    maplist(read_bible, [asv, kjv, nasb]),
    !.
