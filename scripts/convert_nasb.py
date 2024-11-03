#!/usr/bin/env python3

import sys
import re

def convert_bible_to_tsv(input_file, output_file):
    with open(input_file, 'r', encoding='utf-8') as infile, \
         open(output_file, 'w', encoding='utf-8') as outfile:
        
        # Write the header row
        outfile.write("BookName\tChapterNumber\tVerseNumber\tVerseText\n")
        
        BookName = ''
        ChapterNumber = ''
        VerseNumber = ''
        VerseText = ''
        
        lines = infile.readlines()
        i = 0
        while i < len(lines):
            line = lines[i].strip()
            i += 1

            # Skip blank lines
            if not line:
                continue

            # Match chapter header lines
            chapter_header_match = re.match(r'^(.+?)\s+(\d+)\s+New American Standard Bible$', line)
            if chapter_header_match:
                BookName = chapter_header_match.group(1)
                ChapterNumber = chapter_header_match.group(2)
                continue

            # Match verse lines starting with a number
            verse_line_match = re.match(r'^(\d+)\s+(.*)$', line)
            if verse_line_match:
                # Output the previous verse if it exists
                if VerseNumber:
                    outfile.write(f"{BookName}\t{ChapterNumber}\t{VerseNumber}\t{VerseText.strip()}\n")
                VerseNumber = verse_line_match.group(1)
                VerseText = verse_line_match.group(2)

                # Collect additional lines that belong to the same verse
                while i < len(lines):
                    next_line = lines[i].strip()
                    if not next_line:
                        i += 1
                        continue
                    # If the next line is a chapter header or a new verse, break
                    if re.match(r'^(.+?)\s+(\d+)\s+New American Standard Bible$', next_line) or \
                       re.match(r'^\d+\s+.*$', next_line):
                        break
                    # Else, append to VerseText
                    VerseText += ' ' + next_line
                    i += 1
                continue

        # Write the last verse
        if VerseNumber:
            outfile.write(f"{BookName}\t{ChapterNumber}\t{VerseNumber}\t{VerseText.strip()}\n")

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: python convert_bible.py input.txt output.tsv")
    else:
        convert_bible_to_tsv(sys.argv[1], sys.argv[2])
