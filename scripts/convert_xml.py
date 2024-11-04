#!/usr/bin/env python3

import sys
import xml.etree.ElementTree as ET

def convert_bible_xml_to_tsv(input_file, output_file):
    tree = ET.parse(input_file)
    root = tree.getroot()

    with open(output_file, 'w', encoding='utf-8') as outfile:
        outfile.write("VerseID\tBookName\tBookNumber\tChapterNumber\tVerseNumber\tVerseText\n")
        VerseID = 0
        BookNumber = 0
        for book in root.findall('b'):
            BookNumber += 1
            BookName = book.get('n')
            for chapter in book.findall('c'):
                ChapterNumber = chapter.get('n')
                for verse in chapter.findall('v'):
                    VerseID += 1
                    VerseNumber = verse.get('n')
                    VerseText = verse.text.strip() if verse.text else ''
                    outfile.write(f"{VerseID}\t{BookName}\t{BookNumber}\t{ChapterNumber}\t{VerseNumber}\t{VerseText}\n")

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: python convert_bible_xml.py input.xml output.tsv")
    else:
        convert_bible_xml_to_tsv(sys.argv[1], sys.argv[2])
