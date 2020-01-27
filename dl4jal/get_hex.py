#!/usr/bin/env python2

import os
import sys

# Parse modes
PM_VOID = 0
PM_PAGE = 1
PM_HEADER = 2

def get_pages(fpath):
    pages = []
    content = []
    mode = PM_VOID
    cola = -1
    colb = -1
    with open(fpath, "r") as fin:
        for l in fin.readlines():
            l = unicode(l, 'utf-8').rstrip('\n')
            if mode == PM_VOID:
                if "Printed by Andreas" in l or "Dienstag November 10" in l :
                    mode = PM_HEADER
            elif mode == PM_HEADER:
                if "mc.pic" in l and "Nov 10" in l:
                    # Detect page columns
                    cola = l.find("Nov 10")
                    colb = l.find("Nov 10", cola+6)
                    mode = PM_PAGE
            elif mode == PM_PAGE:
                if not l or "Printed by Andreas" in l or "Dienstag November 10" in l:
                    # Compile Page
                    pages.append((cola, colb, content))
                    content = []
                    cola = -1
                    colb = -1
                    if not l:
                        mode = PM_VOID
                    else:
                        mode = PM_HEADER
                else:
                    content.append(l)

    return pages

def output_line(line):
    try:
        print line
    except:
        print line.encode('latin-1', 'xmlcharrefreplace').replace("&#8722;","-").replace("&#8217;","'")

def output_double_page(page):
    cola = -1
    colb = -1
    # Detect actual text columns
    if page[1] == -1:
        # Special handling of last page
        # Get line number, here we assume that all last lines
        # on each page have a line number in the front!
        last_line = page[2][-1]
        ll = last_line.split()
        # Compute start column of actual text
        cola = page[0]+len(ll[0])+3
    else:
        # Split last line of the page in a left and right part
        last_line = page[2][-1]
        left = last_line[0:page[1]]
        right = last_line[page[1]:]
        # Get line number, here we assume that all last lines
        # on each page have a line number in the front!
        ll = left.split()
        lr = right.split()
        # Compute start column of actual text
        cola = page[0]+len(ll[0])+3
        colb = page[1]+len(lr[0])+3

    # Output left page
    line = page[2][0][cola:page[1]]
    for l in page[2][1:]:
        # Does the current line have a line number?
        num = l[0:cola].strip(' ')
        if num:
            # Yes, so output the previous line first
            output_line(line)
            line = l[cola:page[1]].rstrip()
        else:
            # No, so merge both together
            line += l[cola:page[1]].rstrip()
    # Final line
    output_line(line)

    if page[1] != -1:
        # Output right page
        line = page[2][0][colb:]
        for l in page[2][1:]:
            # Does the current line have a line number?
            num = l[page[1]:colb].strip(' ')
            if num:
                # Yes, so output the previous line first
                output_line(line)
                line = l[colb:].rstrip()
            else:
                # No, so merge both together
                line += l[colb:].rstrip()
        # Final line
        output_line(line)

def usage():
    print "Usage: get_hex.py <txt_file>"

def main():
    if len(sys.argv) < 2:
        usage()
        sys.exit(0)

    pages = get_pages(sys.argv[1])

    for p in pages:
        output_double_page(p)

if __name__ == "__main__":
    main()

