#!/usr/bin/env python3

"""Print Common Lisp versions of Toktok regexes.

Reaches into NLTK's ToktokTokenizer and grabs the relevant regex objects.
"""

import re
import string
import nltk

def lisp_version(name, regex_pair):
    regex, replacement = regex_pair
    chars = [c if c in string.printable else "\\U+{:X}".format(ord(c))
             for c in regex.pattern]
    pattern = "".join(chars)

    return (
f"""(defconstant {name}
  '("{pattern}"
     . "{replacement}"))""")

def main():
    pair_names = []
    python_names = []
    for key in dir(nltk.tokenize.toktok.ToktokTokenizer):
        item = getattr(nltk.tokenize.toktok.ToktokTokenizer, key)
        newname = "*" + key.lower().replace("_", "-") + "*"
        if type(item) is tuple and len(item) == 2:
            python_names.append(key)
            pair_names.append(newname)
            try:
                print(lisp_version(newname, item))
                print()

            except:
                print("failed for key", key)

    print("""(defconstant *toktok-regex-pairs* (list {}))"""
            .format(" ".join(pair_names)))

    print(python_names)



if __name__ == "__main__": main()
