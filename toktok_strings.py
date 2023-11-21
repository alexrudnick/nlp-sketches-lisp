#!/usr/bin/env python3

"""Print Common Lisp versions of Toktok regexes.

Reaches into NLTK's ToktokTokenizer and grabs the relevant regex objects.
"""

import re
import string
import nltk


REPLACE_THESE = ''';"'''

def char_replace(c):
    if c in REPLACE_THESE: # or c not in string.printable:
        return "\\\\{}".format(c)
    else:
        return c

def lisp_version(name, regex_pair):
    regex, replacement = regex_pair
    chars = [char_replace(c) for c in regex.pattern]
    pattern = "".join(chars)
    pattern = pattern.replace(r"\.", "[.]")

    replacement = replacement.replace("\\1", "\\\\1")

    return (
f"""(defconstant {name}
  '("{pattern}"
     . "{replacement}"))""")



correct_order = """\
(defconstant *toktok-regex-pairs*
  (list
        *non-breaking*
        *funky-punct-1*
        *funky-punct-2*
        *url-foe-1*
        *url-foe-2*
        *url-foe-3*
        *url-foe-4*
        *ampercent*
        *tab*
        *pipe*
        *open-punct-re*
        *close-punct-re*
        *multi-commas*
        *comma-in-num*
        *prob-single-quotes*
        *stupid-quotes-1*
        *stupid-quotes-2*
        *currency-sym-re*
        *en-em-dashes*
        *multi-dashes*
        *multi-dots*
        *final-period-1*
        *final-period-2*
        *one-space*))
"""

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
    print(correct_order)

    ## print("""(defconstant *toktok-regex-pairs* (list {}))"""
    ##         .format(" ".join(pair_names)))

    ## print(python_names)



if __name__ == "__main__": main()
