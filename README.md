
---
# Replacement of ae oe ue by the UTF-8 umlaut-glyph ä ö ü

ReplaceUmlaut processes a `txt` or `md` file (or all `md` files in a directory) and replaces the umlaut to its proper form (possibly capitalized). It uses an extensible list to avoid replacements which are not appropriate.

## The file nichtUmlaut.txt lists exceptions
It is an extensible list of parts of words containing character 
combinations which should not be replaced; for example, Koeffizient. The list needs only contain parts of words not a complete list of all words with non-replacable combinations. 

## Command Line Use 
The command `replaceUmlaut filepath` processes just the file and returns the changed file (the original is renamed with extension `bak`). 
Switches: 
    - `-m directory` processes all `md` files in the directory.
    - `-d` is a debug option, where the original file is returned unchanged and the changed version is returned with extension `new`. 

## Function 

The function `procMd1` is useful to process a single `md` file in a program. The list of permitted combinations must be passed as an argument. It returns True if the file is changed.
Typical use is 

````haskell
    changed <- header
        then  do 
            erl1 <- readErlaubt  fnErlaubt
            let addErl = dyDoNotReplace . meta1 $ doc1
                -- allow additions to the list in the YAML header
                erl2 = addErl -- add erl1
            changed1 <- applyReplace debugReplace erl2   fnin 
            return changed1
````

<!-- ## Source: 
### in app is Main.hs
this is the main for the utility which converts all md files for myHomepage. it looks at the value of *language* and converts only files which have *de_AT*, *de_CH*, or *de_DE*. 
conversion of test in the yaml header is not converted!

### Lib contains 
- ProcTxt.hs 
- ProcWord.hs
which are the actual conversion routines -->

