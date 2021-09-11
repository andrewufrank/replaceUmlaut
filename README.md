
---
# umwandeln von ae oe ue in umlaut-glyphe

## nichtUmlaut.txt 
liste von woertern oder wortteilen, in denen nicht umwandelbare kombinationen stecken. z.b Poet, zuerst etc.

die liste kann editiert und ergaenzt werden!

## executable 
scheinbar 
umlautConversion
convertiert alle md files im directory

## Source: 
### in app is Main.hs
this is the main for the utility which converts all md files for myHomepage. it looks at the value of *language* and converts only files which have *de_AT*, *de_CH*, or *de_DE*. 
conversion of test in the yaml header is not converted!

### Lib contains 
- ProcTxt.hs 
- ProcWord.hs
which are the actual conversion routines

