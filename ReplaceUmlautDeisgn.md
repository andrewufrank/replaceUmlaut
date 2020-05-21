# Umwandeln von Umlauten als ae, oe und ue geschrieben
## Anwendungsfall (use case)
Ich schreibe, wenn ich deutsch auf einer (normalen) amerikanischen Tastatur
schreibe, umlaute meist ersetzt durch die ae, oe und ue kombinationen,
die im Deutschen ueblich sind und in der Schweiz fuer die Grossbuchstaben
immer angewendet wird.

Fuer die weiter Verbreitung von Texten sind diese durch die Glyphen ä, ö und ü
zu ersetzen -- aber nicht in den wenigen Ausnahmen, bei denen die
Kombinationen in deutschen Woertern vorkommen: neuer, Poetik, Koeffizient etc.
## Architektur
Wie andere Aufgaben im Bereich der Verarbeitung natuerlicher Sprachen
wird diese "im allgemeinen richtig geloest" aber es duerfen Fehlerhafte Woerter
uebrig bleiben, die im immer notwendigen automatisierten Spellcheck und dem
manuellen Durchlesen korrigiert werden.
### Grob
Ein Text wird von einem File eingelesen und in Zeilen und dann in Woerter
zerlegt.
#### *`procWord`*
Fuer jedes Wort wird
- geprueft ob es eine Kombination ae, oe, ue enthaelt
- ob es nicht eine Teil einer erlaubten Gruppe enthaelt und darum nicht
  umgewandelt werden darf (zb. koeff, neue, Poet, Poes)
- im wort wird (sofern erlaubt) der umlaut ersetzt (Papitalisierung muss erhalten
  bleiben!)
## Erweiterung
Der Input kann in einem Format sein, das Pandoc versteht.
Die Datei kann dann in den Abstrac Syntax Tree von Pandoc gelesen werden und
dieser nach der Umwandlung wieder ausgegeben werden.
[Doc](https://pandoc.org/using-the-pandoc-api.html)
Es sollte dann genuegen, den AST zu 'walk' und alle 'inline' die `Str Text`
Struktur haben, zu mit obiger prozedur `procWord` zu untersuchen.
## Plan
### `procWord` bauen und testen
- Erstelle eine TestInputWord der aus einer Sammlung typischer Faelle besteht und
  baue ein einfaches Ersetzungsprogramm.
- Test mit kapitalisierten Woertern.
- Erstelle eine Liste von Ausnahmen und ersetze diese nicht, erweitere testFaelle
### Wrap `procWord` in a txt file reader
- test
### wrap `procWOrd` in reader for pandoc
- read markup into AST
- `walk` AST und wende `procWord` auf alle `Inline` an.
- test case 
- gib als markdown aus.
