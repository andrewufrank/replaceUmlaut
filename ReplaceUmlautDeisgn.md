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

Die Verarbeitung ist pro Datei, die in einem Durchgang ohne Interaktion erfolgt.
Die Verarbeitung ist entweder einfach fuer eine Text Datei oder fuer ein
Format, das Pandoc lesen und schreiben kann (z.B. Markdown, Latex, Doc).

## Verarbeiten Text-Dateien
Ein Text wird von einem File eingelesen und in Zeilen und dann in Woerter
zerlegt. Die Verarbeitung erfolgt in drei Schritten (das sind hier Schichten).
### *`procWord`* und `procLine`
Fuer jedes Wort wird
- geprueft ob es eine Kombination ae, oe, ue enthaelt
  achtung: Umwandeln auch Ae und AE etc.
- ob es nicht eine Teil einer erlaubten Gruppe enthaelt und darum nicht
  umgewandelt werden darf (zb. koeff, neue, Poet, Poes)
- im wort wird (sofern erlaubt) der umlaut ersetzt (Papitalisierung muss erhalten
  bleiben!)
  - zusammensetzungen: haeufige woerter in zusammensetzungen (auto, micro)
  koennen mit autoe und microe erfasst werden.
  die faelle fuer ue sind hoffnungslos und muessen langsam aufgebaut
  werden (total 30,000 erlaubte woerter mit ue)
### *`procTxt`*
Ein Text in einer Datei wird eingelesen (die Datei zur Sicherung auf *.bak
umbenannt) und der Text verarbeitet; das Ergebnis wird in den File
zurueck geschrieben.  Die Liste der erlaubten Woerter wird ebenfalls von einer
Datei eingelesen.
Der Text wird in Woerter zerlegt und diese einzeln behandelt.
Die Dateinamen sind fuer `ProcTxt` absolut. `procTxt` erwartet die Liste
der erlaubten Woerter und den Eingabe Datei Namen.

`procText` sollteidempotent sein - es wird nichts mehr geaendert.
Man koennte auch Testen, ob von der Liste der Woerter mit erlaubten
ae, oe und ue kombinationen keine veraendert werden (sonst allenfalls die
Liste ergaenzen).
## Verarbeitung Markdown und aehnliches
Der Input kann in einem Format sein, das Pandoc versteht.
Die Datei kann dann in den Abstrac Syntax Tree (AST) von Pandoc gelesen werden und
dieser nach der Umwandlung wieder ausgegeben werden.
[Doc](https://pandoc.org/using-the-pandoc-api.html)
Es sollte dann genuegen, den AST zu 'walk' und alle 'inline' die `Str Text`
Struktur haben, zu mit obiger prozedur `procWord` zu untersuchen.
### ProcPandoc
Liest eine md datei ein und erzeugt den AST.

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
