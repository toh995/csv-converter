# csv-converter
This is a small utility to convert plain-text to csv-format, for one of my documents.

## Usage
First, copy the text to your clipboard.

Then, run:
```bash
cabal update
cabal build
xsel -b | cabal run > foo.csv
```
