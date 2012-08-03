#csv2tex

csv2tex is a program that translates a `.csv` file into a (La)TeX style table, where cells are separated by a `$` and rows ended by a `\\`.

The resulting cells are padded with whitespace, so the columns are straight.

##Usage

```
	csv2tex csvfile
```

The output is written to `stdout`.

##Shortcomings/TODO

 - No effort have been put into error messages, but since I'm using parsec, the parser might spit out something sensible.
 - When parsing files with only one column, an empty line will be interpreted as an empty cell. This means that if there is any succeeding whitespace (which some text editors, like Vim adds), it too will be interpreted as an empty cell.
