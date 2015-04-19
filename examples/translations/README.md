# Translated Examples

These are some scripts that have been translated from Bourne/Bash into Hesh. Each Hesh script tries to retain the style of its original except where it's more natural to write in a functional Haskell style. Included below are some example commands you can try for yourself.

## draw-ruler

Draw a ruler on the terminal.

```
./draw-ruler
```

```
./draw-ruler __x__
```

## vipe

Pipe a string through an editor.

```
echo "Edit me." | ./vipe | cat
```

```
./vipe | cat
```

## day

Show the date of a given day.

```
./day wed
```

```
./day wed last
```
