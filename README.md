# Evil Hangman

Implementation of a cheating Hangman game in Elm.
It will try its best to make you loose.

[Play it live here](https://yblein.github.io/evil-hangman).

## How it works

Instead of starting by picking a random word, the game maintain a dictionary of possible words at each step.
The idea is to keep this dictionary as large as possible so that its can always elimate the words that would make you win.

Every time you try to guess a new letter, the game partitions its current dictionary of words according to the occurences of this letter in each word.
Then it restricts the dictionary to the largest partition of words (or the one that gives you less information in case of tie) and shows you the letter occurences of the partition it selected, much like a normal hangman game where you would have guessed a correct letter.

## Build and run

You'll need Elm 0.17. Then call:

```
$ elm-make --output elm.js EvilHangman.elm
```

and then browse to `index.html`.
