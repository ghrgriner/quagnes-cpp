# quagnes: A Program for Solving Agnes Solitaire.

## Summary
This program solves Agnes (Agnes Sorel) solitaire card games. It can be
used to solve games having the rules implemented in the GNOME AisleRiot
package and the rules attributed to Dalton in 1909 and Parlett in 1979
among others [1–3] and to calculate win rates.

Users can simulate random games and calculate win rates under various
permutations of rules, including moving sequences (runs) by same-suit or
same-color, allowing or not allowing movable runs to be split in the middle of the run
for a move, dealing all tableau cards face-up at the start or dealing
only the final tableau card in each column face-up, and whether and how empty
columns can be filled in between deals. The program provides additional
options for debugging and tuning of the search algorithm to be more
memory-efficient at the expense of speed.

In 1979 Parlett named the two main variants of Agnes as Agnes Sorel (the
variant / set of variants described here) and Agnes Bernauer (a variant/set
of variants that uses a reserve) [3]. This program only considers Agnes
Sorel.

## Preparation
Decks must be created before running the program and be stored in files in a
directory structure named `decks/oM/deckN.txt`, etc..., where N is a number
starting at 1 and M = floor(N/1000). See
[Program Input and Output](wiki/Program-Input-and-Output#program-input)
for additional details.

## Example
Play 100 games using the rules given by Dalton [1,2].
```
> ./quagnes -n 100 -e none -u -f
```

## Program Output
Results are printed to standard output with `rep_id` equal to the `N` from
the deck filename `deckN.txt` and return code (`rc`) which indicates whether a
game is winnable (`rc=1`), not winnable (`rc=2`), or terminated without a result
after exceeding the number of game states specified by the `-m` parameter.
(`rc=3`). See
[Program Input and Output](wiki/Program-Input-and-Output#program-output)
for additional output columns and messages printed to standard error.

## Game Rules, Program Methodology, and Analysis of Win Rates
Further details on the game rules, program methodology, and analysis of win
rates can be found
[in the wiki](wiki/Rules,-Methodology,-and-Analysis-of-Win-Rates).

## References
[1] Agnes (card game). Wikipedia.
   https://en.wikipedia.org/wiki/Agnes_(card_game). Retrieved
   March 15, 2024.

[2] Dalton W (1909). "My favourite Patiences" in The Strand Magazine,
    Vol 38.

[3] Parlett D (1979). The Penguin Book of Patience. London: Penguin.

