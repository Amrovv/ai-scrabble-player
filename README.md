# Backwords

A Scrabble-style word game written in Haskell, with an AI player I built to score as high as it can.

It started life as my first year Functional Programming coursework (CS141 at Warwick) — a game where you build words from a rack of letters, each letter worth a Scrabble value. The final part was open ended ("go further"), so I used it to write an AI that plays the game on its own and tries to get the highest total score possible. That's the part I'm most proud of.

## The AI

The interesting bit lives in `src/ScrabbleAI.hs`, mostly `aiMove` and the functions around it.

Instead of just playing the highest scoring word every turn, it looks a few moves ahead using a Monte Carlo approach:

- take the best few candidate words playable from the current rack
- for each one, run a load of random rollouts — play the word, draw new tiles from the bag, then keep greedily playing the best word for a few more turns
- keep whichever candidate had the best average score

Three numbers trade speed against score: how many candidate words to try, how many rollouts per candidate, and how deep to look. I settled on 7 candidates, 15 rollouts and depth 2, which averages around 2600 over 50 games. Pushing the depth higher scores better but gets slow quickly.

## Running it

You'll need [Stack](https://docs.haskellstack.org/).

Build:

    stack build

Play it in the terminal (there's a little TUI):

    stack run

Run the tests — this also plays 50 games and reports the AI's average score, so it takes a while:

    stack test

## Layout

- `src/ScrabbleAI.hs` — my code: game logic and the AI
- `src/Backwords/` — types, game engine and the embedded dictionary
- `app/` — the terminal UI
- `test/` — the test suite
- `assets/words.txt` — the word list

## Credits

The word list in `assets/words.txt` is from InnovativeInventor's [dict4schools](https://github.com/InnovativeInventor/dict4schools/) project (public domain), with very short words removed.

The game engine, terminal UI and tests were provided as coursework skeleton — the code in `ScrabbleAI.hs` is mine.
