# Morgue - a markdown based orgmode replacement

To a Vim user like me, EMACS is very complex, bloated and a generally
unpleasant experience. Yet, one feature that is provided by EMACS made
me use it: orgmode. It is incredibly useful and intuitive for note taking.

After some time I realized that I only use a small subset of it's functionality,
which roughly consists of the following:

* TODO's.
* Sectioning
* Timestamps (with repetition intervals)
* Agenda generation
* Outlining (if I need it)

Almost all of these are already present in markdown's much cleaner and
easier syntax, at least in syntactic extensions.

[vim-markdown](http://www.github.com/gabrielelana/vim-markdown) provides
a lot of the functionality I need, namely:

* easy editing (this is Vim after all)
* better syntax highlighting
* checkboxes via `Space`

Yet, whereas TODO's and sectioning are replaced by the plugin linked above
and markdown's syntax, the last two points above are much harder to replace.

First, I tried to create a plugin that would integrate with the above linked,
but due to the recursive nature of syntax highlighting this wasn't feasible.
Thus, I forked it and included some functionality regarding timestamps, syntax
highlighting included.

**Here it is:** [vim-markdown (fork)](http://www.github.com/ibabushkin/vim-markdown)
See it's readme for more details on usage and features.

## The features

* the agenda generator supports multiple modes:
  * **Timed**: displays the agenda for a custom number of days
  * **Todo**: displays all TODO's without assigned times
  * **Both**: a combination of the above
* and multiple output formats
  * **ANSI**: colored output on your terminal
  * **Pango**: Same for GTK-apps and awesomeWM-notifications to name just a few
  * **Plaintext**: plain, non-colored text (ideal for further processing)

* we've also got an outline generator:
  * supports all the formats presented above
  * supports multiple files as well

And it's quite fast, due to the fact that it's written in Haskell.

### A screenshot
![ansi agenda and vim](http://www.github.com/ibabushkin/morgue/raw/master/ansi_agenda_and_vim.png)

## Rules for agenda generation

* Timestamps are in the format presented below (used by the Vim plugin, too), see examples
```
[DD.MM.YYYY:HH:MM/+n{d,w,m,y}]
          \--+-/\-+--------/
  optional --+    +-- optional
```
* Every list element with a checkbox and/or timestamp is included in the agenda

### Examples

* `[21.08.2015/+1w]`: From the 21st August 2015 on, every week
* `[21.08.2015]`: only on that day
* `[21.08.2015:18:00]`: same day, exact time
* `[21.08.2015:18:00/+2m]`: every two months from that day on, at 6 pm.
* Supported repetition intervals are `d` (Day), `w` (Week), `m` (Month) and `y` (Year),
  prepended by any integer.

**BUT:** The more steps the current date is away from the original date in a timestamp
with a repetition interval set, the more computing power is required for a check,
a small utility that changes timestamps to the most recent valid timestamp will probably
be included. 

# Installation

Just do the following, assuming you have `ghc` and `cabal` installed:
```
$ cd morgue/agenda
$ cabal install
``` 
Then add `~/.cabal/bin` to your `$PATH` if you haven't already.
Now run `morgue -h`/`morgue-outline -h` for usage information.

# TODO

* improve README with images / screenshots
