# Morgue - a markdown based orgmode replacement
EMACS isn't for everyone - for instance, vim users often dislike the experience of using
it. However, there a lot of features in emacs that can't be found elsewhere. One such
feature is orgmode, whose main functionality I have recreated here.

The following features are suppported:

* TODO's.
* Sectioning
* Timestamps (with repetition intervals)
* Agenda generation

Almost all of these are already present in markdown's more widely understood and easier
syntax, or at least in syntactic extensions.

[vim-markdown](http://www.github.com/gabrielelana/vim-markdown) provides
a lot of the functionality I need for the editing task, namely:

* easy editing (this is vim after all)
* better syntax highlighting
* checkboxes via `Space`

Yet, whereas TODO's and sectioning are replaced by the plugin linked above and markdown's
syntax, the last two points above are much harder to replace.

First, I tried to create a plugin that would integrate with the above linked, but due to
the recursive nature of syntax highlighting this wasn't feasible.  Thus, I forked it and
included some functionality regarding timestamps, syntax highlighting included.

**Here it is:** [vim-markdown (fork)](http://www.github.com/ibabushkin/vim-markdown)
See it's README for more details on usage and features.

## The features
* the agenda generator supports multiple modes:
  * **Timed**: displays the agenda for a custom number of days
  * **Todo**: displays all TODO's without assigned times
  * **Both**: a combination of the above
* and multiple output formats:
  * **colored**: ANSI colored output on your terminal
  * **pango**: Same for GTK-apps and awesomeWM-notifications, dunst, to name just a few
  * **plain**: plain, non-colored text (ideal for further processing)

And it's quite fast, due to the fact that it's written in Haskell.

## Agenda format
An agenda is essentially a set of trees that get generated using a specific set of rules.
The tree is then formatted according to the format specified by the user, or using a
directory of mustache templates that are compiled at runtime. The predefined formats are
implemented using the same templating system, based on mustache, and can be used as an
example for user-defined templates.

## Rules for agenda generation
* Timestamps are in the format presented below (used by the vim plugin, too), see examples.
```
[DD.MM.YYYY:HH:MM/+n{d,w,m,y}]
           \-+--/\-+--------/
  optional --'     `-- optional
```
* See [https://github.com/ibabushkin/morgue/blob/master/tests/test.md](test.md) for an
  overview of the syntax.

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
be included in due time.

# Installation
Just do the following, assuming you have `ghc` and `stack` installed:
```
$ git clone morgue
$ cd morgue
$ stack install
``` 
Then add `~/.stack/local/bin` to your `$PATH` if you haven't already.
Now run `morgue -h` for usage information.

# TODO
See the issue tracker for ongoing work.
