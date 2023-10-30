# knighterrant

This program produces solutions for a nested semimagic knight's tour. This is a [knight's tour](https://en.wikipedia.org/wiki/Knight%27s_tour) on an 8x8 board whose numbered steps produce a [semimagic square](https://en.wikipedia.org/wiki/Magic_square) and whose four 4x4 quadrants are each themselves semimagic squares.

The problem appeared in a puzzle called "Knight Errant" in The Ultimate Book of Number Puzzles by Kenneth Kelsey. In it, Kelsey states he could only find four solutions (ignoring rotations and reflections), of which he presents incomplete versions in the book inviting the reader to complete them. This program shows there are four more solutions.

More information about the problem and the algorithm design is at https://greem.co.uk/knighterrant/

## Compiling

The knighterrant program has been compiled on Linux with GCC and libpthread.

Assuming you have gcc, glibc and libpthread-dev installed, you can build the program using `make`:

```
make knighterrant
```

The `knighterrant` binary will be written to the same directory as the source.

In addition, you can `make` the binaries `ketest5` and `ketest6`, which count the number of general knight's tours on a 5x5 and 6x6 board for testing purposes, and `knighterrant-any-start-and-end`, which solves the same problem without imposing restrictions on where the tour can start and end.

## Running

You can run the program without arguments:

```
./knighterrant
```

By default it will use four worker threads. If you like, you can increase this with the `-t` option:

```
# Run with 12 worker threads
./knighterrant -t 12
```

The program finds eight solutions, which are written to standard output and formatted with the Unicode box drawing characters:

```
Found tour #1...
┌──┬──┬──┬──┬──┬──┬──┬──┐
│ 1│48│31│50│33│16│63│18│
├──┼──┼──┼──┼──┼──┼──┼──┤
│30│51│46│ 3│62│19│14│35│
├──┼──┼──┼──┼──┼──┼──┼──┤
│47│ 2│49│32│15│34│17│64│
├──┼──┼──┼──┼──┼──┼──┼──┤
│52│29│ 4│45│20│61│36│13│
├──┼──┼──┼──┼──┼──┼──┼──┤
│ 5│44│25│56│ 9│40│21│60│
├──┼──┼──┼──┼──┼──┼──┼──┤
│28│53│ 8│41│24│57│12│37│
├──┼──┼──┼──┼──┼──┼──┼──┤
│43│ 6│55│26│39│10│59│22│
├──┼──┼──┼──┼──┼──┼──┼──┤
│54│27│42│ 7│58│23│38│11│
└──┴──┴──┴──┴──┴──┴──┴──┘
```
