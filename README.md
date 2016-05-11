#git-details

This small(ish) project is a command line tool, written in Haskell, to collect information from commits of a repository and compile it into easy-to-use files or (maybe, at some point) graphs.

What does this look like? Well suppose you run `cloc` (count lines of code) using a `pre-commit` hook and log the results to a file `cloc.txt`. Then, once the project is slightly more mature,
you'll be able to run a simple command, such as `git-details --cloc=cloc.txt --out=graphs.html` to get a nice collection of graphs.

Because this project is written in Haskell and utilizes [attoparsec](https://hackage.haskell.org/package/attoparsec) to process files, it should be relatively easy to extend. There will be a 
template where you plug in your filetype, parser, and command name to get an option of the form `--[myOption]=[fileToTrack]`, which will then be displayed in the compilation of stats.

Although it's hardly best practice, I will implement a method to perform analysis when most of the project does NOT have the file to track, meaning that it will run some program/script on 
_every_ commit of the repository. This is not the ideal use-case, as it should be used with a pre-commit hook or some other method to ensure the files to be tracked are updated regularly.
