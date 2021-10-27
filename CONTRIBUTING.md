Thank you for your interest in contributing to this project. We value any input you may have, as this helps towards making the package even more useful.

Please read the guidelines below for instructions and criteria on filing issues, making code changes, and submitting PRs.

# Filing issues
* Issues are intended for ideas, suggestions, feature requests, and bug reports. Don't submit an issue for a coding question or support. Use the [Discussions](https://github.com/philips-software/latrend/discussions) area instead.
* Please check the existing issues to see whether your suggestion/feature request/bug has already been discussed.
* An issue should address only a single concept/idea/bug.

# Pull Requests (PRs)
1. Every new code feature or bug fix should include one or more automatic tests.
2. One feature or bugfix per PR. Code format or documentation updates should be restricted to either one file, or an object class (e.g., updating the documentation of the `lcModel` class functions).

# Coding style
1. Use ` = ` for assignment, not ` <- `.
2. Don't assign variables within a function call or condition. E.g., don't do `median(x <- 5)` or `if (x <- y + 2 == 3)`.
3. 2-space indentation. No tabs.
4. Surround the `=` of function calls with named arguments with a space, e.g., `sample.int(n = 1e3, size = 10, replace = TRUE)`.
5. Use `'` for strings, e.g., `var = 'abc'`
6. Use `L` suffix for integers.

As some of these coding styles were introduced later into the project, not all current code may be up to standard. You are invited to submit PRs for formatting improvements.

#### Exception to the coding style
Vignettes and documentation example code use the more popular R coding style. (i.e., using `<-` for assignment and `"` for strings).
