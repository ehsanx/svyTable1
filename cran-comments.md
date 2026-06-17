## Resubmission

This is a resubmission. In response to the previous review, the three requested
changes have been made:

1. **References in the `Description` field.** Method references are now given in
   the `Description` field in the requested form, e.g. Lumley (2004)
   <doi:10.18637/jss.v009.i08>, Archer and Lemeshow (2006)
   <doi:10.1177/1536867X0600600106>, van Buuren and Groothuis-Oudshoorn (2011)
   <doi:10.18637/jss.v045.i03>, with no space after `doi:` and angle brackets
   for auto-linking.

2. **`\value` tags.** `pipe.Rd` now has a `\value` entry, and every exported
   function's `\value` describes the structure (class) and meaning of the
   returned object.

3. **`\dontrun{}`.** All previously `\dontrun{}` examples have been converted to
   runnable examples or `\donttest{}` and rewritten onto the bundled
   `nhanes_mortality` dataset so they execute without the suggested `NHANES`
   package.

## Test environments

* Local: Windows 11, R 4.5.1
* (Before submission, also run: win-builder devel and release; R-hub.)

## R CMD check results

`R CMD check --as-cran --run-donttest`: 0 errors | 0 warnings | 1 note.

The single NOTE is the expected "New submission" note. (During development the
version carries a `.9000` development suffix; set a normal release version,
e.g. `0.15.0`, before submitting.)

All examples and vignettes run on the bundled `nhanes_mortality` dataset and do
not require any Suggests package, so the package checks cleanly with the
Suggests packages absent as well as present.
