# Revision history for tasty-sugar

## 1.3.0.1 -- 2022-06-29

 * Bump optparse-applicative upper bound to allow 0.17 series.

## 1.3.0.0 -- 2022-06-28

 * Add `inputDirs` to `CUBE` to replace single `inputDir`.  Allows test action
   files to be spread across various directories.  The `inputDir` is still
   supported for backward-compatibility, but the `inputDirs` is preferred.

 * Any input directory can be specified with a trailing asterisk
   (e.g. "test/data/*") in which case all subdirectories of that root directory
   will be scanned.

   - Any file (root, expected, or associated) may come from any directory (the
     files in a single Sweet Expectation do not all need to occur in
     the same subdirectory).

   - The parameter matching will also consider subdirectory names (prioritized
     over filename parameter matches).

 * `SweetExplanation` `results` field is a single entry instead of an array.

 * Added `getParamVal` function to extract the value (if any) from any
   `ParamMatch`.

 * Fixed some bugs in filename analysis and parameter determination.

## 1.2.0.0 -- 2022-05-16

 * Update for logict 0.8.0.0 breaking change.
 * Fix small issues in example usage haddock.

## 1.1.1.0 -- 2021-04-19

 * Use 'kvitable' to render `--showsearch` output.
 * Update build warnings and refactor code to remove potential partial
   results.
 * Fix tasty ingredients option double-defaulting.
 * Add dependency constraint bounds.
 * Update haddock usage for usage changes.

## 1.1.0.0 -- 2021-01-31

 * Allow multiple tests to be generated for each `Expectation` via
   `withSugarGroups` function.  The `withSugarGroups` third argument
   function (the test generator) now returns a list instead of a
   single test.  While roughly the same effect could have been
   achieved by the test generator function using a `testGroup` to wrap
   multiple tests, this change allows both (a) multiple tests to be
   generated without requiring another level in the test heirarchy,
   and (b) the generator can return an empty list if there should not
   be any tests generated for the specified `Expectation`.

## 1.0.2.0 -- 2021-01-31

 * Export `paramMatchVal` utility function

 * Fix over-trimming of `Expectation` matches

 * Fix identification ranking of associated files


## 1.0.1.1 -- 2021-01-18

 * Fix error where an expected set of matches could match multiple
   root names where the root names contain separators.

## 1.0.1.0 -- 2021-01-18

 * Associated files are now ranked based on the number of parameter
   components in the name and only the highest number of matches are
   provided.  Previously all possible matches were supplied, which
   meant that "more generic" associations caused an Expectation in
   addition to the "more specific" associations.  Now only the "more
   specific" assocations cause an Expectation.

 * Better indentation on Expectation information for --showsearch.

## 1.0.0.0 -- 2021-01-14

 * Allow multiple CUBE inputs for a single test session; Sweets are a semigroup.

 * Run the withSugarGroups mkSweets in MonadIO instead of pure.

 * Remove blank lines in --showsearch output.

## 0.2.0.0 -- 2021-01-12

 * Renamed CUBE "source" to "rootName"

 * Updated Sweets structure to show root match base and match name

 * Rewritten implementation using Logic capabilities.  Clarified many
   corner cases and fully implemented all logic.

* Significantly enhanced testing.

 * Updated documentation.

## 0.1.0.0 -- 2019-12-24

* Initial version.
