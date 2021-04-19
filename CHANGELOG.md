# Revision history for tasty-sugar

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
