# Revision history for tasty-sugar

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
