# Revision history for tasty-sugar

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
