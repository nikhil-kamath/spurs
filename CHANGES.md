## v0.1.1 Matrix/Vector multiplication and addition

#### New functionalty:

- Added new functions in `ops.ml`:
  - `add_v`
  - `dot_v`
  - `mult`
  - `add`
- Added new functions in `csmat.ml`:
  - `append`
  - `expand`
- Added new functions in `csvec.ml`:
  - `of_dense`
  - `scale`

#### Other Changes:

##### New Property-based testing suite

Several of the new and existing functions are now extensively tested with property-based tests in `QCheck2`. These have found bugs in the existing functions which are now fixed.

## v0.1.0 Initial commit
