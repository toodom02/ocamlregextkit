## v1.0.2 (2024-09-29)

### Fix

 - `get_reachable_states` missing epsilon transitions (#8).

## v1.0.1 (2024-04-15)

### Fix

 - Error handling for DFA `product_construction` over different alphabets (#6).

## v1.0.0 (2023-08-22)

### Added

 - Removed extraneous fields from ADT and enforced single source of truth.

 - Use inplace mutations of automata for functions `prune`, `merge_alphabets` and `minimise`.

## v0.2.0 (2023-08-05)

### Added

 - Updated ADT from `List` to `Hashtbl`
    - Improves performance by a linear factor for automata traversal.


## v0.1.1 (2023-07-18)

### Fix

 - OCaml v4.14 in dependencies

## v0.1.0 (2023-05-07)

 - Initial release