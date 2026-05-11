# Test Migration TODO

- [ ] Test `Between`
  - [ ] Verify that constructing `Between(a, b)` succeeds when the two keys are distinct
  - [ ] Verify that a successful `Between(a, b)` preserves both original keys on the resulting domain object
  - [ ] Verify that constructing `Between(a, a)` fails with `DuplicateBetweenKeys`
