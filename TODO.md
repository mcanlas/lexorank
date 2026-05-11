# Test Migration TODO

- [ ] Test `Between`
  - [ ] Verify that constructing `Between(a, b)` succeeds when the two keys are distinct
  - [ ] Verify that a successful `Between(a, b)` preserves both original keys on the resulting domain object
  - [ ] Verify that constructing `Between(a, a)` fails with `DuplicateBetweenKeys`
- [ ] Test `ChangeRequest`
  - [ ] Verify that constructing `ChangeRequest(id, req)` succeeds when `id` is not one of the request keys
  - [ ] Verify that a successful `ChangeRequest(id, req)` preserves both the original id and the original position request
  - [ ] Verify that constructing `ChangeRequest(id, req)` fails with `DuplicateChangeKeys` when `id` is already one of the request keys
