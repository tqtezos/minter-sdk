# Description

<!--
Please include a summary of the change and which issue(s) are fixed,
including relevant motivation and context.
-->

# Checklist

## Docs

- [ ] Each entrypoint and storage field is documented
- [ ] All errors and failures are documented

## Tests

- [ ] There are property tests for each entrypoint
    - [ ] There is a succeeding example for each
    - [ ] There is a failing example for each
        - [ ] If no failure is possible without an out-of-gas or other
                protocol-level error, there is a property test asserting this
    - [ ] When using `cleveland`, some test cases are generated using `Arbitrary`

- [ ] There is an integration test for each happy path in the docs, i.e. for
        each documented feature

- [ ] Administrator tests
    - [ ] Vs. unknown address
    - [ ] Vs. known address (i.e. in storage)
    - [ ] Vs. contract's own address (`SELF_ADDRESS`)
    - [ ] Can any administrators be updated?
        - [ ] Are new administrators required to confirm their role?
            - [ ] Administrators retain their privilege
                    until new administrators are confirmed
        - [ ] New administrators replace the current ones

- [ ] Authorization flaws
    - [ ] Doesn't use `SOURCE` for authorization
    - [ ] Uses tickets in case a sender can be tricked into issuing a malicious request

- [ ] UX tests
    - [ ] Errors whose reasons are not clear to the user or whose details are
          difficult to derive off-chain, must be documented with a method to
          derive those additional details.  For example:
          `INSUFFICIENT_BALANCE` → `(INSUFFICIENT_BALANCE, (<required>, <present>))`

- [ ] CI checks
    - [ ] All contracts are included in the list of contracts compiled by CI
    - [ ] Tests on all contracts are executed by CI

## Contract architecture

- [ ] Does the contract interact with other ones?
    - [ ] If your contract accepts or holds a token,
            there exists a series of contract calls to this and any
            depended-upon contracts that results in a token balance of zero.
            In other words, it's possible to transfer the entire balance to
            another address.

    - [ ] TZIP standards
        - [ ] Is the contract FA2 compliant?
            - [ ] Does it comply with optional requirements as well?
        - [ ] Does the contract support TZIP-16 metadata?
            - [ ] An example/template metadata is provided

    - [ ] Are there `view` entrypoints and/or can the
            contract call one of it's own entrypoints?
        - [ ] There are tests to cover the possibility of your contract calling itself

- [ ] Does not use [Michelson anti-patterns](https://tezos.gitlab.io/developer/michelson_anti_patterns.html), in particular:
    - [ ] If `CHECK_SIGNATURE` is used, replay attacks are prevented
    - [ ] Are there strict structures (`list`, `set`, `map`) that
            can grow in the storage?
        - [ ] Is there a strict limit to their growth?
        - [ ] Can their size grow without moderation?
            - [ ] This is tested to ensure that entrypoints
                    execution's gas cost remains acceptable
            - [ ] No unprivledged user may trigger unmoderated
                    growth of such a structure

