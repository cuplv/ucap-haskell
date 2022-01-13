This library provides replicated datastore transactions based on Update Capabilities in Haskell, along with Update Capability domains for common datatypes and containers.

Domains include:

* `ConstC`, for setting new state values (on any state type).
* `CounterC`, for addition and multiplication on numeric state values.
* `EitherC`, for modifying and case-switching on sum-type state values.
* `MapC`, for inserting, deleting, and modifying entries in a Map state.

Many instances are combinators.
For example, `MapC String (CounterC Int)` defines capabilities for both inserting and deleting entries in a String-to-Int map, and for modifying existing entries using addition and multiplication.
