This library provides a definition for Update Capabilities in Haskell, along with instances for common datatypes and containers.

Instances include:

* `Data.UCap.Const`, for setting new state values.
* `Data.UCap.Counter`, for addition and multiplication on numeric values.
* `Data.UCap.Either`, for modifying and case-switching on sum-type values.
* `Data.UCap.Map`, for inserting, deleting, and modifying Map entries.

Many instances are combinators.
For example, `MapC String (CounterC Int)` defines capabilities for both inserting and deleting entries in a String-to-Int map, and for modifying existing entries using addition and multiplication.
