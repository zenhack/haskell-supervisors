[![hackage][hackage-img]][hackage]

# Haskell Supervisors

The `supervisors` package provides a useful abstraction for managing the
groups of Haskell threads, which may not have a strictly hierarchical
structure to their lifetimes.

One way to think of it is that `supervisors` is to [async][async] as
[resourcet][resourcet] is to [bracket][bracket].

Most of the time you can manage these things in a hierarchical manner:
for bracket, acquire a resource, do stuff with it, and release it. For
async, spawn some tasks, wait for some or all of them, maybe kill the
remaining ones, and return. The memory used by all of these threads is
not reclaimed until the entire subtree finishes.

But sometimes, your concurrency patterns don't fit neatly into a tree;
that is what this package is for.

This package was originally written for use in the rpc layer of the
[capnp][capnp] package, where the various threads handling rpc calls
can have essentially arbitrary lifetimes, but we often want to make
sure they are all shut down when a connection is closed.

Concretely, the library provides a `Supervisor` construct, which can be
used to safely spawn threads while guaranteeing that:

* When the supervisor is killed, all of the threads it supervises will be
  killed.
* Child threads can terminate in any order, and memory usage will always
  be proportional to the number of *live* supervised threads.

[async]: https://hackage.haskell.org/package/async
[bracket]: http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception-Base.html#v:bracket
[resourcet]: https://hackage.haskell.org/package/resourcet
[capnp]: https://hackage.haskell.org/package/capnp

[hackage-img]: https://img.shields.io/hackage/v/supervisors.svg
[hackage]: https://hackage.haskell.org/package/supervisors
