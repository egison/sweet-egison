# sweet-egison

[![Actions Status](https://github.com/egison/sweet-egison/workflows/latest/badge.svg)](https://github.com/egison/sweet-egison/actions?workflow=latest)
[![Actions Status](https://github.com/egison/sweet-egison/workflows/release/badge.svg)](https://github.com/egison/sweet-egison/actions?workflow=release)
[![Hackage](https://img.shields.io/hackage/v/sweet-egison.svg)](https://hackage.haskell.org/package/sweet-egison)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/sweet-egison.svg)](http://packdeps.haskellers.com/reverse/sweet-egison)

The [sweet-egison](https://hackage.haskell.org/package/sweet-egison) is a shallow embedding implementation of non-linear pattern matching with extensible and polymorphic patterns [1]. In other words, this implements [Egison](https:///www.egison.org) pattern matching in Haskell by desugaring pattern expressions. This library provides a base of the Pattern-Match-Oriented (PMO) programming style [2] for Haskell users at a practical level of efficiency.

## Getting started

We code the equivalent pattern match of `case [1, 2, 3] of x : xs -> (x, xs)` in this library as follows:

```haskell
> matchAll [1, 2, 3] (List Something) [[mc| $x : $xs -> (x, xs) |]]
[(1,[2,3])]
```

Here, we can only observe the small syntactic difference in pattern expressions: the variable bindings are prefixed with `$`. (We'll come back to `List Something` later.)
You may notice that `matchAll` returns a list. In our library, pattern matching can return many results. See the following example that doubles all elements in a list:

```haskell
> take 10 $ matchAll [1 ..] (List Something) [[mc| _ ++ $x : _ -> x * 2 |]]
[2,4,6,8,10,12,14,16,18,20]
```

`++` is the *join* operator that decomposes a list into an initial prefix and the remaining suffix. We can implement `map` with pattern matching using this:

```haskell
> map f xs = matchAll xs (List Something) [[mc| _ ++ $x : _ -> f x |]]
> map (*2) [1,2,3]
[2,4,6]
```

Note that we don't see any recursions or `fold`s in our `map` definition! An intuition of `map` function, that applies the function to all elements, are expressed directly in the pattern expression.

### Matchers

Because our pattern matching can return many results, we can use it to decompose *non-free data types* such as multisets and sets. For example:

```haskell
> matchAll [1, 2, 3] (Multiset Something) [[mc| $x : $xs -> (x, xs) |]]
[(1,[2,3]),(2,[1,3]),(3,[1,2])]
```

We use `Multiset Something` instead of `List Something` here to match the target `[1, 2, 3]` as a multiset. These parameters such as `Multiset Something`, `List (List Something)`, and `Something` are called *matchers* and specify pattern-matching methods. Given a matcher `m`, `Multiset m` is a matcher for multisets that matches its elements with `m`. `Something` is a matcher that provides simple matching methods for an arbitrary value.

### Controlling matching strategy

Some pattern matching have infinitely many results and `matchAll` is designed to be able to enumerate all the results. For this purpose, `matchAll` traverses a search tree for pattern matching in the breadth-first order. The following example illustrates this:

```haskell
> take 10 $ matchAll [1 ..] (Set Something) [[mc| $x : $y : _ -> (x, y) |]]
[(1,1),(2,1),(1,2),(3,1),(1,3),(2,2),(1,4),(4,1),(1,5),(2,3)]
```

We can use the depth-first search with `matchAllDFS`.

```haskell
> take 10 $ matchAllDFS [1 ..] (Set Something) [[mc| $x : $y : _ -> (x, y) |]]
[(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10)]
```

In most cases, the depth-first search is faster than the default breadth-first search strategy. It is recommended to always use `matchAllDFS` if it is OK to do so.

With `matchAllDFS`, we can define an intuitive pattern-matching version of `concat` function on lists.

```haskell
> concat xs = matchAllDFS xs (List (List Something)) [[mc| _ ++ (_ ++ $x : _) : _ -> x |]]
> concat [[1,2], [3,4,5]]
[1,2,3,4,5]
```

### Non-linear patterns

The non-linear pattern is another powerful pattern-matching feature. It allows us to refer the value bound to variables appear in the left side of the pattern. We provide a pattern syntax named value patterns in the form of `#e`. The `EqM` matcher enables value patterns to match with targets that are equal to the corresponding expression. For example, the following example enumerates (p, p+2) pairs of primes:

```haskell
> import Data.Numbers.Primes ( primes )
> take 10 $ matchAll primes (List EqM) [[mc| _ ++ $p : #(p + 2) : _ -> (p, p+2) |]]
[(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),(59,61),(71,73),(101,103),(107,109)]
```

We can implement a pattern-matching version of set functions such as `member` and `intersect` in a declarative way using non-linear patterns. Match clauses are monoids and can be concatenated using `<>`.

```haskell
> member x xs = matchDFS xs (Multiset Eql) [[mc| #x : _ -> True |], [mc| _ -> False |]]
> member 1 [3,4,1,4]
True
> intersect xs ys = matchAllDFS (xs, ys) (Pair (Set Eql) (Set Eql)) [[mc| ($x : _, #x : _) -> x |]]
> intersect [1,2,3] [4,5,3,2]
[2,3]
```

### Further readings

Some practical applications of PMO such as a [SAT solver](https://github.com/egison/sweet-egison/blob/master/example/cdcl.hs) are placed under [example/](https://github.com/egison/sweet-egison/blob/master/example/). Detailed information of Egison, the original PMO language implementation, can be found on [https://www.egison.org/](https://www.egison.org/) or in [1]. You can learn more about pattern-match-oriented programming style in [2].

## Implementation / Difference from miniEgison

[miniEgison](https://github.com/egison/egison-haskell) is also a Haskell library that implements Egison pattern matching. The main difference from [miniEgison](https://github.com/egison/egison-haskell) is that sweet-egison translates pattern matching into Haskell control expressions (shallow embedding), where [miniEgison](https://github.com/egison/egison-haskell) translates it into Haskell data expressions (deep embedding).

Our quasi-quoter `mc` translates match clauses into functions that take a target and return a non-deterministic computation as `MonadPlus`-like monadic expression. As `MonadPlus` can express backtracking computation, we can perform efficient backtracking pattern matching that is essential to PMO programming on it.

For example, `[mc| $xs ++ $x : $ys -> (xs, x, ys) |]` is translated as follows:

```haskell
\tgt ->
  join tgt >-> \(xs, d0) ->
    cons d0 >-> \(x, ys) ->
      pure (xs, x, ys)
```

`:` and `++` are synonyms of `cons` and `join` respectively, and desugared in that way during translation. Here, pattern constructor names such as `join` and `cons` are overloaded over matchers of collections to archive the ad-hoc polymorphism of patterns.

## Bibliography

- [1] Egi, Satoshi, and Yuichi Nishiwaki. "Non-linear pattern matching with backtracking for non-free data types." Asian Symposium on Programming Languages and Systems. Springer, Cham, 2018.
- [2] Egi, Satoshi, and Yuichi Nishiwaki. "Functional Programming in Pattern-Match-Oriented Programming Style." arXiv preprint arXiv:2002.06176 (2020).
