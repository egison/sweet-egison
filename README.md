# Sweet Egison

[![Actions Status](https://github.com/egison/sweet-egison/workflows/latest/badge.svg)](https://github.com/egison/sweet-egison/actions?workflow=latest)
[![Actions Status](https://github.com/egison/sweet-egison/workflows/release/badge.svg)](https://github.com/egison/sweet-egison/actions?workflow=release)
[![Hackage](https://img.shields.io/hackage/v/sweet-egison.svg)](https://hackage.haskell.org/package/sweet-egison)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/sweet-egison.svg)](http://packdeps.haskellers.com/reverse/sweet-egison)

The [Sweet Egison](https://hackage.haskell.org/package/sweet-egison) is a shallow embedding implementation of non-linear pattern matching with extensible and polymorphic patterns [1].
This library desguars the [Egison](https:///www.egison.org) pattern-match expressions into Haskell programs that use [non-deterministic monads](https://github.com/egison/backtracking).
This library provides a base of the pattern-match-oriented (PMO) programming style [2] for Haskell users at a practical level of efficiency.

## Getting started

We code the equivalent pattern match of `case [1, 2, 3] of x : xs -> (x, xs)` in this library as follows:

```haskell
> matchAll dfs [1, 2, 3] (List Something) [[mc| $x : $xs -> (x, xs) |]]
[(1,[2,3])]
```

Here, we can only observe the small syntactic difference in pattern expressions: the variable bindings are prefixed with `$`. (We'll come back to `List Something` later.)
You may notice that `matchAll` returns a list.
In our library, pattern matching can return many results.
See the following example that doubles all elements in a list:

```haskell
> take 10 $ matchAll dfs [1 ..] (List Something) [[mc| _ ++ $x : _ -> x * 2 |]]
[2,4,6,8,10,12,14,16,18,20]
```

`++` is the *join* operator that decomposes a list into an initial prefix and the remaining suffix.
We can implement `map` with pattern matching using this:

```haskell
> map f xs = matchAll dfs xs (List Something) [[mc| _ ++ $x : _ -> f x |]]
> map (*2) [1,2,3]
[2,4,6]
```

Note that we don't see any recursions or `fold`s in our `map` definition! An intuition of `map` function, that applies the function to all elements, are expressed directly in the pattern expression.

### Matchers

Because our pattern matching can return many results, we can use it to decompose *non-free data types* such as multisets and sets.
For example:

```haskell
> matchAll dfs [1, 2, 3] (Multiset Something) [[mc| $x : $xs -> (x, xs) |]]
[(1,[2,3]),(2,[1,3]),(3,[1,2])]
```

We use `Multiset Something` instead of `List Something` here to match the target `[1, 2, 3]` as a multiset.
These parameters such as `Multiset Something`, `List (List Something)`, and `Something` are called *matchers* and specify pattern-matching methods.
Given a matcher `m`, `Multiset m` is a matcher for multisets that matches its elements with `m`.
`Something` is a matcher that provides simple matching methods for an arbitrary value.
Pattern constructors such as `:` and `++` are overloaded over matchers for collections to archive the ad-hoc polymorphism of patterns.

### Controlling matching strategy

Some pattern matching have infinitely many results and `matchAll bfs` is designed to be able to enumerate all the results.
For this purpose, `matchAll bfs` traverses a search tree for pattern matching in the breadth-first order.
The following example illustrates this:

```haskell
> take 10 $ matchAll bfs [1 ..] (Set Something) [[mc| $x : $y : _ -> (x, y) |]]
[(1,1),(2,1),(1,2),(3,1),(1,3),(2,2),(1,4),(4,1),(1,5),(2,3)]
```

We can use the depth-first search with `matchAll dfs`.

```haskell
> take 10 $ matchAll dfs [1 ..] (Set Something) [[mc| $x : $y : _ -> (x, y) |]]
[(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10)]
```

In most cases, the depth-first search is faster than the default breadth-first search strategy.
It is recommended to always use `matchAll dfs` if it is OK to do so.

With `matchAll dfs`, we can define an intuitive pattern-matching version of `concat` function on lists.

```haskell
> concat xs = matchAll dfs xs (List (List Something)) [[mc| _ ++ (_ ++ $x : _) : _ -> x |]]
> concat [[1,2], [3,4,5]]
[1,2,3,4,5]
```

### Non-linear patterns

The non-linear pattern is another powerful pattern-matching feature.
It allows us to refer the value bound to variables appear in the left side of the pattern.
We provide a pattern syntax named value patterns in the form of `#e`.
The `Eql` matcher enables value patterns to match with targets that are equal to the corresponding expression.
For example, the following example enumerates (p, p+2) pairs of primes:

```haskell
> import Data.Numbers.Primes ( primes )
> take 10 $ matchAll bfs primes (List Eql) [[mc| _ ++ $p : #(p + 2) : _ -> (p, p+2) |]]
[(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),(59,61),(71,73),(101,103),(107,109)]
```

We can implement a pattern-matching version of set functions such as `member` and `intersect` in a declarative way using non-linear patterns.
Match clauses are monoids and can be concatenated using `<>`.

```haskell
> member x xs = match dfs xs (Multiset Eql) [[mc| #x : _ -> True |], [mc| _ -> False |]]
> member 1 [3,4,1,4]
True
> intersect xs ys = matchAll dfs (xs, ys) (Pair (Set Eql) (Set Eql)) [[mc| ($x : _, #x : _) -> x |]]
> intersect [1,2,3] [4,5,3,2]
[2,3]
```

### Further readings

Some practical applications of PMO such as a [SAT solver](https://github.com/egison/sweet-egison/blob/master/example/cdcl.hs) are placed under [example/](https://github.com/egison/sweet-egison/blob/master/example/).
Detailed information of Egison, the original PMO language implementation, can be found on [https://www.egison.org/](https://www.egison.org/) or in [1].
You can learn more about pattern-match-oriented programming style in [2].


## Implementation

Sweet Egison transform patterns into a program that uses non-deterministic monads.
Our quasi-quoter `mc` translates match clauses into functions that take a target and return a non-deterministic computation as `MonadPlus`-like monadic expression.
As `MonadPlus` can express backtracking computation, we can perform efficient backtracking pattern matching.
For example, the match clause `[mc| $x : #(x + 10) : _ -> (x, x + 10) |]` is transformed as follows:
```haskell
    \ (mat_a5sV, tgt_a5sW)
      -> let (tmpM_a5sX, tmpM_a5sY) = (consM mat_a5sV) tgt_a5sW
         in
           ((fromList (((cons (GP, GP)) mat_a5sV) tgt_a5sW))
              >>=
                (\ (tmpT_a5sZ, tmpT_a5t0)
                   -> let x = tmpT_a5sZ in
                      let (tmpM_a5t1, tmpM_a5t2) = (consM tmpM_a5sY) tmpT_a5t0
                      in
                        ((fromList (((cons (GP, WC)) tmpM_a5sY) tmpT_a5t0))
                           >>=
                             (\ (tmpT_a5t3, tmpT_a5t4)
                                -> ((fromList ((((value (x + 10)) ()) tmpM_a5t1) tmpT_a5t3))
                                      >>= (\ () -> pure (x, x + 10)))))))
```
The infix operators `:` and `++` are synonyms of `cons` and `join`, respectively, and desugared in that way during translation.

The `matchAll` function is defined as a function that creates and passes the argument for this non-deterministic monads.
```haskell
matchAll strategy target matcher =
  concatMap (\b -> toList (strategy (matcher, target) >>= b))
```

Consequently, the pattern-match expression
```haskell
matchAll dfs [1, 2, 3, 12] (Multiset Eql)
  [[mc| $x : #(x + 10) : _ -> (x, x + 10) |]]
-- [(2, 12)]
```
is transformed into a program that is equivalent to the following:
```haskell
concatMap (\b -> toList (dfs (Multiset Eql, [1, 2, 3, 12]) >>= b))
    [\ (mat_a5sV, tgt_a5sW)
       -> let (tmpM_a5sX, tmpM_a5sY) = (consM mat_a5sV) tgt_a5sW
          in
            ((fromList (((cons (GP, GP)) mat_a5sV) tgt_a5sW))
               >>=
                 (\ (tmpT_a5sZ, tmpT_a5t0)
                    -> let x = tmpT_a5sZ in
                       let (tmpM_a5t1, tmpM_a5t2) = (consM tmpM_a5sY) tmpT_a5t0
                       in
                         ((fromList (((cons (GP, WC)) tmpM_a5sY) tmpT_a5t0))
                            >>=
                              (\ (tmpT_a5t3, tmpT_a5t4)
                                 -> ((fromList ((((value (x + 10)) ()) tmpM_a5t1) tmpT_a5t3))
                                       >>= (\ () -> pure (x, x + 10)))))))]
```

### MiniEgison (Deep Embedding) vs. Sweet Egison (Shallow Embedding)

[miniEgison](https://github.com/egison/egison-haskell) is also a Haskell library that implements Egison pattern matching.
The main difference between [miniEgison](https://github.com/egison/egison-haskell) and Sweet Egison is that Sweet Egison translates pattern matching into Haskell control expressions (shallow embedding), whereas [miniEgison](https://github.com/egison/egison-haskell) translates it into Haskell data expressions (deep embedding).
As a result, Sweet Egison is faster than miniEgison.
The following benchmark is taken using MacBook Pro (2017, 2.3 GHz Intel Core i5).

|              | comb2 (n = 15000) | perm2 (n = 5000) | CDCL (50 vars) |
|--------------|-------------------|------------------|----------------|
| miniEgison   | 13.029 sec        | 3.854 sec        | 1.025 sec      |
| Sweet Egison | 0.303 sec         | 0.462 sec        | 0.097 sec      |

There is almost no execution performance differences between programs written using list comprehensions and Sweet Egison.

|                     | comb2 (n = 15000) | comb2 (n = 15000) | perm2 (n = 5000) | perm2 (n = 10000) |
|---------------------|-------------------|-------------------|------------------|-------------------|
| List Comprehensions | 0.347 sec         | 1.244 sec         | 0.409 sec        | 2.077 sec         |
| Sweet Egison        | 0.309 sec         | 1.081 sec         | 0.434 sec        | 1.984 sec         |

Programs used for the above benchmarks are follows:
* [sample/comb2.hs](https://github.com/egison/sweet-egison/blob/master/sample/comb2.hs)
* [sample/perm2.hs](https://github.com/egison/sweet-egison/blob/master/sample/perm2.hs)
* [sample/cdcl.hs](https://github.com/egison/sweet-egison/blob/master/sample/cdcl.hs)

## Bibliography

- [1] Satoshi Egi and Yuichi Nishiwaki: Functional Programming in Pattern-Match-Oriented Programming Style, The Art, Science, and Engineering of Programming, 2020, Vol. 4, Issue 3, Article 7, DOI: 10.22152/programming-journal.org/2020/4/7
- [2] Satoshi Egi and Yuichi Nishiwaki: Non-linear Pattern Matching with Backtracking for Non-free Data Types, APLAS 2018 - Asian Symposium on Programming Languages and Systems, DOI: 11.1007/978-3-030-02768-1_1
