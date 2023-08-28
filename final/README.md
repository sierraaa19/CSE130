[![Open in Codespaces](https://classroom.github.com/assets/launch-codespace-7f7980b617ed060a017424585567c406b6ee15c891e84e1186181d67ecf80aa0.svg)](https://classroom.github.com/open-in-codespaces?assignment_repo_id=11345472)
# Spring 2023 Final [Points: 39 public / 120 private]

**Released:** Thursday, June 15, 9am PDT

**Due:** Friday, June 16, 9am PDT

- You **may** consult any course material (lecture notes, assignments, past exams, etc)
- You **may** ask for clarifications on Piazza via a private message to instructors
- You **may not** communicate with other students or ask for anyone's help
- You **may not** search help forums (StackOverflow) and the Internet for a solution
- You **may not** use AI coding assistants (Copilot, ChatGPT, etc)

## Overview

The exam is in the files:

- [Matryoshka.lc](./src/Matryoshka.lc)
- [Trie.hs](./src/Trie.hs)
- [Types.hs](./src/Types.hs)
- [Patterns.hs](./src/Patterns.hs)
- [tests/Test.hs](./tests/Test.hs)

As before `Test.hs` has some sample tests, and testing code that
you can use to check your solution before submitting.

### Testing and Evaluation

All the points, will be awarded automatically, by
evaluating your functions against a given test suite.

[Tests.hs](./tests/Test.hs) contains a very small suite
of tests which gives you a flavor of of these tests.
When you run

```shell
$ make test
```

Your last lines should have

```
All N tests passed (...)
```

**or**

```
K out of N tests failed
```

**If your output does not have one of the above your code will receive a zero**

If for some problem, you cannot get the code to compile,
leave it as is with the `error ...` with your partial
solution enclosed below as a comment.

The other lines will give you a readout for each test.
You are encouraged to try to understand the testing code,
but you will not be graded on this.

### Submission Instructions

Submit your code via the `final` assignment on Gradescope. 
Connect your Github account to Gradescope and select your repo.
Detailed instructions on how to submit your code directly from the Git repo can be found on Piazza.

## Q1. Lambda Calculus: Matryoshka Numbers [30 points]

In this task we will implement an alternative representation of natural numbers in lambda calculus, 
which we are going to call [Matryoshka](https://en.wikipedia.org/wiki/Matryoshka_doll) numbers,
because they are reminiscent of nesting Russian dolls.

More precisely, Matryoshka numbers are nested pairs.
The number `0` is represented as a pair of `FALSE` and `FALSE`,
and a number `N + 1` is represented as a pair of `TRUE` and the number `N`.
In other words, we can define the zero value `MZERO`
and the successor function `MSUCC` as follows:

```haskell
let MZERO = PAIR FALSE FALSE     -- Zero; normal form: \b -> b FALSE FALSE
let MSUCC = \n -> PAIR TRUE n    -- Successor
```

Using `MZERO` and `MSUCC` we can easily define any Matryoshka numeral,
for example:

```haskell
let MONE   = MSUCC MZERO   --                             \b -> b TRUE (\b -> b FALSE FALSE)
let MTWO   = MSUCC MONE    --               \b -> b TRUE (\b -> b TRUE (\b -> b FALSE FALSE))
let MTHREE = MSUCC MTWO    -- \b -> b TRUE (\b -> b TRUE (\b -> b TRUE (\b -> b FALSE FALSE)))
```
(The comment next to each number is its normal form).

### 1.1. Is zero (5 points)

Implement the function `MISZ` that determines whether or not a Matryoshka number is zero.
Your implementation must satisfy the following test cases:

```haskell
eval isz_zero:
  MISZ MZERO
  =~> TRUE
  
eval isz_one:
  MISZ MONE
  =~> FALSE
```

In this and following tasks, you **may use** any function already defined in [Matryoshka.lc](./src/Matryoshka.lc).
You may also define your own helper functions if needed.

### 1.2. Decrement (5 points)

Implement the function `MDEC` that decrements a Matryoshka number by one
(and bottoms out at zero).
Your implementation must satisfy the following test cases:

```haskell
eval dec_two:
  MDEC MTWO
  =~> \b -> b TRUE (\b -> b FALSE FALSE)  -- MONE

eval dec_zero:
  MDEC MZERO
   =~> \b -> b FALSE FALSE                -- MZERO
```

### 1.3. Subtraction (15 points)

Implement the function `MSUB` that subtracts one Matryoshka number from another.
Your implementation must satisfy the following test cases:

```haskell
eval sub_three_two:
  MSUB MTHREE MTWO
  =~> \b -> b TRUE (\b -> b FALSE FALSE)  -- MONE

eval sub_two_three:
  MSUB MTWO MTHREE
  =~> \b -> b FALSE FALSE                 -- MZERO
```  

### 1.4. Church to Matryoshka (5 points)

Implement the function `FROM_CHURCH` that converts a Church numeral
into a Matryoshka number.
(Recall that Church numerals are the traditional representation of natural numbers we studied in class.)
Your implementation must satisfy the following test cases:

```haskell
eval from_church_zero:
  FROM_CHURCH ZERO
  =~> \b -> b FALSE FALSE                 -- MZERO
  
eval from_church_one:
  FROM_CHURCH ONE
  =~> \b -> b TRUE (\b -> b FALSE FALSE)  -- MONE
```

## Q2. Datatypes and Recursion: Autocomplete 2.0 [50 points]

All the functions you must write for this problem are in [Trie.hs](./src/Trie.hs).

The autocomplete feature you built a couple of months ago turned out to be a big hit,
so now you decide to improve its performance and add more functionality.
To avoid pesky bugs in the future, you also decide to add property-based tests using QuickCheck.

Recall that our autocomplete stores its dictionary—the set of known words—in a **prefix tree** (also called a **trie**).
Please re-read the Q3 of the [midterm](https://drive.google.com/file/d/1281R1xrKSmprtxAn1MXaJKRMHAN0vOcy/view?usp=sharing)
for the definition of this data structure.

### More Efficient Search

Recall that we represent a trie using the following Haskell datatype:

```haskell
-- | A trie node stores whether it is terminal + zero or more edges
data Trie = Node Bool [Edge]

-- | An edge is a pair of a character and a child trie
type Edge = (Char, Trie)
```

The main function used in autocomplete is `withPrefix`,
which returns the subset of words in the dictionary that start with a given prefix.
You realize that you can make this function more efficient
if you keep your trie **sorted**.
We will say that a trie `t` is sorted,
if each node in `t` has its outgoing edges ordered in *strictly increasing* order by their labels.

For example, the `pronouns` trie from the midterm, 
which in ASCII forms looks like this:
```
░--h--░--e--█--r--█
|     |
|     └--i--░--s--█       -- out of order!
|           |
|           └--m--█       -- out of order!
└--s--░--h--░--e--█
```
was **not** sorted,
because here the label `'s'` came before `'m'` among the outgoing edges of the `"hi"` node.
Instead, the trie containing the same set of words `"he", "him", "his", "she", "her"`
should look like this:  
```
░--h--░--e--█--r--█
|     |
|     └--i--░--m--█
|           |
|           └--s--█
└--s--░--h--░--e--█
```
We will refer to this value as `pronounsSorted`.

For a sorted trie, we can implement a more efficient version of `withPrefix` like so:

```haskell
-- | Words with a given prefix stored in a trie
withPrefix :: String -> Trie -> [String]
withPrefix []     t           = words t
withPrefix _      (Node _ []) = []
withPrefix (c:cs) (Node b ((k,t):es))
  -- Found an outgoing edge labeled 'c': go down that edge:
  | k == c                    = [k:w | w <- withPrefix cs t]
  -- NEW: current edge's label is less than 'c': check the remaining edges:
  | k < c                     = withPrefix (c:cs) (Node b es)
  -- NEW: current edge's label is already greater than 'c', so there is no 'c' edge:
  | otherwise                 = []
```
As you can see, the iteration through the edges of a node can stop early
if it encounters a label that is larger than the character it is looking for.

In the rest of this task,
you will implement various operations on *sorted* tries.
In this question you **are allowed** to:

- Use any functions already defined in [Trie.hs](./src/Trie.hs)
- In later subtasks, use the functions you have implemented in earlier subtasks
- Define helper functions under a `where` clause
- Use any functions from *already imported* libraries, including but not limited to:

```haskell
elem   :: a -> [a] -> Bool               -- Does the element occur in the list?
take   :: Int -> [a] -> [a]              -- The prefix of a list of a given length
drop   :: Int -> [a] -> [a]              -- The suffix of a list after a given number of elements
map    :: (a -> b) -> [a] -> [b]          
filter :: (a -> Bool) -> [a] -> [a]
foldr  :: (a -> b -> b) -> b -> [a] -> b
foldl  :: (b -> a -> b) -> b -> [a] -> b
all    :: (a -> Bool) -> [a] -> Bool     -- Do all elements of the list satisfy the predicate?
any    :: (a -> Bool) -> [a] -> Bool     -- Do any elements of the list satisfy the predicate? 
```


### 2.1. Insertion [15 pts]

Fill in the definition of `insert` that inserts a word into the trie.
You can *assume* that the input trie is sorted.
You must *guarantee* that the output trie is also sorted
and contains the new word along with all the old words.
Your implementation must be *efficient*:
specifically, its complexity should be no worse than `O(n*k)`,
where `n` is the length of the word
and `k` is the maximum number of outgoing edges from a node in the trie.

Your implementation must satisfy the following test cases:

```haskell
λ> insert "he" empty
Node False [('h',Node False [('e',Node True [])])]

λ> insert "it" (insert "he" empty)
Node False [('h',Node False [('e',Node True [])]),('i',Node False [('t',Node True [])])]

```

### 2.2. Build with HOF [5 pts]

Fill in the definition of `build` that converts the input `[String]` list
into a `Trie` by inserting all list elements into an empty trie.
**You may not** use explicit recursion and must use existing library functions instead.

Your implementation must satisfy the following test cases:

```haskell
λ> build ["it", "he"]
Node False [('h',Node False [('e',Node True [])]),('i',Node False [('t',Node True [])])]

λ> build []
Node False []
```

### Property-based testing

Now we want to use QuickCheck to test whether we implemented `insert` and `build` correctly.
In particular, we want to test the following properties:

1. `prop_contains_elts`: a trie created with `build xs` contains all the elements of `xs`
2. `prop_sorted`: any trie created with `build` is sorted
3. `prop_with_prefix_correct`: in a trie created with `build`, `withPrefix` returns exactly those words that start with a given prefix

To be able to check these properties, 
you need to implement some helper functions.

### 2.3. Checking if two lists have the same elements [10 pts]

Fill in the definition of `sameElems` 
that takes as input two lists
and determines whether they store the same *set* of elements.
**You may not** use explicit recursion and must use existing library functions instead.

Your implementation must satisfy the following test cases:

```haskell
λ> sameElems [1,2,3] [3,2,1,2]
True

λ> sameElems [1,2,3] [1,2]
False
```

After this step you should see the following output:

```haskell
λ> quickCheck prop_contains_elts
+++ OK, passed 100 tests.
```

### 2.4. Checking if a trie is sorted [10 pts]

Fill in the definition of `sortedTrie` 
that takes as input a trie and determines whether it is sorted.
**You may** use explicit recursion for this function.

Your implementation must satisfy the following test cases:

```haskell
λ> sortedTrie (Node False [('h',epsilon),('i',epsilon)])
True

λ> sortedTrie (Node False [('i',epsilon),('h',epsilon)])
False
```

After this step, you should see the following output:

```haskell
λ> quickCheck prop_sorted
+++ OK, passed 100 tests.
```

### 2.5. Checking search correctness [10 pts]

Fill in the definition of `withPrefixCorrect`
that takes as input a prefix string `p` and a trie `t`
and determines whether `withPrefix p t` returns a correct result relative to `words t`:
that is, whether `withPrefix p t` returns exactly those words from `words t` that start with `p`.
**You may not** use explicit recursion and must use existing library functions instead.
Your implementation must satisfy the following test cases:

```haskell
λ> withPrefixCorrect "h" pronounsSorted
True 
``` 

After this step, you should see the following output:

```haskell
λ> quickCheck prop_with_prefix_correct
+++ OK, passed 100 tests.
```

## Q3. Interpreters and Monads: Pattern Matching [40 points]

All the functions you must write for this problem are in [Patterns.hs](./src/Patterns.hs).

In this task we will add pattern matching and `case` expressions to the Nano language.

In this question you **are allowed** to:

- Use any functions already defined in [Types.hs](./src/Types.hs) or [Patterns.hs](./src/Patterns.hs)
- Use any functions from the Haskell standard library
- In later subtasks, use the functions you have implemented in earlier subtasks
- Define helper functions, either at the top level or under a `where` clause

### Syntax

Recall that a *pattern* is either a *variable* or a *constructor applied to patterns*.
We will represent patterns in Nano using the following datatype,
found in [Types.hs](./src/Types.hs)

```haskell
data Pattern
  = PVar Id                -- Variable
  | PInt Int               -- Integer constant
  | PNil                   -- Nil
  | PCons Pattern Pattern  -- Cons applied to patterns
```

(For simplicity, we removed Booleans from Nano, so the only values are integers and lists.)
With this datatype at hand,
we have extended our definition of `Expr` with a `case` expression:

```haskell
data Expr
  = ...                           -- as before
  | ECase Expr [(Pattern, Expr)]  -- case expression
```

For example, the case expression:

```haskell
case xs of
  []     -> 0
  (y:ys) -> y
```

would be represented as 

```haskell
ECase (EVar "xs") 
  [ (PNil                        , EInt 0)
  , (PCons (PVar "y") (PVar "ys"), EVar "y")]
```

### 3.1. The Result Monad [10 pts]

Recall the way pattern matching works:
matching a *value* against a *pattern* can either
(1) fail, or
(2) succeed and return a list of bindings of variables in the pattern to parts of the value.

To represent the result of pattern matching,
we have defined the datatype `Result a` which denotes computations that can
either fail or succeed and return a value of type `a`: 

```haskell
data Result a
  = Fail
  | Success a
```

Define an instance of `Monad` for `Result`,
i.e. implement methods `return` and `(>>=)`
to achieve the following behavior:

- If the first step of the computation fails, then the whole computation halts and fails.
- If the first step of the computation succeeds and returns a value,
  this value is passed to the rest of the computation.

Your implementation must satisfy the following test cases:

```haskell
ret3 :: Result Int
ret3 = return 3

... 

λ> do x <- ret3 ; return (x + 5)
Success 8

λ> do x <- Fail ; return (x + 5)
Fail
```

### 3.2. Pattern Matching [15 pts]

Implement the function 

```haskell
match :: Pattern -> Value -> Result Env
```

which takes as input a pattern `p` and a value `v`.
If `v` *does not* match `p`, the function must return `Fail`.
If `v` *matches* `p`, the function must return `Success env`,
where `env` contains the bindings of all variables in `p`
to the corresponding parts of the value `v`.

You can **assume** that all variables inside a single pattern `p` are unique.

Your pattern matching should behave the way is does in Haskell:
a variable pattern matches any value,
while a constructor/value pattern matches only the same constructor/value.
In particular, your implementation must satisfy the following tests cases:

```haskell
let consPat = PCons (PVar "y") (PVar "ys")
let l123    = VCons (VInt 1) (VCons (VInt 2) (VCons (VInt 3) VNil))

-- match `y:ys` against `[1,2,3]` yields environment `y := 1, ys := [2,3]`
λ> match consPat l123
Success [("y", VInt 1), ("ys", VCons (VInt 2) (VCons (VInt 3) VNil))]

-- match `y:ys` against `[]` fails
λ> match consPat VNil
Fail
```

**Hint:** We encourage you to use `do` notation and the monadic operations
on `Result`s you have implemented in 3.1 to structure your code.
However, if you have not finished 3.1, you can also solve this task
without monads, albeit more verbosely.

### 3.3. Case expression [15 pts]

Using the `match` function you have implemented in 3.2,
finish the implementation of `eval` for `case` expressions.

The behavior of Nano `case` expressions should be the same as in Haskell.
To make it easier for you to test your evaluator,
we have implemented a parser for `case` expressions.
For simplicity, our parser requires that the cases must be **separated by a semicolon**.
You can use `execString` to test your evaluator in the command line.

For example, your implementation must satisfy the following test case:

```haskell
λ> execString "case [1,2,3] of [] -> 0 ; (y:ys) -> y"
VInt 1
```

When none of the patterns match the value of the scrutinee,
you should **throw an error** with the message `"non-exhaustive patterns"`
(please use exactly this error message):

```haskell
λ> execString "case [1,2,3] of [] -> 0"
*** Exception: Error {errMsg = "non-exhaustive patterns"} 
```

You **may assume** that all the programs are well-typed,
i.e. we will not test programs where the scrutinee and the patterns are of different types,
like so:

```haskell
-- Ill-typed program, do not worry about this case!
λ> execString "case [1,2,3] of 0 -> 0 ; x -> x" -- matching a list [1,2,3] against an integer 0
```

As before, you can also test your evaluator on files using `execFile`.
For example:

```haskell
λ> execFile "tests/input/t1.hs"
VInt 1
```

Feel free to inspect the input files we provided
and write your own to test your evaluator.


