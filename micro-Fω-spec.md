# mu-fomega
Exploration of implementation techniques for System Fω interpreters

## Overview of microFω

We will implement microFω, a subset of Dhall that is just about large enough to run the first example program shown above.

We will not implement records, union types, or imports. Our goal is to learn how a functional language can be specified and implemented. Once we learn that, we will be able to add new features safely and consistently.

We will implement parsing, type-checking, and evaluation of microFω programs.

Here is an example microFω program that uses all features we will support:

     let f = λ(x : Natural) → λ(x : Natural) → (123 + x) * x@1
     let id = λ(a : Type) → λ(x : a) → x
     let type_of_id = ∀(a : Type) → a → a
     let _ = id : type_of_id
     let _ = type_of_id : Type
     let _ = Type : Kind
     let Void = ∀(r : Type) → r
     let Unit = ∀(r : Type) → r → r 
     let Pair = λ(a : Type) → λ(b : Type) → ∀(r : Type) → (a → b → r) → r
     in f (Natural/subtract 2 10) (id Natural 20)

This program evaluates to the number `1144`.

An expression in microFω is one of the 9 cases:

1. A constant number of type `Natural`, for example: `123`
2. A constant built-in symbol, for example: `Type` or `Natural/subtract`
3. A variable symbol, possibly with an index, for example: `x@n`
4. An exression with an explicit type, for example: `x : t`
5. A lambda function, for example: `λ(a : t) → b`
6. A function type, for example: `∀(a : t) → b`
7. A `let` expression, for example: `let x = a in b`
8. A function application, for example: `f a`
9. A built-in binary operation, for example: `x * y`

## Grammar and syntax

The grammar of microFω is a much simplified version of the [Dhall grammar](https://github.com/dhall-lang/dhall-lang/blob/master/standard/dhall.abnf).

```
end-of-line =
      %x0A     ; "\n"
    / %x0D.0A  ; "\r\n"

valid-non-ascii = %x80-FFFD

tab = %x09  ; "\t"

not-end-of-line = %x20-7F / valid-non-ascii / tab

line-comment = "--" *not-end-of-line end-of-line

whitespace-chunk =
      " "
    / tab
    / end-of-line
    / line-comment

whsp = *whitespace-chunk

; Nonempty whitespace.
whsp1 = 1*whitespace-chunk

; Uppercase or lowercase ASCII letter.
ALPHA = %x41-5A / %x61-7A

; ASCII digit.
DIGIT = %x30-39  ; 0-9

ALPHANUM = ALPHA / DIGIT

; A simple label cannot be one of the reserved keywords
; listed in the `keyword` rule.
; A PEG parser could use negative lookahead to
; enforce this, e.g. as follows:
; label =
;       keyword 1*label-next-char
;     / !keyword (label-first-char *label-next-char)
label-first-char = ALPHA / "_"

label-next-char = ALPHANUM / "-" / "/" / "_"

label = label-first-char *label-next-char

; A nonreserved-label cannot be any of the reserved identifiers for builtins.
; Their list can be found in the `builtin` rule.
; The only place where this restriction applies is bound variables.
; A PEG parser could use negative lookahead to avoid parsing those identifiers,
; e.g. as follows:
; nonreserved-label =
;      builtin 1*label-next-char
;    / !builtin label
nonreserved-label = label

; Keywords.
let                   = "let"
in                    = "in"
forall-keyword        = "forall"
forall-symbol         = %x2200 ; Unicode FOR ALL: ∀
forall                = forall-symbol / forall-keyword

keyword = let / in / forall-keyword

; Builtin constants.
Natural = "Natural"
Natural-fold = "Natural/fold"
Natural-subtract = "Natural/subtract"
Type = "Type"
Kind = "Kind"

builtin =
    Natural
    / Natural-fold
    / Natural-subtract
    / Type / Kind

; Operators.
lambda        = %x3BB  / "\"
arrow         = %x2192 / "->"
plus          = "+"
times         = "*"

natural-literal =
    ; Decimal; leading 0 digits are not allowed
    / ("1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9") *DIGIT
    ; ... except for 0 itself
    / "0"

identifier = variable / builtin

variable = nonreserved-label [ whsp "@" whsp natural-literal ]

expression =
    ; "\(x : a) -> b"
      lambda whsp "(" whsp nonreserved-label whsp ":" whsp1 expression whsp ")" whsp arrow whsp expression
    ;
    ; "let x = e1 in e2"
    ; We allow dropping the `in` between adjacent let-expressions; the following are equivalent:
    ; "let x = e1 let y = e2 in e3"
    ; "let x = e1 in let y = e2 in e3"
    / 1*let-binding in whsp1 expression
    ;
    ; "forall (x : a) -> b"
    / forall whsp "(" whsp nonreserved-label whsp ":" whsp1 expression whsp ")" whsp arrow whsp expression
    ;
    ; Grammar was modified here to avoid backtracking:
    ; instead of separate alternatives, combine arrow and annotation as optional suffixes to operator-expression.
    ; In prevoius grammar spec, the alternatives were:
    ; / operator-expression whsp arrow whsp expression
    ; / annotated-expression  ; "x : t"
    ; The modified production is:
    / operator-expression [ whsp arrow whsp expression | whsp ":" whsp1 expression ]
    ; Previous grammar was:
    ;; "a -> b", shorthand for forall (_ : a) -> b
    ;; NOTE: Backtrack if parsing this alternative fails
    ; / operator-expression whsp arrow whsp expression
    ;
    ;; "x : t"
    ;/ annotated-expression
    ;; Nonempty-whitespace required after `:` in type annotations
    ;annotated-expression = operator-expression [ whsp ":" whsp1 expression ]

; "let x = e1"
let-binding = let whsp1 nonreserved-label whsp "=" whsp expression whsp1

operator-expression = plus-expression

plus-expression = times-expression *(whsp plus whsp times-expression)

times-expression = application-expression *(whsp times whsp application-expression)

application-expression = primitive-expression *(whsp1 primitive-expression)

primitive-expression =
    ; 123
    natural-literal
    ; "x"
    ; "x@2"
    / identifier
    ;
    ; "( e )"
    / "(" ~ full-expression ~ ")"

full-expression = whsp expression whsp
```

When a grammar rule specifies `a / b` then `a` is preferred to `b`; if `a` and the rest are parsed successfully then the `b` variant is not considered.

We use different libraries to implement this grammar, and compare performance.

### Pretty-printing


The pretty-printer works by computing the "inner" and "outer" binding precedence of each expression and each sub-expression.

Higher precedence values bind _weaker_.
The operation `*` has precedence `10`, and the operation `+` has precedence `20`.

How do we decide to write parentheses in `a * (b + c)`? The sub-expression `b + c` has outer precedence `20`, while the operation `x * y` prints both its inner sub-expressions `x`, `y` at precedence `10`. We write parentheses in `a * (b + c)` because `20` is above `10`.

More generally: Parentheses are required around a sub-expression if that sub-expression's outer precedence is strictly greater than the inner precedence of that sub-expression in the enclosing expression.

The outer precedence of constants and variable symbols is `0`, so they will never get parentheses in any enclosing expression.

When we pretty-print a standalone expression, we will say that it is enclosed in something with inner precedence `100`.
Then we will never put parentheses around a standalone expression.

Each of the `Expr` constructors has a separate inner precedence for each `Expr` argument.
Also, each `Expr` constructor has an outer precedence.

The task is to define all those precedence values for each of the constructors appropriately, so that sub-expressions get parentheses exactly when needed.


Here are some examples of how this mechanism works.


Binary operations have equal outer and inner precedence values. This is what we call the "precedence of the operation".

$$
\underbrace{~a~ +~}_{20\rightarrow}~ \underbrace{~b ~*~ c~}_{\leftarrow 10}\quad\quad
\underbrace{\texttt{Plus(a, }}_{20\rightarrow}~ \underbrace{\texttt{Times(b, c)}}_{\leftarrow 10} \texttt{)}
$$ 

$$
\underbrace{~a~ *~}_{10\rightarrow}~ (~\underbrace{~b ~+~ c~}_{\leftarrow 20}~)\quad\quad
\underbrace{\texttt{Times(a, }}_{10\rightarrow}~ \underbrace{\texttt{Plus(b, c)}}_{\leftarrow 20} \texttt{)}
$$ 

 
Some constructors have unequal outer and inner precedence values.
For example, `Applied(func, arg)` has outer precedence `5` and inner precedence `4` for each of its two arguments.
This allows us to have correct precedence in these examples.

$$
(a~ + ~ b) ~ * ~ f ~~ (g ~~ c)  \quad\quad
\underbrace{  \underbrace{\texttt{Times(}}_{10\rightarrow}~ \underbrace{\texttt{Plus(a, b)}}_{\leftarrow 20} \texttt{,} }_{10 \rightarrow}~ \underbrace{  \underbrace{\texttt{Applied(f,}}_{4\rightarrow}~ \underbrace{\texttt{Applied(g, c)}}_{\leftarrow 5} \texttt{ ) }}_{\leftarrow 5 } \texttt{)}
$$


$$
f ~~a~~ (b~ +~ c)~ *~ d \quad\qquad
\underbrace{\texttt{Times(}}_{10 \rightarrow} ~  \underbrace{\texttt{Applied(}}_{\leftarrow 5\quad 4\rightarrow} ~\underbrace{\texttt{Applied(f, a)}}_{\leftarrow 5} ~ \texttt{,}  ~ \underbrace{\texttt{Plus (b, c)}}_{\leftarrow 20} ~ \texttt{)}   ~ \texttt{,} ~ \texttt{d))}
$$

$$
 \lambda(a : \texttt{Natural}) \rightarrow \lambda(b : \texttt{Natural})  \rightarrow   ~f~~a~+~b  \quad\quad
\underbrace{\texttt{Lambda(a, Natural,}}_{50 \rightarrow} ~\underbrace{\texttt{Lambda(b, Natural, }}_{\leftarrow 50\qquad 50\rightarrow} ~   \underbrace{\texttt{Plus(}}_{\leftarrow 20\rightarrow}~ \underbrace{\texttt{Applied(f, a)}}_{\leftarrow 5} \texttt{, b)}   \texttt{)} \texttt{)}
$$

The constructor `Annotated(body, tipe)` has unequal precedence for the `body` and the `tipe`, because we want `123 : Natural : Type` to be printed without parentheses.

$$
\texttt{(123 : Natural) : Natural} \quad\quad
\underbrace{\texttt{Annotated(}}_{7\rightarrow}~\underbrace{\texttt{Annotated(123, Natural)}}_{\leftarrow 8}\texttt{,}~ \texttt{Natural}  \texttt{)}
$$ 

$$
\texttt{(1 + 2) : Natural : Type} \quad\qquad
\underbrace{\underbrace{ \texttt{Annotated(}}_{7\rightarrow} \underbrace{ \texttt{Plus(1, 2)}}_{\leftarrow 20} \texttt{,}~ }_{8\rightarrow}~\underbrace{\texttt{Annotated(Natural, Type)}}_{\leftarrow 8}\texttt{)}
$$ 

## De Bruijn indices

De Bruijn indices are numbers that disambiguate each shadowed variable inside lambdas.

Consider a curried function that shadows a variable:

```scala
{ (x : Int) => (x : Int) => 123 + x }   // Scala
```

The corresponding code in microFω is:

     λ(x : Natural) → λ(x : Natural) → 123 + x

This code shadows the outer `x` inside the inner lambda. An equivalent code without shadowing is:

```scala
{ (x : Int) => (t : Int) => 123 + t }   // Scala
```

In microFω:

     λ(x : Natural) → λ(t : Natural) → 123 + t

Here we just renamed the inner `x` to `t`. What if we do not want to rename `x` to `t` but still want to refer to the outer `x`?
In microFω, we can write `x@1` for that:

     λ(x : Natural) → λ(x : Natural) → 123 + x@1

This code is equivalent to:

     λ(x : Natural) → λ(t : Natural) → 123 + x

The `1` in `x@1` is the de Bruijn index of the outer variable `x` when accessed from the inner scope.
De Bruijn indices are non-negative integers.

We usually do not write `x@0`, we write just `x`.

Each de Bruijn index points to a specific nested lambda in some outer scope for a variable with a given name. For example, in this code:

     λ(x : Natural) → λ(t : Natural) → λ(x : Natural) → 123 + x@1

the variable `x@1` still points to the outer `x`. The presence of another lambda with the argument `t` does not matter for counting the nested depth for `x`.

At top level, it is invalid to use an index that is greater than the number of nested lambdas. For example, these expressions are invalid at top level (as there cannot be any outer scope):

     λ(x : Natural) → 123 + x@1
     λ(x : Natural) → λ(x : Natural) → 123 + x@2

At top level, these expressions are just as invalid as the expression `123 + x` since we never defined `x`.

The variable `x` in the expression `123 + x` is considered **free**; meaning that it should be defined in the outer scope.
Similarly, `x@1` in `λ(x : Natural) → 123 + x@1` is free. It is invalid to have expressions with free variables at top level.
At the top level, all variables must be bound. Expressions with free variables must be within bodies of some functions that bind their free variables.

When a function of the form `λ x → ...` is applied to an argument, we will need to substitute the outer `x`. Then we may need to recalculate some de Bruijn indices.

Suppose we would like to evaluate a function applied to an argument in this code:

     ( λ(x : Natural) → λ(y : Natural) → λ(x : Natural) → x + x@1 + x@2 ) y

This expression has free variables `y` and `x@2`, so it can occur only within a function body that binds `x` and `y`. It is worth remarking that here we are about to evaluate an expression "under a lambda". That is, we are going to simplify the body of a function before applying that function.

To evaluate this expression correctly, we cannot simply substitute `y` instead of `x`. Instead:

- the outer `x` corresponds to `x@1` within the expression, so we need to substitute `y` instead of `x@1` while keeping `x` and `x@2` unchanged
- `y` is already bound in the inner scope; so, we need to write `y@1` instead of `y`, in order to refer to the free variable `y` in the outside scope
- as we remove the outer `x`, the free variable `x@2` will now become `x@1`.

So, we must evaluate the given expression like this:

     ( λ(x : Natural) → λ(y : Natural) → λ(x : Natural) → x + x@1 + x@2 ) y
       = 
     λ(y : Natural) → λ(x : Natural) → x + y@1 + x@1  

During a single substitution, sometimes we need to shift the index upwards by 1, and sometimes downwards by 1. Also note that the variable `x@0` was left unchanged: the decrementing of indices for `x` starts only at index `1` because we are in a scope under a `λ x`.

This motivates the general "shift" operation for working with de Bruijn indices. That operation is defined in the Dhall standard [here](https://github.com/dhall-lang/dhall-lang/blob/master/standard/shift.md) as a function of _four_ arguments:

     // d = +1 or d = -1: whether we will increment or decrement the indices
     // x is the variable symbol on which the indices will be shifted
     // m is the minimum index value for shifting; we will shift x@n only if n >= m
     // e1 is a target expression where we will shift all bound variables named `x`
     // e2 is the resulting expression
     
     shift(d, x, m, e1) = e2

The Dhall documentation uses the "proof notation" for all definitions. The proof notation shows how to prove a desired statement (or "judgment") by proving some other statements first, or by referring to axioms. The equation `shift(d, x, m, e1) = e2` is a judgment that the result of evaluating the `shift` function with some arguments happens to equal `e2`. But we prefer to interpret that equation as a recipe for computing `e2` given `d`, `x`, `m`, and `e1`.

Adapting the Dhall specification to microFω, we write the following specification for `shift`, casing on each of the 9 expression types:

1. Variables get shifted if their index is above threshold:

       shift(d, x, m, x@n) = x@(n + d)  ; shift if n >= m
       shift(d, x, m, x@n) = x@n        ; no change if n < m
       shift(d, x, m, y@n) = y@n        ; no change if x and y are different symbols

2. All expressions other than lambdas, function types, and `let` expressions, will just recursively shift all their sub-expressions.
3. Lambdas and function types introduce a new bound variable, say `x`. If we are also shifting the symbol `x`, we will need to avoid shifting that new bound variable; so, we need to increment the threshold when descending into the body. Note that in `λ(x : T)` the variable `x` is not in scope for its type `T`; so we do not need to increment the threshold when shifting `T`. The specification looks like this:
    ```
    shift(d, x, m, A₀) = A₁   shift(d, x, m + 1, b₀) = b₁
    ─────────────────────────────────────────────────────
    shift(d, x, m, λ(x : A₀) → b₀) = λ(x : A₁) → b₁


    shift(d, x, m, A₀) = A₁     shift(d, x, m, b₀) = b₁
    ───────────────────────────────────────────────────  ; x ≠ y
    shift(d, x, m, λ(y : A₀) → b₀) = λ(y : A₁) → b₁


    shift(d, x, m, A₀) = A₁   shift(d, x, m + 1, b₀) = b₁
    ─────────────────────────────────────────────────────
    shift(d, x, m, ∀(x : A₀) → b₀) = ∀(x : A₁) → b₁


    shift(d, x, m, A₀) = A₁     shift(d, x, m, b₀) = b₁
    ───────────────────────────────────────────────────  ; x ≠ y
    shift(d, x, m, ∀(y : A₀) → b₀) = ∀(y : A₁) → b₁
    ```
    The last rule says that when we shift a symbol that is not bound, we just recursively shift all sub-expressions.
4. The `let` expressions also introduce a bound variable. Just like lambdas, `let x = a in b` needs to raise the shifting threshold when shifting `b` but not when shifting `a` because `x` is not in scope for `a`. (Recursive definitions are not supported!)
   ```
   shift(d, x, m, a₀) = a₁
   shift(d, x, m + 1, b₀) = b₁
   ───────────────────────────────────────────────────
   shift(d, x, m, let x = a₀ in b₀) = let x = a₁ in b₁


   shift(d, x, m, b₀) = b₁     shift(d, x, m, a₀) = a₁
   ───────────────────────────────────────────────────  ; x ≠ y
   shift(d, x, m, let y = a₀ in b₀) = let y = a₁ in b₁   
   ```
5. All other expressions just recursively shift all their sub-expressions. To make that code shorter, we implement a `map`-like function for the `Expr` type.

Here are the rules in mathematical notation:

$$
\frac{
  \begin{aligned}
    & \text{shift}(d, x, m, A_0) = A_1 \qquad \text{shift}(d, x, m + 1, b_0) = b_1
  \end{aligned}
}{
  \text{shift}(d, x, m, \lambda(x : A_0) \to b_0) = \lambda(x : A_1) \to b_1
}
$$

$$
\frac{
  \begin{aligned}
    & \text{shift}(d, x, m, A_0) = A_1 \qquad \text{shift}(d, x, m, b_0) = b_1
  \end{aligned}
}{
  \text{shift}(d, x, m, \lambda(y : A_0) \to b_0) = \lambda(y : A_1) \to b_1
}
; x \neq y
$$

$$
\frac{
  \begin{aligned}
    & \text{shift}(d, x, m, A_0) = A_1 \qquad \text{shift}(d, x, m + 1, b_0) = b_1
  \end{aligned}
}{
  \text{shift}(d, x, m, \forall(x : A_0) \to b_0) = \forall(x : A_1) \to b_1
}
$$

$$
\frac{
  \begin{aligned}
    & \text{shift}(d, x, m, A_0) = A_1 \qquad \text{shift}(d, x, m, b_0) = b_1
  \end{aligned}
}{
  \text{shift}(d, x, m, \forall(y : A_0) \to b_0) = \forall(y : A_1) \to b_1
}
; x \neq y
$$

$$
\frac{
  \begin{aligned}
    & \text{shift}(d, x, m, a_0) = a_1 \qquad \text{shift}(d, x, m + 1, b_0) = b_1
  \end{aligned}
}{
  \text{shift}(d, x, m, \text{let } x = a_0 \text{ in } b_0) = \text{let } x = a_1 \text{ in } b_1
}
$$

$$
\frac{
  \begin{aligned}
    & \text{shift}(d, x, m, b_0) = b_1 \qquad \text{shift}(d, x, m, a_0) = a_1
  \end{aligned}
}{
  \text{shift}(d, x, m, \text{let } y = a_0 \text{ in } b_0) = \text{let } y = a_1 \text{ in } b_1
}
; x \neq y
$$


Now we are ready to write the code for the `shift` function.

## Evaluation

Evaluation in microFω is based on applying just three rules:

- Evaluate all function applications by substituting variables.
- Evaluate built-in operations and functions (for example, `10 + 20` or `Natural/subtract 1 2`) according to their specific meaning.
- If the result still contains any lambdas, rename all bound variables to `_` and introduce enough de Bruijn indices to disambiguate name clashes.

Evaluation will take a microFω expression and produce an equivalent expression in the "normal form". The normal form cannot be evaluated any further. In most cases, the normal form will be a simple value (say, a `Natural` number).

For example, `(λ(y : Natural) → x + 10) 123` is evaluated to the normal form `133`.

However, in some cases the normal form is not a simple value but a function; and it can even be longer than the initial expression.
This is because evaluation in microFω is _symbolic_. In microFω, functions are applied even under lambda where some operands are variable symbols.

For example, this expression:

     λ(x : Natural) → (λ(y : Natural) → x + y) 123

is evaluated to the normal form `λ(_ : Natural) → _ + 123`. We have simplified `(λ(y : Natural) → x + y) 123` to `x + 123`, while `x` remains a variable symbol. Then we renamed `x` to `_`, replacing `λ(x : Natural) → x + 123` by `λ(_ : Natural) → _ + 123`.

When bound variables are renamed to `_`, we will introduce de Bruijn indices whenever we have to avoid a name clash. For example:

     λ(a : Type) → λ(b : Type) → a

is renamed to:

     λ(_ : Type) → λ(_ : Type) → _@1

If the variable `_` is already present, we may also need to increment its de Bruijn index:

     λ(x : Type) → _

will become:

     λ(_ : Type) → _@1


Evaluating by substition is called **beta-normalization**. Renaming bound variables is called **alpha-normalization**.

We specify the formal rules for these normalization operations using the "proof notation". We denote by $a\to_\alpha\, b$ a proof that the alpha-normalization of $a$ will give $b$, and by $a\to_\beta\, b$ a proof that the beta-normalization of $a$ will give $b$.

Proof steps are specified for each type of expressions by descending into sub-expressions; this is known as **small-step semantics**.

For example, the beta-normalization rules for adding natural numbers look like this:

$$\frac { a \to_\beta\, \texttt{0}\quad\quad  b\to_\beta\, b'} { a + b \to_\beta\, b' } $$

$$\frac { a \to_\beta\, a'\quad\quad b\to_\beta\,  \texttt{0}} { a + b \to_\beta\, a' } $$

$$\frac { a \to_\beta\, \texttt{m}\quad\quad  b\to_\beta\, \texttt{n}} { a + b \to_\beta\, \texttt{add m n} }\quad\textrm{Here ``}\texttt{add}\textrm{'' is the built-in integer addition.} $$

$$ \frac{a \to_\beta\, a'\quad\quad b \to_\beta\, b'}{a + b\to_\beta\, a' + b'} \quad\textrm{If no other rule matches.} $$

These four rules show how to compute the beta-normalization for any expression of the form $a + b$. We will need to specify how these operations work with all possible expression structures. This will allow us to translate the specification directly into code.

Other rules work similarly.

Although the rules are formulated as relations between arbitrary variables, in fact all the rules express functions from input to output.
To clarify this, consider this rule:

$$\frac { a \to_\beta\, a'\quad\quad b\to_\beta \, \texttt{0}} { a + b \to_\beta\, a' } $$

Formally, this rule says: if any expressions $a$, $a'$, and $b$ happen to be such that $a \to_\beta\, a'$ and $b\to_\beta  \,\texttt{0}$ then we will have a proof of $a + b \to_\beta\, a'$. So, formally this is just a statement about a relation between arbitrary expressions $a$, $a'$, and $b$. However, we notice that all rules have the form of _functions_: we are able to compute the right side of $\to\beta$ given variables to the left of $\to_\beta$ by recursive descent into sub-expressions.

Here is an (imaginary) rule that is not of this form:

$$\frac { a \to_\beta\, b\quad\quad c\to_\beta \, b} { a \to_\beta\, c }\quad \text{Not a rule we can use!} $$

The problem with this rule is that $c$ is only seen on the left-hand side in the numerator. This means we need to _guess_ the expression $c$ such that it is beta-normalized to $b$. Even if we knew $b$, the rule does not tell us how to find an expression $c$ such that its beta-normalization returns a given expression $b$. So, this rule cannot be directly translated into an algorithm.

The actual rules of alpha- and beta-normalization were intentionally designed to avoid this situation and to permit a straightforward implementation.

## Typechecking

Type-checking is a procedure where we determine whether a given microFω expression has a well-defined type. If so, we will have "inferred" that type and found that the expression is "well-typed". Otherwise, the expression will be considered as "ill-typed" (having a type error), and we will reject that expression as invalid and report the error to the user.

Type-checking is a necessary step before evaluating any expressions: only well-typed expressions will be evaluated.

In microFω, type-checking is simple. The user needs to specify the types in all lambda functions, and expressions may not be recursive. So, any variable contained in a valid expression already has a specified type.

For example, consider this expression:

     λ(x : Natural) → x + y

When we look at the sub-expression `x + y`, we already know that `x` has type `Natural`. If `y` is undefined (a free variable), we cannot know the type of `y`, so we cannot type-check this expression. So, we stop and report a type error due to an undefined variable `y`. If `y` has been bound in some lambda in an outer scope (such as `λ(y : T) → ...`) then we will know the type `T` that has been assigned to `y`. After that, we need to verify that the operation `+` is well-typed: both its operands must have type `Natural`. In this case, we need to verify that `T` is `Natural`. If this is so, the entire expression is well-typed: it is a function of type `∀(x : Natural) → Natural`. Otherwise, we will have found a type error: the operation `+` is applied to a value of type different from `Natural`. We reject the expression and show the type error to the user.

We see that type-checking requires us to know what types have been previously assigned to bound variables when we descend recursively into sub-expressions. This information is held as a list of type assignments such as `[ y : T, x : Natural ]`. This list is called a **typing context** and is often denoted by the Greek letter Γ ("gamma").

To describe this property of microFω's type-checking algorithm, we say it is **unidirectional**. The intuition is that the type information goes from lambda function declarations (`λ(x : Natural) → ...`) towards the function bodies, but not in the opposite direction.

The unidirectional type-checking algorithm descends recursively through the expressions and their sub-expressions. Each time we encounter a lambda, such as `λ(x : Natural) → ...`, we add the type annotation `x : Natural` to the current typing context and proceed to type-check the function body using the new typing context. Checking the function body will yield success or failure, but no additional type information about `x`.

A unidirectional type-checking algorithm is not able to infer the type of `x` in the following example:

     λ(x) → λ(y : Natural) → x + y  -- Not valid in microFω!

The binary operation `+` is defined only for `Natural` operands, but the algorithm is not able to infer that `x` must be `Natural` and to fill in the missing type annotation for `x`. That can be done via **bidirectional type-checking** algorithms, but they are much more complicated.

We also see that type-checking needs evaluation and equality comparison. In the example just shown, we need to know whether the type `T` equals `Natural`. However, `T` could be a complicated type expression involving function applications. To know whether `T` is really equal to `Natural`, we need to be able to evaluate arbitrary expressions to an unambiguous normal form. So, implementing type-checking requires us to use full alpha- and beta-normalization.

It is not a problem that beta-normalization needs type-checking to be performed first, while type-checking depends on beta-normalization. The two functions (beta-normalization and type-checking) are mutually recursive, and recursion will terminate because each subsequent recursive call will be always applied to a smaller sub-expression.