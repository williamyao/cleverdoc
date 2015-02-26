CLeverdoc
=========

Your unit tests are your program specification, but enforceable. Why should a unit test take more time to write than a specification?

Something of this sort is needlessly verbose:

```lisp
(deftest test-somefunction ()
  (assert-equal value1 (somefunction input1))
  (assert-equal value2 (somefunction input2))
  (assert-equal value3 (somefunction input3)))
```

Let's have something a little more palatable for this pattern of writing unit tests for functions.

Basics
------

The most important macro is `DOCUMENT`. Let's look at testing `+` as an example.

```lisp
(document +
  (2 2 ==> 4)
  (6 5 ==> 11)
  (1 1 ==> 3))
```

Once this is defined, now we can run it using `TEST`.

```lisp
(test)

==>
    
Specification for COMMON-LISP::+ FAILED:
    (+ 1 1) =/= 3

=============
   RESULTS
=============

Performed 3 checks.
    FAIL: 1 (33%)
    Pass: 2 (67%)
NIl
```

As an added bonus, if the first form in the `DOCUMENT` body is a string, it will be set as the function documentation.

```lisp
(document +
  "Adds numbers together."
  ...)
```

Simple.

Usage
-----

**CLeverdoc** is based around various operators, like `==>`, that associate some form, like a function call, with some expected value. For example, `==>` checks if calling the documented function with the arguments on the left side of `==>` results in the right side. Each such operator will result in a single check when `TEST` is called.

Arbitrary Lisp forms can be inserted into the body of a `DOCUMENT` call. They will be left as is, and forms containing CLeverdoc operators will execute as expected. For example:

```lisp
(document subseq
  (let ((some-string "this is a value"))
    (some-string 0 4 ==> "this")))
```

Many of the CLeverdoc operators are aware of multiple return values.

```lisp
(document floor
  (2.5 ==> 2 0.5)
  (-5.5 ==> -6 0.5))
```

###Operators

####Function calling

The backbone of CLeverdoc. All of these operators automatically expand into calls of the documented function, passing in the arguments to the left of the operator

+ **==>**

  [*argument*]\* **==>** *value* [*multiple-value*]\*

  Test that calling the documented function with the arguments specified results in the value(s) specified.

  ```lisp
  (document intern
    ("tests" :keyword ==> :tests)
    ("are" :keyword ==> :are)
    ("fun!" :keyword ==> :fun!))
  ```

+ **==/**

  [*argument*]\* **==/** *value* [*multiple-value*]\*

  Test that calling the documented function with the arguments specified does not result in exactly the value(s) specified. In the case of multiple return values, only one needs to differ for **==/** to pass.

  ```lisp
  (document +
    (1 1 ==/ 3))
  ```

+ **==x**

  [*argument*]\* **==x** [*error*]\*

  Test that calling the documented function with the arguments specified results in one of the errors specified.

  ```lisp
  (document aref
    ('(not an array) 3 ==x type-error arithmetic-error))
  ```

+ **==>>**

  [*argument*]\* **==>>** [*output*]\*

  Binds the variable **{OUT}** to an output stream with type inferred through the right-side arguments. Tests that calling the function with the arguments specified results in writing the right-side sequence to **{OUT}**. Useful for testing I/O functions.

  ```lisp
  (document write-utf8-char
    (#\€ {out} ==>> #xe2 #x82 #xac)
    (#\¢ {out} ==>> #xc2 #xa2))

  (document format
    ({out} "~{~c~}" '(#\p #\i #\z #\z #\a) ==>> "pizza"))
  ```

+ **>>>**

  [*input*]\* **>>>** &body *body*

  Binds the variable **{IN}** to an input stream within the scope of BODY, with type inferred from the left-side arguments. Reading the stream will return the left-side arguments. Not a test per-se, but useful for testing I/O functions.

  ```lisp
  (document read-utf8-char
    (#xe2 #x82 #xac >>> {in} ==> #\€)
    (#xc2 #xa2 >>> {in} ==> #\¢))

  (document read-string
    (#\p #\i #\z #\z #\a >>> {in} ==> "pizza"))
  ```    

####Equality

CLeverdoc supports basic equality operators. However, this should usually be eschewed in favor of using the operators with implied function calls. They may be useful in some situations, though.

+ **===**

  *form* **===** *value* [*multiple-value*]\*

  Test that evaluating *form* results in the value(s) specified.

  ```lisp
  ((list) === nil)
  ```
+ **=/=**

  *form* **=/=** *value* [*multiple-value*]\*

  Test that evaluating *form* does not result in exactly the value(s) specified. In the case of multiple values, only one needs to differ for **===** to pass.

  ```lisp
  (2 =/= 3)
  ```
+ **=x=**

  *form* **=x=** [*error*]\*

  Test that evaluatiing *form* results in one of the errors specified.

  ```lisp
  ((+ 2 #\c) =x= simple-type-error end-of-file)
  ```