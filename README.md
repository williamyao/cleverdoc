CLeverdoc
=========

Your unit tests are your program specification, but enforceable. Why should a unit test take more time to write than a specification?

Something of this sort is needlessly verbose:

    (deftest test-somefunction ()
      (assert-equal value1 (somefunction input1))
      (assert-equal value2 (somefunction input2))
      (assert-equal value3 (somefunction input3)))

Let's have something a little more palatable for this pattern of writing unit tests for functions.

Basics
------

The most important macro is `DOCUMENT`. Let's look at testing `+` as an example.

    (document +
      (2 2 ==> 4)
      (6 5 ==> 11)
      (1 1 ==> 3))

Once this is defined, now we can run it using `TEST`.

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

As an added bonus, if the first form in the `DOCUMENT` body is a string, it will be set as the function documentation.

    (document +
      "Adds numbers together."
      ...)

Simple.
