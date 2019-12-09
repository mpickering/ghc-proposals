Lightweight Stage Polymorphism
==============

.. author:: Matthew Pickering & Jamie Willis
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

In order to guarantee abstraction overhead is eliminated before runtime it
is necessary to use typed template haskell. Unfortunately, when making this decision
you must also decide what arguments are known at compile-time and which are
known at run-time. This binding-time analysis is fixed which is unfortunate for
a general purpose library.

With a slight change in the desugaring rules for typed quotations and splices we
can abstract away from this decision and liberate authors to stage their libraries,
so users know the generated code will be efficient but without constricting
users to a specific binding-time analysis.


Motivation
----------

Consider staging the power function::

  power :: Int -> Int -> Int
  power n 0 = 1
  power n k = n * power n (k - 1)

This amounts to explaining to the compiler how to split the computation into two
distinct stages with the assumption that the exponent will be known at the stage
before the base. Therefore in the first stage, the recursive calls can be computed,
as they only depend on the assumed static knowledge of the exponent::

  power :: Quote m => Code m Int -> Int -> Code m Int
  power n 0 = [| 1 |]
  power n k = [| $n * $(power n (k - 1)) |]

The fact that ``n`` and hence the result of the function are only known at the next
stage are indicated by the ``Code`` type constructor.

So far so good, we have a generic function which will compute an optimised ``power``
function for any statically exponent ``k``. The issue now is that in order to
use ``power``, you **must** statically know the value of ``k``. The purpose of this
proposal is to suggest a mechanism where from the staged version of ``power``
you can recover the unstaged version by "erasing" the brackets.





The ``vector`` library is another example of a library which would greatly benefit
from being staged. At the moment it relies on brittle rules and inlining in order
to perform fusion operations -- there is no hope on relying on this behaviour if
you really need the performance. Every GHC release there are tickets about the very
long compile times or how the "optimiser" is broken. The library author should
take the decisions into their own hands.


Proposed Change Specification
-----------------------------

This process works by overloading quotation brackets by introducing a type class
which allows us to give different interpretations of the quoted expression::

  class Repr r where
    rep :: a -> m (TExp a) -> r m a

The type of a quoting an expression ``e :: T` is now ``(Repr r, Quote m) => r m T``.
This is achieved by modifying the desugaring of a quotation to be in terms
of ``rep``::

  [| 5 |] => rep 5 (numE (litE 5))

The first argument is simply the expression contained inside the quotation, the
second argument is the representation of the expression as computed currently
by the quotation.

Two important implementations of ``Repr`` that can be provided in a library
are the two projection functions to extract either the current stage value or the
representation::

  data Wrapper m a = Wrapper { extract :: a }

  newtype Code m a = Code { getCode :: m (TExp a) }

  instance Repr Wrapper where
     rep a _ = Wrapper a

  instance Repr Code where
     rep _ c = Code

These wrappers are used when desugaring splices::


  foo3 :: (Repr p, Quote m) => p m Int -> p m Int
  foo3 x = [| 1 + $(x) |]

  =>
  -- Splice replaced with projection

  foo3 :: (Repr p, Quote m) => p m Int -> p m Int
  foo3 x = rep (1 + (extract x)) (appsE (varE (+)) [(numE 1), getCode x])


The ``rep`` method is also exposed directly to users so that different
implementations can be given in the case where the library author wants the
behaviour to be different depending on whether an argument is statically known
or not::

  sp_int :: (Repr r, Quote m) => r m Int
  sp_int = rep 4 (getCode [| 3 |])

This is useful in situations where complicated expensive logic is used at compile
time but would be too expensive to perform at runtime at part of an interpreter.
By a careful manual insertion whole compilation passes could be elided from interpretation.

A tricky point is what to do if you have nested splices with more specific types.
For example, one of your nested splices fixes the type of the whole quotation to
be ``Code``. This causes difficulty because the ascribed desugaring rules dictate
that the pure component of the representation is traversed and splices replace with
a project to the pure component. Something which is not possible if a nested splice only
constaints a future stage representation. Perhaps one option is to desugar using
``undefined`` because we know that the overall type of the bracket must also
be restricted to ``Code`` and therefore the pure fragment will not be forced.



Examples
--------

Consider if you are writing a staged parser generator library. You want to support
the two sitations where a user provides a ::

  parser :: (WQ p, MonadError CompileError m) => Grammar a -> p m (String -> Maybe a)

  -- Rely on specialisation to remove abstraction overhead but it is quite simple overhead to
  -- remove which should be removed by specialisation.
  runParser :: Grammar a -> m (String -> Maybe a)
  runParser g = forget (parser g)

  -- Splicing Code

  -- Interpret errors into Q and generate the parser based
  -- on a static grammar.
  generateParser ::  String -> Maybe a
  generateParser = $$(runExceptT (parser staticGrammar)

Effect and Interactions
-----------------------
Detail how the proposed change addresses the original problem raised in the
motivation.

Discuss possibly contentious interactions with existing language or compiler
features.


Costs and Drawbacks
-------------------
Give an estimate on development and maintenance costs. List how this effects
learnability of the language for novice users. Define and list any remaining
drawbacks that cannot be resolved.


Alternatives
------------
List existing alternatives to your proposed change as they currently exist and
discuss why they are insufficient.


Unresolved Questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and
specification. Be upfront and trust that the community will help. Please do
not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to
the steering committee.


Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other resources
and prerequisites are required for implementation?

Endorsements
-------------
(Optional) This section provides an opportunty for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.
