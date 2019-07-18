Overloaded Quotations
=====================

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
.. sectnum::
.. contents::

This proposal is about making quotation brackets polymorphic. The motivation
is so that quotes can be used without being tied to the ``Q`` monad.


Motivation
------------

Quoting an expression ``[| e |]`` yields its representation. In the current
implementation the type of representations is ``Q Exp``. This proposal is
changing the type of the representation to the polymorphic
``Quote m => m Exp``, and likewise ``Quote m => m (TExp a)``.
The ``Quote`` interface defines the operations which are necessary to construct
the code representation::

   class Applicative m => Quote m where
      newName :: String -> m Name

An instance for ``Quote`` can be implemented
for ``Q``, retaining backwards compatibility, but also ``State NameSupply`` to
enable a pure way to extract expressions.

``Q`` is not necessary for the implementation of quotations.
It provides much more power than is necessary. Quotations are used in order to
construct a representation of expressions, the only effect which is used in the
implementation is a name generation effect.

Detaching quotations from ``Q`` makes way for a form of "pure" template haskell
so there is no need to invoke ``Q`` in order to create the representation of an
expression. The most immediate application is the ability to purely
manipulate ``Exp`` and ``TExp a`` values in user libraries.

Another benefit is that in a cross compilation setting a "pure" quote can be
fully evaluated on the host and then the generated code compiled for the target.
Certain effects in the ``Q`` monad mean that currently all splices have to be
evaluated on the target which leads to significant complication when
cross-compiling.

The proposal doesn't deal with anything to do with Typed Template Haskell at
all. This generalisation is expected to follow in a future proposal.



Proposed Change Specification
-----------------------------

The goal of the changes is for an expression ``e : T`` to give the
representation ``[| e |] : Quote m => m Exp`` and ``[|| e ||] : Quote m => m (TExp a)``.
Several steps are necessary to make this change possible.

1. Define the interface for ``Quote``::

      class Applicative m => Quote m where
         newName :: String -> m Name

   These are all the operations which are necessary to build the representation
   of expressions.

2. Generalise all the combinators which build syntax in ``Language.Haskell.TH.Lib``.
   Due to an `audit <https://github.com/ghc-proposals/ghc-proposals/issues/211#issuecomment-472092412>`_
   conducted by Richard, it was found that the only effect from
   ``Q`` which was used is the ``newName`` function which generates a fresh
   name. All the other combinators can be defined using the ``Applicative``
   operations.


3. Generalise the ``Lift`` type class::

      class Lift a where
         lift :: Quote m => a -> m Exp
         liftT :: Quote m => a -> m (TExp a)

   This is necessary so that implicit lifting can continue to work without
   enforcing strong constraints on the type of the bracket.

4. Refine the rules to do with splicing.  The type of
   a quotation depends on the types of the nested splices inside it::

      -- Add a redundant constraint to demonstrate that constraints on the
      -- monad used to build the representation are propagated when using nested
      -- splices.
      f :: (Quote m, C m) => m Exp
      f = [| 5 | ]

      -- Likewise for typed TH
      ft :: (Quote m, C m) => m (TExp Int)
      ft = [| 5 | ]

      -- f is used in a nested splice so the constraint on f, namely C, is propagated
      -- to a constraint on the whole representation.
      g :: (Quote m, C m) => m Exp
      g = [| $f + $f |]

      -- Likewise for typed TH
      gt :: (Quote m, C m) => m (TExp Int)
      gt = [| $$ft + $$ft |]

   A top-level splice still requires its argument to be of type ``Q Exp``,
   and top-evel typed splice likewise ``Q (TExp a)``.
   So then splicing in ``g`` will cause ``m`` to be instantiated to ``Q``::

    h :: Int
    h = $(g) -- m ~ Q

    ht :: Int
    ht = $$(gt) -- m ~ Q

5. The types of type, pattern and declaration quotes will also
   be generalised in the same manner.


Effect and Interactions
-----------------------

When making an interface more general it is important to think about whether it
will affect type inference. If there are functions where we have to generalise
the argument type but not the result then generalisation can result in
ambiguity in the composition.

It doesn't seem to me that there will be any problems with ambiguity here as
the types of splices is not overloaded in the same manner.


Interaction with Lift
.....................

The main breakage from this patch comes from modifying the type signature for
``lift``.

Instances defined using ``DeriveLift`` will continue to work because they are
defined in terms of quotation brackets.

Instances written in terms of the combinators from ``Language.Haskell.TH.Lib``
will continue to work because these combinators will be generalised.

Instances written in terms of ``Q`` will no longer work. For users to migrate
an additional class ``LiftQ`` could be defined which has the old interface.
This would mean users need to explicitly lift but there are likely only a few
instances which fall into this category if any at all.

Definition of Quote
...................

Richard observes that ``Language.Haskell.TH.Lib.Internal.numTyLit`` calls
``fail`` from the ``Q`` monad. This call to ``fail`` can be replaced with
a call to ``error``. It will still be executed at compile-time but with a
potentially slightly worse error message.

Other Effects
.............

Vlad points out that you don't need to very strict about the types of
expressions in splices. Each nested splice could have different constraints::

      f :: Quasi m => m Exp
      g :: MonadIO m => m Exp
      [| putStrLn $(f) >> putStrLn $(g) |] :: (Applicative m, Quasi m, MonadIO m) => m Exp

      ft :: Quasi m => m (TExp a)
      gt :: MonadIO m => m (TExp a)
      [| putStrLn $(f) >> putStrLn $(g) |] :: (Applicative m, Quasi m, MonadIO m) => m (TExp a)

If one of the nested splices has a specific type, for instance ``Q Exp``/``Q (TExp Int)``, then
the type of the whole expression is fixed to be ``Q Exp``/``Q (TExp Int)``.


Costs and Drawbacks
-------------------

* The generalisation of untyped brackets does not seem like it will cause
  any significant breakage but it's hard to predict.
* The modification to the ``Lift`` interface could cause user-written instances
  to break but users should not define their own instances anyway.
  ``DeriveLift`` is the blessed manner in which to define a ``Lift`` instance.

Alternatives
------------

* Just keep things the way they are.

Unresolved Questions
--------------------

* Carter points out that if you want to achieve "pure" template haskell then
  you still need to deal with the fact that different platforms have different
  representations of primitive data types. This is out of scope of this
  proposal.

* It would also be possible to make ``Quote`` a superclass of ``Q`` but
  this hierarchy refactoring seems unecessary.

Implementation Plan
-------------------

* I (mpickering) will implement this.
