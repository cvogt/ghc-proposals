Class Method Headers
====================

.. author:: Vladislav Zavialov
.. date-accepted::
.. proposal-number::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/267>`_.
.. sectnum::
.. contents::

We propose to allow class variable binders on the left of ``::`` in class method signatures::

   class C a b where
      f @b @a :: (a, b) -> Bool

   -- f :: forall b a. C a b => (a, b) -> Bool

The ordering of these variables may differ from their ordering in the class declaration header.

The syntax mirrors that of associated type headers and
`Type variable binders <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst>`_.

Motivation
------------

At the moment, the signature of a class method is prefixed with class variables
and a class constraint::

  class C a b where
    f :: (a, b) -> Bool

  -- f :: forall a b. C a b => (a, b) -> Bool
  --      ^^^^^^^^^^^^^^^^^^^^
  --      compiler-generated prefix


The users have no way to guide generation of this prefix. In particular, the
ordering of the class variables is inherited from the class declaration header.
This matters at use sites with ``-XTypeApplications``::

  f @Int :: forall b. C Int b => (Int, b) -> Bool

What if we wanted a different ordering? ::

  f @Int :: forall a. C a Int => (a, Int) -> Bool

We cannot specify that we'd prefer  ``forall b a.`` instead of ``forall a b.``
for this particular method. There are two workarounds:

* Change the ordering of class variables in the class declaration header::

    class C b a where   -- instead of:  class C a b

  This has the downside that now the class constructor has a different ordering
  of variables, not just its method. Also, it does not give per-method control.

* Create a wrapper function::

    fWrapper :: forall b a. C a b => (a, b) -> Bool
    fWrapper = f @a @b

  This means that users have to define instances using original method names,
  but call the methods using the wrappers, which leads to more complicated
  APIs.

Variable ordering aside, consider visible dependent quantification that we have
in types today::

  data I   (a :: k)   --  I :: forall k.   (a :: k) -> Type
  data V k (a :: k)   --  V :: forall k -> (a :: k) -> Type

We distinguish invisible forall (``forall k.``) and visible forall (``forall k
->``). With the advent of dependent types, a similar feature will be added at
the term level, and then we will face the question of how to choose between
visible and invisible ``forall`` for class method variables.

This proposal provides an excellent forward compatibility story: we can simply
omit the ``@`` symbol in binders to indicate visibility::

  class C k (a :: k) where
    f @k a :: P a

  -- C :: forall k -> k -> Constraint
  -- f :: forall k. forall (a :: k) -> P a

That is, not only the ordering, but also the visibility of class variables can
differ between the class constructor and class methods.

Furthermore, class method headers are a limited version of the same feature of
associated types, and as such, it will be an indispensable asset in their
unification as described in the `Grand Class Unification
<https://github.com/ghc-proposals/ghc-proposals/pull/236>`_ meta-proposal.
Compare ``f`` and ``F``, which are now quite similar::

  class C a where
    type F a :: P a
    f a :: P a

    -- F :: forall a ->        P a
    -- f :: forall a -> C a => P a

To summarize, there are three reasons to make this change:

* Control of class variable ordering in class methods for use with ``-XTypeApplications``.
* Control of class variable visibility with advent of visible ``forall`` in terms.
* A step toward unification of associated types and class methods.

Proposed Change Specification
-----------------------------

Syntax
~~~~~~

Take the Haskell 2010 class method signature grammar as the starting point::

  gendecl -> vars :: [context =>] type
  vars    -> var_1 , ... , var_n         (n ≥ 1)

Instead of variable names for class method left-hand sides, we introduce the
notion of a signature header::

  sighdr  -> var (sigbndr_1 ... sigbndr_n)
  sigbndr -> tyvar
           | @tyvar

  gendecl -> sighdrs :: [context =>] type
  sighdrs -> sighdr_1 , ... , sighdr_n         (n ≥ 1)

A validity check ensures that the binders are only used in class method
signatures and are disallowed in function signatures.

Semantics
~~~~~~~~~

* When one or more binders are present in a class method signature, we consider
  it a class method header, and require the ``-XClassMethodHeaders`` extension.

* A class method header must bind every class variable mentioned in the class
  declaration header, and must bind it exactly once.

* No other variables can be bound in the class method header.

* In the compiler-generated top-level signature for the class method, variables
  bound as ``@a`` are quantified with ``forall a.``, and variables bound as
  ``a`` are quantified with ``forall a ->``.

* In the compiler-generated top-level signature for the class method, the ordering
  of quantifiers matches the ordering of binders in the class method header.

* The compiler-generated top-level signature for the class method is subject to
  validity checking, which should reject variables quantified out of dependency
  order and the (as of yet) unsupported visible ``forall``.

* Class method definitions in instance declarations may explicitly write out
  class variable instantiations::

    class C a where
      f @a :: a

    instance C Int where
      f @Int = 42

  This mirrors the syntax of associated type definitions (associated type family equations).

Examples
--------

* Comma-separated class methods with different class variable ordering::

    class C a b where
      f @a @b, g @b @a :: a -> b

    -- f :: forall a b. C a b => a -> b
    -- g :: forall b a. C a b => a -> b

* Erroneous class method header that mentions non-class variable::

    class C a where
      f @b :: a -> b

  Rejected with the following message::

    • ‘b’ is not bound in the class declaration header ‘C a’
    • In the class method header: f @b

  The implementation may opt to provide a different error message in the same spirit.

* Erroneous class method header that mentions class variables out of dependency order::

    class C (a :: k) where
      f @a @k :: P a

    -- f :: forall a k. C (a :: k) => P a

  Rejected with the following message::

    • These kind and type variables: a k
      are out of dependency order. Perhaps try this ordering:
        k (a :: k)
    • In the compiler-generated class method signature:
        f :: forall a k. C (a :: k) => P a

  This is the same message as one would get if this signature was written by hand.
  The implementation may opt to provide a different error message in the same spirit.

* Erroneous class method header that uses (as of yet) unsupported visible ``forall`` in terms::

    class C (a :: k) where
      f @k @a :: P a

    -- f :: forall k. forall a -> C (a :: k) => P a

  Rejected with the following message::

    • Illegal visible, dependent quantification in the type of a term:
        forall k. forall (a :: k) -> C a => P a
      (GHC does not yet support this)
    • In the compiler-generated class method signature:
        f :: forall k. forall a -> C (a :: k) => P a

  This is the same message as one would get if this signature was written by hand.
  The implementation may opt to provide a different error message in the same spirit.

Effect and Interactions
-----------------------

The immediate pay-off of this change is that users get the ability to specify
the ordering of class variable quantification in class methods for use with
``-XTypeApplications``.

The long-term pay-off is that it offers syntax for visible quantification of
class variables and represents one of the steps in the `Grand Class Unification
<https://github.com/ghc-proposals/ghc-proposals/pull/236>`_  plan.

Costs and Drawbacks
-------------------

This is one more feature to implement and support.


Alternatives
------------

`Top-level signatures
<https://github.com/ghc-proposals/ghc-proposals/pull/148>`_ (not to be confused
with top-level kind signatures) is a different take on this issue.


Unresolved Questions
--------------------

None at the moment.

Implementation Plan
-------------------

I (Vladislav Zavialov) will implement.
