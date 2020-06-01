Visible ``forall`` in types of terms
====================================

.. author:: Vladislav Zavialov
.. date-accepted::
.. proposal-number::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/281>`_.
.. sectnum::
.. contents::

We propose to allow visible irrelevant dependent quantification, written as
``forall x ->``, in types of terms.

NB. This proposal assumes that the `"Extend term-level lookup rules"
<https://github.com/ghc-proposals/ghc-proposals/pull/270>`_
proposal has been accepted.

Background
----------

(Skip to "Motivation" if you are comfortable with the notions of dependence,
relevance, and visibility, when talking about quantifiers).

Function parameters can be bound using various quantifiers. Consider the
identity function::

  id :: forall a. a -> a
  id = \x -> x

There are two parameters to ``id``:

1. ``a :: Type``
2. ``x :: a``

Quantifiers are parts of the type that introduce the function parameter. In the
type of ``id``, there are two quantifiers:

1. ``forall a.`` introduces ``a :: Type``
2. ``a ->`` introduces ``x :: a``

We classify quantifiers along several axes:

* Dependent or non-dependent
* Relevant or irrelevant
* Visible or invisible

Dependence
~~~~~~~~~~
We call a quantifier dependent when the parameter can be used in the type of
the function result. ``forall a.``, which introduces ``a :: Type``, is a
dependent quantifier::

  id :: forall a. a -> a
                 ^^^^^^^^^^^^^^^^
                 'a' is used here

On the other hand, ``a ->``, which introduces ``x :: a``, is a non-dependent quantifier::

  id :: forall a. a -> a
                      ^^^^^^^^^^^^^^^^^^^^^^^
                      'x' cannot be used here

Relevance
~~~~~~~~~
We call a quantifier relevant when the parameter can be used in the function
body in a position other than a type annotation. Intuitively, it means that
relevant parameters are relevant to the evaluation of the function and must be
passed at runtime. ``forall a.``, which introduces ``a :: Type``, is not a
relevant quantifier::

  id :: forall a. a -> a
  id = \x -> x
      ^^^^^^^^^
      'a' cannot be used here (other than as a type annotation
                               with scoped type variables)

On the other hand, ``a ->``, which introduces ``x :: a``, is a relevant
quantifier::

  id :: forall a. a -> a
  id = \x -> x
            ^^^
            'x' is used here

Visibility
~~~~~~~~~~
We call a quantifier visible when the parameter must be specified at use sites,
and invisible when the compiler tries to infer it at use sites.

Consider an expression such as ``id True``. In this call, we have:

* ``x=True``, as specified
* ``a=Bool``, as inferred from ``(x :: a) = (True :: Bool)``

The reason we don't write ``id Bool True`` is that ``forall a.`` is an
invisible quantifier, while ``a ->`` is a visible quantifier.

With the ``TypeApplications`` extension, we can use a visibility override ``@``
to specify an invisible parameter as if it was visible::

  id @Bool True

Motivation
----------
In types of types (in kinds), we have the choice between invisible and visible
dependent quantification::

  type PInv :: forall k. k -> Type  -- invisible quantification of 'k'
  data PInv a = MkPInv

  type PVis :: forall k -> k -> Type  -- visible quantification of 'k'
  data PVis k a = MkPVis

Invisible parameters, introduced with ``forall x.``, are inferred by the
compiler at use sites. Visible parameters, introduced with ``forall x ->``,
must be specified by the user::

  type TInv = PInv     15   -- infer (k~Nat) from (a::k)~(15::Nat)
  type TVis = PVis Nat 15   -- no inference

This means our quantifier grid is complete with regards to dependence and
visibility::

  Quantifiers in
  types of types    Dependent     Non-dependent
                 +--------------+---------------+
        Visible  | forall a ->  |  a ->         |
                 +--------------+---------------+
      Invisible  | forall a.    |  c =>         |
                 +--------------+---------------+

On the other hand, in types of terms, our grid is incomplete::

  Quantifiers in
  types of terms    Dependent     Non-dependent
                 +--------------+---------------+
        Visible  |              |  a ->         |
                 +--------------+---------------+
      Invisible  | forall a.    |  c =>         |
                 +--------------+---------------+

Other than making terms and types more symmetrical, filling this empty cell
would let us design better APIs without the use of proxy types or ambiguous
types, and with better error messages.

For example, consider a function that gives the memory residence for a type::

  sizeOf :: forall a. Sized a => Proxy a -> Int

To find out the size of a boolean value, the user of this API would write
``sizeOf (Proxy :: Proxy Bool)`` or ``sizeOf (Proxy @Bool)``. This has two disadvantages:

* Constructing a ``Proxy`` value is unnecessarily verbose, making ``sizeOf``
  clunky to use.

* The ``Proxy`` value is passed at runtime. Even if the optimizer can eliminate
  it sometimes, there are cases when it cannot.

There is a workaround which involves ``AllowAmbiguousTypes`` and
``TypeApplications``. Here's an alternative API design::

  sizeOf :: forall a. Sized a => Int

The user is supposed to use a visibility override, ``sizeOf @Bool``. While it
does address the concerns about verbosity and the runtime cost, the error
messages degrade significantly. The invisible parameter ``a`` is now ambiguous,
so if the user forgets to specify it, the compiler tries to infer ``a`` and
inevitably fails::

  print_int :: Int -> IO ()

  -- Valid code:
  main = print_int (sizeOf @Bool)

  -- The parameter is not specified, extremely bad error message:
  --
  --    • Ambiguous type variable ‘a0’ arising from a use of ‘sizeOf’
  --      prevents the constraint ‘(Sized a0)’ from being solved.
  --      Probable fix: use a type annotation to specify what ‘a0’ should be.
  --      These potential instance exist:
  --        instance [safe] Sized Bool -- Defined at <interactive>:15:10
  --    • In the first argument of ‘print_int’, namely ‘sizeOf’
  --      In the expression: print_int sizeOf
  --      In an equation for ‘main’: main = print_int sizeOf
  --
  main = print_int sizeOf

It also means that eta-reduction is not possible::

  -- Valid code:
  mySizeOf :: forall a. Sized a => Int
  mySizeOf @a = sizeOf @a

  -- Eta-reduction attempt fails:
  --
  --  • Could not deduce (Sized a0) arising from a use of ‘sizeOf’
  --    from the context: Sized a
  --      bound by the type signature for:
  --                 mySizeOf :: forall a. Sized a => Int
  --    The type variable ‘a0’ is ambiguous
  --
  mySizeOf :: forall a. Sized a => Int
  mySizeOf = sizeOf


If we had visible ``forall``, for which there is already precedent at the
type-level, we could design an API for ``sizeOf`` that has none of the issues
listed above::

  sizeOf :: forall a -> Sized a => Int

This type captures the intent behind this function, and, if we allow it, its
use would have the least noise and good error messages::

  print_int :: Int -> IO ()

  -- Valid code:
  main = print_int (sizeOf Bool)   -- NB: no visibility override '@'


  -- The parameter is not specified, good error message:
  --
  --    • Couldn't match expected type ‘Int’
  --                with actual type ‘forall a -> Sized a => Int’
  --    • Probable cause: ‘sizeOf’ is applied to too few arguments
  --      In the first argument of ‘print_int’, namely ‘sizeOf’
  --      In the expression: print_int sizeOf
  --      In an equation for ‘main’: main = print_int sizeOf
  --
  main = print_int sizeOf

Eta-reduction is now possible::

  -- Valid code:
  mySizeOf :: forall a -> Sized a => Int
  mySizeOf a = sizeOf a

  -- Eta-reduction attempt succeeds:
  mySizeOf :: forall a -> Sized a => Int
  mySizeOf = sizeOf

The proposed visible ``forall`` would be an irrelevant quantifier. However, if
we were to make it relevant, we would get full-blown dependent functions
(pi-types). Therefore, implementing this feature would pave the road for future
work on Dependent Haskell.

To summarize, there are three reasons to make this change:

* Language consistency (symmetry between terms and types)
* Ability to design better APIs (good error messages, no proxy types, no ambiguous types)
* Prepare the compiler internals for further work on dependent types

Proposed Change Specification
-----------------------------

* Add a new language extension, ``VisibleForAll``.

* When ``VisibleForAll`` is in effect, lift the restriction that the ``forall a
  ->`` quantifier cannot be used in types of terms.

* In types of terms, ``forall a ->`` is an irrelevant quantifier.

* Parsing and name resolution are not affected. Given ``f :: forall a -> t``,
  while ``x`` in ``f x`` is a type, it is parsed and renamed as a term, and
  then reinterpreted as a type:

  * A data constructor ``MkT`` is reinterpreted as a promoted data constructor
    ``MkT`` and requires the ``DataKinds`` extension.

  * A numeric literal ``42`` is reinterpreted as a promoted numeric literal and
    requires the ``DataKinds`` extension.

  * A string literal ``"Hello"`` is reinterpreted as a promoted string literal
    ``"Hello"`` and requires the ``DataKinds`` extension.

  * A character literal ``'x'`` cannot be reinterpreted at the moment, as we do
    not have promoted character literals.

  * A term-level variable ``a`` cannot be reinterpreted and its use is an
    error, as we do not have full dependent types at this stage.

  * Function application ``f a`` is reinterpreted as type-level function
    application ``f a``.

  * Type application ``f @a`` is reinterpeted as type-level type application
    ``f @a`` and requires the ``TypeApplications`` extension.

  * Operator application ``x + y`` is reinterpreted as type-level operator
    application ``x + y`` and requires the ``TypeOperators`` extension.

  * A type signature ``a :: t`` is reinterpreted as a kind signature ``a :: t``
    and requires the ``KindSignatures`` extension.

  * Lambda functions ``\x -> b`` are not reinterpreted and their use is an
    error, as we do not have type-level lambdas at the moment.

  * Case-expressions ``case x of ...`` are not reinterpreted and their use is
    an error, as we do not have type-level case-expressions.

  * If-expressions ``if c then a else b`` are not reinterpreted and their use
    is an error, as we do not have type-level if-expressions.

  * In the same spirit, other syntactic constructs are reinterpreted when
    there's a direct type-level equivalent, and their use is an error
    otherwise.

* When ``VisibleForAll`` is in effect, make ``forall`` a keyword at the term
  level. Add a warning ``-Widentifier-forall``, included in ``-Wcompat``, which
  warns on identifiers named ``forall``. In three releases, make ``forall`` a
  keyword everywhere.

* Extend the term-level syntax with ``a -> b``, ``a => b``, ``forall a. b``,
  and ``forall a -> b``, so that these constructs can be reinterpreted as
  types.

Examples
--------

Compile-time color literals
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Definition site::

  type family ParseRGB (s :: Symbol) :: (Nat, Nat, Nat) where
    ...

  type KnownRGB :: (Nat, Nat, Nat) -> Constraint
  class KnownRGB c where
    _rgbVal :: (Word8, Word8, Word8)

  rgb :: forall s -> KnownRGB (ParseRGB c) => (Word8, Word8, Word8)
  rgb s = _rgbVal @(ParseRGB s)

Use site::

  ghci> rgb "red"
  (255, 0, 0)

  ghci> rgb "#112233"
  (17, 34, 51)

  ghci> rgb "asdfasdf"
  -- custom type error from ParseRGB

Effect and Interactions
-----------------------

* Visible ``forall`` becomes available in types of terms, making them more similar to
  types of types. There remains a discrepancy that ``forall`` for types is actually a
  relevant quantifier, while the proposed ``forall x ->`` for terms is
  irrelevant. This is to be resolved in the future by making type-level
  ``forall`` irrelevant.

* The renaming of a visible dependent argument is different than that of a
  dependent argument with a visibility override. Consider this code::

    f :: forall a.   Tagged a ()
    g :: forall a -> Tagged a ()

    data T = T

    a = f @T
    b = g  T

  In ``f @T``, we refer to the type constructor, but in ``g T`` we refer to the
  data constructor.

  This issue is resolved by using ``-Werror=punning``.

Costs and Drawbacks
-------------------

This is one more feature to implement and maintain.

Alternatives
------------

* Keep types and terms forever different by not supporting visible ``forall``
  in terms.

* Include the proposed functionality in ``ExplicitForAll`` instead of
  introducing a new extension.

* The extension name could use different capitalization or pluralization
  (``VisibleForall``, ``VisibleForalls``, ``VisibleForAlls``). The proposed
  variant is consistent with ``ExplicitForAll``.

* We could guard type-level uses of visible ``forall`` behind the
  ``VisibleForAll`` extension flag. This would break existing code.

* To avoid the name resolution issues, we could introduce a syntactic marker to
  indicate visible type arguments. That is, for some ``f :: forall x
  -> ...``, one would pass ``x`` as ``f ^x`` instead of ``f x``, where ``^``
  is new syntax (and a different choice of symbols is possible). There are
  several issues with this alternative:

  * it creates more syntactic noise
  * it is inconsistent with what we have in types where we do not need a marker
  * it does not move us towards pi-types

  As such, it only serves one point of the motivation (design better APIs) at
  the expense of the other two (language consistency and paving the ground for
  dependent types).

* We could repurpose ``@`` as a syntactic marker that indicates types occurring
  within terms. That is, while ``forall x ->`` is a compulsory parameter and
  ``forall x.`` is not, the use sites would be ``f @Int`` in both cases.

  The criticism from the previous alternative applies as well:

  * it creates more syntactic noise
  * it is inconsistent with what we have in types where we do not need a marker
  * it does not move us towards pi-types

  In addition to that, there's another issue:

  * The dual purpose of ``@`` as both a visibility override and a namespace
    specifier would lead to unwanted interference between ``forall x.`` and
    ``forall x ->``. For example, given ``f :: forall k. forall (a::k) ->
    blah``, it wouldn't be possible to specify ``a=Int`` as ``f @Int``;
    one would have to write ``f @_ @Int`` or change the type of ``f`` to ``f
    :: forall {k}. forall (a::k) -> blah``.


Unresolved Questions
--------------------

None at the moment.

Implementation Plan
-------------------

I (Vladislav Zavialov) will implement this change.
