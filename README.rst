Equals Assessment
=================

This repo will play host to the (re)implementation of the Equals Assessment System.
In an attempt to keep the tool small, fast and easily maintainable, the whole system (from the internal calculations, to the CSV export to the UI) will be implemented in Haskell using the lightest-weight solutions we can manage.

Install Process
---------------

First, install the `Haskell Platform <https://www.haskell.org/downloads>`_ or ``ghc`` version 7.10 (we use some prelude changes present in this version).

Next, clone `this repository <https://github.com/AbleNetInc/EqualsAssessment>`_, ``cd`` into the resulting directory and run the following commands:

.. code:: console

    $ cabal sandbox init
    $ cabal install
    $ cabal build

Following this, you can run the project simply by using ``cabal run -- <arguments>``.

All internal dependencies are handled by ``cabal``, but some of them rely on external dependencies to be installed as well (namely ``sqlite3``).
