Equals Assessment
=================

This repo will play host to the (re)implementation of the Equals Assessment System.
In an attempt to keep the tool small, fast and easily maintainable, the whole system (from the internal calculations, to the CSV export to the UI) will be implemented in Haskell using the lightest-weight solutions we can manage.

At the moment, we have the following dependencies:

- `GHC (or the Haskell Platform) >= 7.10 <https://www.haskell.org/downloads>`_
- `sqlite3 >= 0.5.2.2 <https://hackage.haskell.org/package/sqlite-0.5.2.2>`_

Note that the ``sqlite3`` Haskell package includes a few other dependencies and will require that ``sqlite3`` itself be installed.
