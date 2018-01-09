Delimited Overloading
=====================

Description
-----------

This package contains three syntax extensions.
- pa_infix
- pa_do      (includes Delimited_overloading, Macros).
- pa_do_nums

Building
--------

Type "make".

Installing
----------

To install the library, you need to have findlib installed on your
system (see http://projects.camlcity.org/projects/findlib.html).  Then
just type "make install".  You can uninstall with "make uninstall".

Using
-----

The easier way to use this syntax extension is through findlib.  We
define three packages: pa_do (macros and delimited overloading),
pa_do.num (arbitrary-precision numbers) and pa_do.infix (priority and
associativity of unary and binary operators).  For example, you can
compile natively code that use this syntax extension with

    ocamlfind ocamlopt -syntax camlp4o -package pa_do ...


Testing
-------

After creating the library with "make", you can run the automated
tests with "make test".



Acknowledgments
---------------

This project is sponsored by Jane Street Capital
(http://www.janestcapital.com/) through their OCaml Summer Project
2008 (http://osp.janestcapital.com).  We are very grateful for their
support.
