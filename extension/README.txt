                 OCAML TUTORIAL: Interacting with C++ Library 

This example demonstrates how to interface OCaml with C++ libraries using the
standard OCaml C stub approach. OCaml provide support for standard C
libraries. To use it the C++ library needs to provide API for C. The
dependencies of this example is illustrated as the following

 [OCaml Module]     [C Stub library]    [ C++ library ]   [Std C++ library]
     My (my.ml)  =>    libmystub.a    =>    libmy.a     =>  libstdc++

The 'stubbing' mechanism in OCaml is as follows. The OCaml module defines the
API in the OCaml language. Its implementations are provided separately in a c
files. The name and type of the C functions are specified in the OCaml module as
'external' functions. The OCaml compiler knows how to link the module with the
C stub implementations. However, this workflow is different from regular OCaml
bytecode workflow in that native code is involved. This example illustrates two 
potential uses:

* Native executables
* Bytecode executable with custom runtime
* Top-level loop compiled with custom runtime environement

While native code is straightforward to understand. The 'custom runtime' has the
name because it is a mixture of bytecode and native code. The module is compiled
as bytecode. But the stub library, library and runtime (ocamlrun) are linked
statically into this file. This is referred to as the 'static' linking option in
the OCaml documentation.

=== Requirements ===
* A unix OCaml development environment (OCaml installed natively in the system)
* g++, gcc

=== Content ===

lib/ - A simple C++ library that wraps std::vector<double> and provides C APIs.

_tags - Tags of various targets to use with ocamlbuild.

my.ml - An OCaml binding for the C++ library.

mystub.c - C stub code for my.ml

libmystub.clib - Definition of the C stub library

mytop.top - Definition of a custom top-level loop for rapid prototyping

myocamlbuild.ml - ocamlbuild plugin with additional rules and recipes for building stub library and linking with C++ library.

main.ml - A simple executable file.

=== Usage ===

1) Compile C++ library: 
  cd lib
  make

2) Build native executable 
  ocamlbuild main.native -no-hygiene

3) Build bytecode executable (custom runtime)
   ocamlbuild main.byte -no-hygiene

4) Build custom top-level loop (with custom runtime)
   ocamlbuild mytop.top -no-hygiene


   To run the top-loop
     cd _build
     ./mytop.top
     
      Objective Caml version 3.12.1

      # open My;;
      # let a = MyVector.create () ;;
      val a : My.MyVector.t = <abstr>
      # MyVector.append a 1.0;;
      - : unit = ()
      #  MyVector.append a 2.0;;
      - : unit = ()


