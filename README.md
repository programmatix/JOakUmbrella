# C to JVM
Reads C source and compiles it into Java .class files so it can run on the JVM.

## Status
Parser working well with quite complex source, probably a few bugs left.
A C generator working quite well, great for testing the parser.
A ScalaJS widget that turns C into the AST, and back into C.
JVM generation just started.

## Projects
CParser: Parse C into a clean AST. And XML, and CLI?
Enhanced javap: much better error diagnostics when you're trying to generate a class file.  Can parse a class file into XML or a case class.

Desirables
==========
* Better display of errors
* Handle comments

Release Process
===============
sbt
fastOptJS
fullOptJS

Copy to site:
copy
copyProd

Build & release site
