# C to JVM
Reads C source and compiles it into Java .class files so it can run on the JVM.
Has ballooned into a number of problems related to creating, reading and even running JVM .class files.

## Status
Parser working well with quite complex source, probably a few bugs left.
A C generator working quite well, great for testing the parser.
A ScalaJS widget that turns C into the AST, and back into C.  Working well.
JVM class file generation started, but proving quite hard.
JVM class file reading coming along, can read basic files.
A JVM implemention, can do hello world.

## Projects
CParser: Parse C into a clean AST. And XML, and CLI?
Enhanced javap: much better error diagnostics when you're trying to generate a class file.  Can parse a class file into XML or a case class.

## JVM release process
sbt assembly
launch4j to produce exe


Release Process
===============
sbt
fastOptJS
fullOptJS

Copy to site:
copy
copyProd

Build & release site
