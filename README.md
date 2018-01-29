# Overview
This has ballooned into a number of problems related to creating, reading and even running JVM .class files.

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

## Other ideas
Inline JVM ASM, like C __asm - probably in Scala
A DSL or toy language, compiled to JVM
A Java parser
A Java compiler

# C to JVM
Reads C source and compiles it into Java .class files so it can run on the JVM.

## JVM release process
sbt assembly
launch4j to produce exe

# JVM

## Terms
Managed code/class - Handled by this JVM, vs unmanaged which is handled by the standard Java JVM we're running upon.

## Limitations
This is a toy JVM running on top of a real JVM, so some parts are left to the real JVM, including:

* Memory management.
* Loading and execution of core Java libraries, like String, or any jars passed to the standard Java classloader.
* Threading.

Unlikely to be supported:
* Jar files.
* synchronized keyword.
* native keyword.


Release Process
===============
sbt
fastOptJS
fullOptJS

Copy to site:
copy
copyProd

Build & release site
