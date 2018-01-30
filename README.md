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

# Javap replacement
Benefits:
* Displays fields (actually javap does this)
* In verbose mode, logs as it reads so gives much better debugging if something's wrong with the .class file.

# JVM
A toy Java Virtual Machine, written in Scala. 

## Terms
Managed code/class - Handled by this JVM, vs unmanaged which is handled by the standard Java JVM we're running upon.

## Limitations
This is a toy JVM intended to be a learning exercise.  A real optimised and fully-functional JVM would take at least man-months of effort, so many important features aren't implemented.  These include: 

* .class files compiled with Java 7+.
* Jar files.
* synchronized keyword.
* native keyword.
* float & double operations.

In addition, some important parts work but are left to the 'real' Java JVM we're running on, including:

* Memory management.
* Loading and execution of core Java libraries, like String.
* Threading.


Release Process
===============
sbt
fastOptJS
fullOptJS

Copy to site:
copy
copyProd

Build & release site
