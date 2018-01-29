package jvm

import java.io.{File, FileFilter}

import jvm.JVMClassFileReader.ReadParams

import scala.collection.mutable.ArrayBuffer

/*
A custom class loader.  This will be used by our custom JVM whenever it needs to load a class.

This could easily 'cheat' and use Java's standard class loader to load any class.  This wouldn't be fair, but equally,
this is a toy JVM and it's too much work to try and handle all the Java standard code.

As a compromise, our custom class loader will use Java's bootstrap class loader as its parent.  This will handle core
classes such as java.lang.  This class loader will handle everything else - e.g. all user-provided code, plus non-core
libraries.

Update. Found some issues with that approach:
1. Can only get system class loader, not bootstrap.
2. Our classloader can't be a 'true' classloader, returning a Class. It doesn't know how to build a Class.

So:
1. Make sure IntelliJ doesn't build test classes, so they don't get picked up by the system classloader.
2. Have our JVM check this classloader first, then check the system classloader.

Wiki:
When the JVM is started, three class loaders are used:[3][4]

Bootstrap class loader

Extensions class loader

System class loader

The bootstrap class loader loads the core Java libraries[5] located in the /jre/lib directory. This class loader, which is part of the core JVM, is written in native code.

The extensions class loader loads the code in the extensions directories (/jre/lib/ext,[6] or any other directory specified by the java.ext.dirs system property). It is implemented by the sun.misc.Launcher$ExtClassLoader class.

The system class loader loads code found on java.class.path, which maps to the CLASSPATH environment variable. This is implemented by the sun.misc.Launcher$AppClassLoader class.
 */
case class JVMClassLoaderParams(verbose: Boolean = false,
                                classfileRead: ReadParams = ReadParams())

class JVMClassLoader(paths: Seq[String], params: JVMClassLoaderParams = JVMClassLoaderParams()) {
  private[jvm] val classFiles = ArrayBuffer.empty[JVMClassFileBuilderForReading]

  def loadClass(name: String): Option[JVMClassFileBuilderForReading] = {
    var out: Option[JVMClassFileBuilderForReading] = None

    if (params.verbose && paths.isEmpty) println(s"Classloader: no paths configured")

    paths.foreach(path => {
      val dir = new File(path)
      val javaFiles = dir.listFiles(new FileFilter {
        override def accept(pathname: File): Boolean = pathname.getName.stripSuffix(".class") == name
      })
      if (params.verbose) {
        println(s"Classloader: [${dir.getCanonicalPath}] found ${javaFiles.size} files matching required class $name")
      }
      javaFiles.foreach(javaFile => {
        JVMClassFileReader.read(javaFile, params.classfileRead) match {
          case Some(cf) =>
            classFiles += cf
            out = Some(cf)

          case _ => JVM.err(s"unable to read classfile ${javaFile.getCanonicalPath}")
        }

      })
    })

    out
  }
}
