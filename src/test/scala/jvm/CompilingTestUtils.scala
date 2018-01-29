package jvm

import java.io.{File, IOException}
import java.util
import javax.tools.{DiagnosticCollector, JavaFileObject, ToolProvider}

import jvm.JVMByteCode.{JVMOpCodeWithArgs, JVMVar, JVMVarObjectRefManaged}


object CompilingTestUtils {
  def compareStack(sf: StackFrame, expected: Array[JVMByteCode.JVMVarInt]): Boolean = {
    val real = sf.stack.toArray
    assert (real sameElements  expected, s"${real.mkString(",")} != ${expected.mkString(",")}")
    real sameElements  expected
  }


  def containsVar(in: StackFrame, value: JVMVar): Boolean = {
    in.locals.find(_._2 == value).nonEmpty
  }

  def getKlassInstanceLocal(in: StackFrame): JVMClassInstance = {
    in.locals.find(_._2.isInstanceOf[JVMVarObjectRefManaged]).get._2.asInstanceOf[JVMVarObjectRefManaged].klass
  }


  case class ExecuteOpcodeResult(jvm: JVM, sf: StackFrame)

  def executeOpcode(opcodes: Seq[JVMOpCodeWithArgs], params: ExecuteParams = ExecuteParams()): ExecuteOpcodeResult = {
    val classLoader = new JVMClassLoader(Seq())
    val jvm = new JVM(classLoader)
    val sf = new StackFrame(null, "fake")
    jvm.executeFrame(sf, opcodes, params)
    ExecuteOpcodeResult(jvm, sf)
  }

  // Compiles a .java file to .class and returns the .class filename, if successful
  def compileJavaFile(javaFile: File): Option[File] = {

    try {
      // https://stackoverflow.com/questions/21544446/how-do-you-dynamically-compile-and-load-external-java-classes
      val diagnostics = new DiagnosticCollector[JavaFileObject]()
      val compiler = ToolProvider.getSystemJavaCompiler()
      val fileManager = compiler.getStandardFileManager(diagnostics, null, null)
      val optionList = new util.ArrayList[String]()
      //      optionList.add("-classpath");
      //      optionList.add(System.getProperty("java.class.path") + ";dist/InlineCompiler.jar");

      val compilationUnit = fileManager.getJavaFileObjectsFromFiles(util.Arrays.asList(javaFile))
      val task = compiler.getTask(
        null,
        fileManager,
        diagnostics,
        optionList,
        null,
        compilationUnit)

      val out = if (task.call()) {
        //        val classOutputDir = "./target/scala-2.12/test-classes/"
        val classOutputDir = javaFile.getParent + "/"
        val classFilename = classOutputDir + javaFile.getName.replace(".java", ".class")
        Some(new File(classFilename))

        //        val classLoader = new URLClassLoader(new Array[URL](new File("./").toURI().toURL()))
        //        val loadedClass = classLoader.loadClass("testcompile.HelloWorld")
        //        val obj = loadedClass.newInstance()
      } else {
        diagnostics.getDiagnostics().forEach(diagnostic => {
          println(diagnostic)
          //          System.out.format("Error on line %d in %s%n",
          //            diagnostic.getLineNumber(),
          //            diagnostic.getSource().toUri());
        })
        None
      }

      fileManager.close()

      out
    }
    catch {
      case e@(_: IOException | _: ClassNotFoundException | _: InstantiationException | _: IllegalAccessException) =>
        e.printStackTrace
        None
    }
  }

  case class CompileResults(jvm: JVM)

  def compileAndExecuteJavaFile(resource: String, onReturn: (StackFrame) => Unit = (sf) => {
    assert (sf.stack.isEmpty)
  }): CompileResults = {
    compileAndExecuteJavaFileX(resource, resource.stripSuffix(".java"), "main", onReturn)
  }


    def compileAndExecuteJavaFileX(resource: String, classToExecute: String, funcToExecute: String = "main", onReturn: (StackFrame) => Unit = (sf) => {
      assert (sf.stack.isEmpty)
    }): CompileResults = {
    //    val javaFilename = Thread.currentThread().getContextClassLoader().getResource(resource)
    val sampleDir = "./src/test/java/"
    val javaFilename = sampleDir + resource
    val javaFile = new File(javaFilename)
    assert(javaFile.exists())

    CompilingTestUtils.compileJavaFile(javaFile) match {
      case Some(classFile) =>
        val classLoader = new JVMClassLoader(Seq(sampleDir), JVMClassLoaderParams(verbose = true))
        val jvm = new JVM(classLoader)
        jvm.execute(classToExecute, funcToExecute, ExecuteParams(onReturn = Some(onReturn)))

        CompileResults(jvm)

      case _ =>
        assert(false, s"Failed to compile ${javaFile}")
        null
    }
  }
}
