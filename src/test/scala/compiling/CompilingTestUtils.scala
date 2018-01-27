package compiling

import java.io.{File, IOException, StringWriter}
import java.util
import javax.tools.{DiagnosticCollector, JavaFileObject, ToolProvider}

import jvm.JVMByteCode.JVMOpCodeWithArgs
import jvm.JVMClassFileReader.ReadParams
import jvm._
import parsing._
import pprint.PPrinter

import scala.scalajs.niocharset.StandardCharsets

object CompilingTestUtils {
  def createParser() = new CParser

  //  def createByteCode() = new JVMByteCodeGenerator

  //  def createInterim() = new JVMClassFileGenerator

  def testCSnippetAgainstJVM(cCode: String, asmCode: String): Unit = {
    testCAgainstJVM(cCode, asmCode, isSnippet = true)
  }

  def testCTopAgainstJVM(cCode: String, asmCode: String): Unit = {
    testCAgainstJVM(cCode, asmCode, isSnippet = false)
  }

  private def testCAgainstJVM(cCode: String, asmCode: String, isSnippet: Boolean): Unit = {
    val p = createParser()
    //    val interimParser = createInterim()
    //    val writer = new JVMClassFileWriter()
    val split = asmCode.trim().replace("\r\n", "\n").split('\n').filterNot(_.startsWith("method "))

    val parsed = if (isSnippet) p.parseSnippet(cCode) else p.parse(cCode)
    parsed match {
      case CParseSuccess(x) =>
        PPrinter.Color.log(x)
        val cf = new JVMClassFileBuilderForWriting(50, 0, Some("test"), "Test")
        val asm = new JVMByteCodeGenerator(cf)
        x match {
          case v: Seq[BlockItem]  =>
            asm.generateSeqBlockItem(v)
          case v: TranslationUnit =>
            asm.generateTranslationUnit(v)
        }

        val sb = new StringWriter()
        cf.write(sb, StandardCharsets.UTF_8)
        val resolved = sb.toString.trim().split('\n').map(_.trim).filterNot(_.startsWith("method:"))
        //        val resolved = resolvedRaw.takeRight(resolvedRaw.length - 1)


        if (resolved.length != split.length) {
          ////          println(s"length mismatch\n${resolved.mkString("\n")} != \n$asmCode")
          PPrinter.Color.log(x)
          //          PPrinter.Color.log(generated)
          PPrinter.Color.log(resolved)
          assert(false, s"length mismatch\n${resolved.mkString("\n")} \n!= \n\n$asmCode")
        }
        else {
          implicit val genParams: JVMByteCode.GenParams = JVMByteCode.GenParams()
          for (idx <- resolved.indices) {
            val a = resolved(idx)
            val s = split(idx)
            //            val genned = a.gen(genParams)

            assert(a == s, s"${a} != $s")
          }
        }

      case CParseFail(v) =>
        assert(false, s"Parse fail on $cCode:\n$v")
    }

  }

  def compileCToJVMOpcode(cCode: String, isSnippet: Boolean): Seq[JVMOpCodeWithArgs] = {
    val p = createParser()

    val parsed = if (isSnippet) p.parseSnippet(cCode) else p.parse(cCode)
    parsed match {
      case CParseSuccess(x) =>
        PPrinter.Color.log(x)
        val cf = new JVMClassFileBuilderForWriting(50, 0, Some("test"), "Test")
        val asm = new JVMByteCodeGenerator(cf)
        x match {
          case v: Seq[BlockItem]  =>
            asm.generateSeqBlockItem(v)
          case v: TranslationUnit =>
            asm.generateTranslationUnit(v)
        }
        if (isSnippet) cf.getMethod("fake").get.getCode().codeOrig
        else cf.getMainMethod().get.getCode().codeOrig

      case CParseFail(v) =>
        assert(false, s"Parse fail on $cCode:\n$v")
        null
    }
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
        val classOutputDir = "./target/scala-2.12/test-classes/"
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
      case e @ (_: IOException | _: ClassNotFoundException | _: InstantiationException | _: IllegalAccessException) =>
        e.printStackTrace
        None
    }
  }

  case class CompileResults(cf: JVMClassFileBuilderForReading, jvm: JVM)

  def compileAndExecuteJavaFile(resource: String, funcToExecute: String = "main"): CompileResults = {
//    val javaFilename = Thread.currentThread().getContextClassLoader().getResource(resource)
    val javaFilename = "./src/test/java/" + resource
    val javaFile = new File(javaFilename)

    CompilingTestUtils.compileJavaFile(javaFile) match {
      case Some(classFile) =>
        JVMClassFileReader.read(classFile, ReadParams()) match {
          case Some(cf) =>
            cf.getMethod(funcToExecute) match {
              case Some(main) =>
                val jvm = new JVM(cf)
                val code = main.getCode().codeOrig
                jvm.execute(code, ExecuteParams(stopBeforeFinalReturn = true))

                CompileResults(cf, jvm)

              case _ =>
                assert(false, "Failed to find main in compiled file")
                null
            }

          case _ =>
            assert(false, s"Failed to read input ${classFile}")
            null
        }
      case _ =>
        assert(false, s"Failed to compile ${javaFile}")
        null
    }

  }
}
