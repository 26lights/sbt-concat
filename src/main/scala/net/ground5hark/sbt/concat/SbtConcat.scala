package net.ground5hark.sbt.concat

import sbt.Keys._
import sbt._

import com.typesafe.sbt.web.{PathMapping, SbtWeb}
import com.typesafe.sbt.web.pipeline.Pipeline
import com.typesafe.sbt.web.incremental._

import collection.mutable
import mutable.ListBuffer

import java.io.File
import java.io.FileOutputStream
import java.nio.ByteBuffer
import java.io.OutputStreamWriter

import scala.language.implicitConversions

sealed trait ConcatSource {
  def filter(file: File, relativePath: String): Boolean
}

case class ConcatSourceString(relativePath: String) extends ConcatSource {
  val normalizedRelativePath = 
    relativePath
      .replace('\\', File.separatorChar)
      .replace('/', File.separatorChar)

  def filter(file: File, relativePath: String): Boolean = {
    relativePath == normalizedRelativePath
  }
}

case class ConcatSourcePathFinder(pathFinder: PathFinder) extends ConcatSource {
  lazy val files = Set(pathFinder.get :_*)

  def filter(file: File, relativePath: String): Boolean = files.contains(file)
}

case class ConcatGroup(name: String, sources: Seq[ConcatSource], comments: Option[String => String] = Some(ConcatGroup.cLikeComments)) {
  def from(sources: ConcatSource*) = this.copy(sources = sources)
  def commentedBy(f: String => String): ConcatGroup = this.copy(comments = Some(f))
  def noCommented = this.copy(comments = None)
}
object ConcatGroup{
  val cLikeComments = (fileName: String) => s"\n/** $fileName **/\n"
}

object Import {
  val concat = TaskKey[Pipeline.Stage]("web-concat", "Concatenates groups of web assets")

  object Concat {
    val groups = SettingKey[Seq[ConcatGroup]]("web-concat-groups", "List of ConcatGroup items")
    val keepSources = SettingKey[Boolean]("web-concat-keep-sources", "Keep original source files in the pipeline (default: false)")
  }

  implicit def string2ConcatGroup(str: String): ConcatGroup = new ConcatGroup(str, Seq())
  implicit def string2ConcatSource(str: String): ConcatSource = new ConcatSourceString(str)
  implicit def pathFinder2ConcatSource(pathFinder: PathFinder): ConcatSource = new ConcatSourcePathFinder(pathFinder)
  implicit def pathFinder2SeqConcatSource(pathFinder: PathFinder): Seq[ConcatSource] = Seq(new ConcatSourcePathFinder(pathFinder))
}

object SbtConcat extends AutoPlugin {
  override def requires = SbtWeb

  override def trigger = AllRequirements

  val autoImport = Import

  import SbtWeb.autoImport._
  import WebKeys._
  import autoImport._
  import Concat._

  override def projectSettings = Seq(
    groups := Seq.empty[ConcatGroup],
    excludeFilter in (Assets, concat) := HiddenFileFilter,
    concat := concatFiles.value,
    keepSources := false
  )

  case class ConcatOperation(targetPath: File, group: ConcatGroup, sources: Seq[PathMapping]) {
    val targetFile = targetPath / group.name

    def writeConcat(log: Logger): OpResult = {

      IO.touch(targetFile)

      val outputStream = new FileOutputStream(targetFile)
      val writer = new OutputStreamWriter(outputStream, IO.utf8)

      sources.foreach { case (file, relativePath)=>
        group.comments.foreach{ comment =>
          writer.write(comment(relativePath))
          writer.flush()
        }
        IO.transfer(file, outputStream)
      }

      outputStream.close()
      log.info(s"${sources.length} files added to $targetFile")

      OpSuccess(sourceFiles.toSet, Set(targetFile))
    }

    def sourceFiles = sources.map{case (file, _) => file }
    def generated = targetFile -> group.name
  }

  private def concatFiles: Def.Initialize[Task[Pipeline.Stage]] = Def.task { mappings: Seq[PathMapping] =>
    var usedMappings = Set.empty[PathMapping]
    val targetPath = (webTarget in concat).value
    val log = streams.value.log

    val filteredMappings = mappings.filter{ 
      case(file, name) => !(excludeFilter in (Assets, concat)).value.accept(file) 
    }

    val operations = groups.value.map { group =>
      var sources = Seq[PathMapping]()
      group.sources.foreach { source =>
        filteredMappings.foreach { case mapping @ (file, relativePath) =>
          if(source.filter(file, relativePath)) {
            sources :+= mapping
            usedMappings += mapping
          }
        }
      }
      ConcatOperation(targetPath, group, sources)
    }

    syncIncremental(streams.value.cacheDirectory / "concat", operations) { toRun =>
      val result = toRun.map { operation =>
        operation -> operation.writeConcat(log)
      }.toMap
      result -> Unit
    }

    val generated = operations.map(_.generated)
    val result = if(keepSources.value) mappings else mappings.filterNot(usedMappings.contains)

    result ++ generated
  }
}
