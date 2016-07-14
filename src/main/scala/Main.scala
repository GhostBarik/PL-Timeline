import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.model.{Document, Element}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

import scala.collection.parallel.{ForkJoinTaskSupport, TaskSupport}
import scala.collection.parallel.immutable.ParSeq

import scala.collection.immutable.Set
import scala.collection.immutable.Map

/**
  * Created by ghostbarik on 7/12/16.
  */
object Main {

  // type alias for URL
  type URL = String

  // url of the wikipedia main domain
  val wikiDomain = "https://en.wikipedia.org"

  var visitedUrls = Set[URL]()
  var graphOfLanguages = Map[URL, Set[URL]]()
//  var pageByUrl = Map[URL, WikiPage]()

  implicit val executionContext = new ForkJoinTaskSupport(
    new scala.concurrent.forkjoin.ForkJoinPool(8)
  )


  def main(args: Array[String]) {

//
    val firstPage = WikiPage(wikiDomain + "/wiki/Java_(programming_language)", external = false, "Java")

    scanPages(firstPage, fetchHtml(firstPage.url))

//    for ((k,v) <- graphOfLanguages) {
//      println( (k,v) )
//    }

    //println("visited: " + visitedUrls.size)
    println(toDotFormat(graphOfLanguages))
  }

  def toDotFormat(graph: Map[URL, Set[URL]]): String = {

    def escapeString(s: String) = s
      .replace("programming_language", "")
      .replace("()", "").replace(wikiDomain, "")
      .replace("/wiki/", "")
      .replace("_","")

    val escaped = graph.map{case (k, v) => (escapeString(k), v.map(escapeString(_))) }

    println(escaped)

    val res = escaped.flatMap{ case (k,v) => v.map(elem => k + " -> " + elem + ";")}.mkString("\n")
    "digraph G {" + res + "}"
  }

  def scanPages(startW: WikiPage, from: Document): Unit = {

    val nextPages: List[WikiPage] = parseHtml(from)

    val parsed = parsePages(startW, nextPages.filter{ page =>

        graphOfLanguages.get(startW.url) match {

          case Some(l: Set[URL]) =>
            graphOfLanguages += (startW.url -> (l + page.url))

          case None =>
            graphOfLanguages += (startW.url -> Set(page.url))
        }

        !(visitedUrls contains page.url)
      }.filter(!_.external))

      for ((d,w) <- parsed) {

        if (w.url == "https://en.wikipedia.org/wiki/R_(programming_language)") {
          val a = "test"
        }
        scanPages(w,d)
      }


  }




  case class WikiPage(url: URL, external: Boolean, language: String)

  def fetchHtml(url: URL): Document = {

    println("fetching url: " + url)

    JsoupBrowser().get(url)
  }

  def parsePages(from: WikiPage, pages: List[WikiPage]): List[(Document, WikiPage)] = {

    val results = pages.map{p =>


      this.synchronized {

        visitedUrls += p.url

        println("visiting: " + p.url)
      }



      (fetchHtml(p.url), p)

    }.seq


    results.toList
  }

  def inParallel[E](l: List[E])(implicit ts: TaskSupport): ParSeq[E] = {
    val pr = l.par
    pr.tasksupport = ts
    pr
  }

  // return -> List[influenced]
  def parseHtml(html: Document): List[WikiPage] = {

    val rightContentBlock = html >> elementList("#mw-content-text > table.infobox.vevent > tbody > tr")

    val influenced = rightContentBlock.zipWithIndex.find{ case (elem, _) =>
      (elem.innerHtml contains "Influenced") &&  !(elem.innerHtml contains "Influenced by")
    }

    influenced.map{ case (_,index) =>

      val (_,index) = influenced.get

      val languages = rightContentBlock(index + 1) >> elementList("a")

      for {
        elem <- languages
        if !(elem.attr("href") contains "cite_note")
        if !(elem.attr("href") contains "Citation_needed")
        if !elem.attrs.get("class").contains("new")

      } yield WikiPage(
        wikiDomain + elem.attr("href")
        .replace("(programming_language)", "programming_language")
        .replace("programming_language", "(programming_language)"),
        !elem.attr("href").contains("/wiki"),
        elem.innerHtml
      )

    }.getOrElse(List.empty)
  }
}
