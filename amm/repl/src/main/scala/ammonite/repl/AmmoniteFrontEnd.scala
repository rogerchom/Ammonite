package ammonite.repl

import java.io.{InputStream, OutputStream, OutputStreamWriter}

import ammonite.repl.api.FrontEnd
import ammonite.terminal.filters._
import GUILikeFilters.SelectionFilter
import ammonite.terminal._
import fastparse.Parsed
import ammonite.util.{Colors, Res}
import ammonite.interp.{Parsers, Preprocessor}
case class AmmoniteFrontEnd(extraFilters: Filter = Filter.empty) extends FrontEnd{

  def width = FrontEndUtils.width
  def height = FrontEndUtils.height

  def action(input: InputStream,
             reader: java.io.Reader,
             output: OutputStream,
             prompt: String,
             colors: Colors,
             compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
             history: IndexedSeq[String],
             addHistory: String => Unit) = {
    readLine(reader, output, prompt, colors, compilerComplete, history) match{
      case None => Res.Exit(())
      case Some(code) =>
        addHistory(code)
        fastparse.parse(code, Parsers.Splitter(_)) match{
          case Parsed.Success(value, idx) =>
            Res.Success((code, value.map(_._2)))
          case f @ Parsed.Failure(_, index, extra) =>
            Res.Failure(
              Preprocessor.formatFastparseError("(console)", code, f)
            )
        }
    }
  }

  val cutPasteFilter = ReadlineFilters.CutPasteFilter()

  def readLine(reader: java.io.Reader,
               output: OutputStream,
               prompt: String,
               colors: Colors,
               compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
               history: IndexedSeq[String]) = {

    val writer = new OutputStreamWriter(output)

    val autocompleteFilter: Filter = Filter.action(SpecialKeys.Tab){
      case TermState(rest, b, c, _, _) =>
        val (newCursor, completions, details) = TTY.withSttyOverride(TTY.restoreSigInt()) {
          compilerComplete(c, b.mkString)
        }

        val details2 = for (d <- details) yield {

          Highlighter.defaultHighlight(
            d.toVector,
            colors.comment(),
            colors.`type`(),
            colors.literal(),
            colors.keyword(),
            fansi.Attr.Reset
          ).mkString
        }

        lazy val common = FrontEndUtils.findPrefix(completions, 0)

        val blacklisted = Seq(
          "!=",
          "==",
          "asInstanceOf",
          "equals",
          "getClass",
          "hashCode",
          "isInstanceOf",
          "toString",
          "|>"
        )

        val completions2 = for(comp <- completions.filterNot(blacklisted.contains)) yield {

          val (left, right) = comp.splitAt(common.length)
          (colors.comment()(left) ++ right).render
        }

//        val foo = FrontEndUtils.printCompletions(completions2, details2)
//        println(s"length: ${foo.length}")
//        val foo2 = foo.map { s => s"""("$s", ${s.length})"""}
//        val stdout = foo2.mkString("[",", ","]") //FrontEndUtils.printCompletions(completions2, details2).mkstring

        val result = FrontEndUtils.printCompletions(completions2, details2)
        val stdout = result.completions.mkString
        val colWidth = result.colWidth

        if (details.nonEmpty || completions.isEmpty)
          Printing(TermState(rest, b, c), stdout)
        else{
          val newBuffer = b.take(newCursor) ++ common ++ b.drop(c) ++ stdout.toVector
//          println(s"autocompleteFilter -- b: $b, c: $c, common: $common, newBuffer: $newBuffer") // $newBuffer is usually the same as $b, $common is usually empty
//          Printing(TermState(rest, newBuffer, newCursor + common.length), stdout)
          TermState(rest, newBuffer, newCursor + common.length, popupState=Some(PopupState(newCursor, colWidth)))
        }

    }

    // Enter
    val multilineFilter = Filter.action(
      SpecialKeys.NewLine,
      ti => Parsers.split(ti.ts.buffer.mkString).isEmpty
    ){
      case TermState(rest, b, c, _, _) => BasicFilters.injectNewLine(b, c, rest)
    }

    val historyFilter = new HistoryFilter(
      () => history.reverse, colors.comment()
    )
    val selectionFilter = GUILikeFilters.SelectionFilter(indent = 2)

    val allFilters = Filter.merge(
//      Filter.action(Strings(Seq("?"))){
//        case TermState(rest, b, c, _) =>
//          println("am I alive?")
//          Printing(TermState(rest, b, c), "I LIVE!!!!")
//      },
      UndoFilter(),
      historyFilter,
      extraFilters,
      selectionFilter,
      GUILikeFilters.altFilter,
      GUILikeFilters.fnFilter,
      ReadlineFilters.navFilter,
      autocompleteFilter,
      cutPasteFilter,
      multilineFilter,
      BasicFilters.all
    )


    val res = Terminal.readLine(
      prompt,
      reader,
      writer,
      allFilters,
      displayTransform = { (buffer, cursor) =>


        val indices = Highlighter.defaultHighlightIndices(
          buffer,
          colors.comment(),
          colors.`type`(),
          colors.literal(),
          colors.keyword(),
          fansi.Attr.Reset
        )
        val highlighted = fansi.Str(Highlighter.flattenIndices(indices, buffer).mkString)
        val (newBuffer, offset) = SelectionFilter.mangleBuffer(
          selectionFilter, highlighted, cursor, colors.selected()
        )

        val newNewBuffer = HistoryFilter.mangleBuffer(
          historyFilter, newBuffer, cursor, fansi.Underlined.On
        )
        (newNewBuffer, offset)
      }
    )
    res
  }
}
