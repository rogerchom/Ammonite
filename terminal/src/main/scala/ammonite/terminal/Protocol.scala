package ammonite.terminal

case class TermInfo(ts: TermState, width: Int)

sealed trait TermAction
case class Printing(ts: TermState, stdout: String) extends TermAction
case class TermState(inputs: LazyList[Int],
                     buffer: Vector[Char],
                     cursor: Int,
                     msg: fansi.Str = "",
                     popupState: Option[PopupState] = None) extends TermAction
object TermState{
  def unapply(ti: TermInfo): Option[(LazyList[Int], Vector[Char], Int, fansi.Str, Option[PopupState])] = {
    TermState.unapply(ti.ts)
  }
  def unapply(ti: TermAction): Option[(LazyList[Int], Vector[Char], Int, fansi.Str, Option[PopupState])] = ti match{
    case ts: TermState => TermState.unapply(ts)
    case _ => None
  }

}
case class ClearScreen(ts: TermState) extends TermAction
case object Exit extends TermAction
case class Result(s: String) extends TermAction
case class PopupState(
                       commandCursor: Int, // Cursor position of the user's command text
                       colWidth: Int // Char-width of suggestion columns
                     )

