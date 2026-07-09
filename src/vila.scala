package vila

// ═══════════════════════════════════════════════════════════════════
//  Vila — a minimal vim clone in Scala 3 using TamboUI
//  Pure functional style with immutable data
// ═══════════════════════════════════════════════════════════════════

// ─── Mode ─────────────────────────────────────────────────────────

enum Mode:
  case Normal, Insert, VisualChar, VisualLine, VisualBlock,
    Command, Search, OperatorPending

// ─── Small data types ─────────────────────────────────────────────

final case class PendingOp(op: String, count: Int = 1)
final case class DotRepeat(op: String, count: Int = 1)

final case class CursorPos(row: Int, col: Int)
    extends Ordered[CursorPos]:
  def compare(that: CursorPos): Int = {
    val rc = row.compare(that.row)
    if rc != 0 then rc else col.compare(that.col)
  }

object CursorPos:
  val zero: CursorPos = CursorPos(0, 0)

final case class TextLine(text: String):
  def length: Int = text.length
  def insert(pos: Int, c: Char): TextLine =
    if pos < 0 then TextLine(c.toString + text)
    else if pos >= text.length then TextLine(text + c.toString)
    else TextLine(text.substring(0, pos) + c + text.substring(pos))
  def delete(pos: Int): TextLine =
    if pos < 0 || pos >= text.length then this
    else TextLine(text.substring(0, pos) + text.substring(pos + 1))

object TextLine:
  val empty: TextLine = TextLine("")

// ═══════════════════════════════════════════════════════════════════
//  TextBuffer — immutable text model
// ═══════════════════════════════════════════════════════════════════

final case class TextBuffer(
    lines: Vector[TextLine] = Vector(TextLine.empty),
    filename: Option[String] = None,
    modified: Boolean = false,
    cursor: CursorPos = CursorPos(0, 0),
    undoStack: Vector[TextBuffer] = Vector.empty
):
  def lineCount: Int = lines.length
  def currentLine: TextLine = lines(cursor.row)
  def line(row: Int): TextLine = lines(row)
  def col: Int = cursor.col
  def row: Int = cursor.row

  def clampCursor: TextBuffer =
    val maxRow = (lineCount - 1).max(0)
    val r = cursor.row.max(0).min(maxRow)
    val c = cursor.col.max(0).min(lines(r).length)
    copy(cursor = CursorPos(r, c))

  def moveUp(n: Int = 1): TextBuffer =
    copy(cursor = CursorPos((row - n).max(0), col)).clampCursor
  def moveDown(n: Int = 1): TextBuffer =
    copy(cursor =
      CursorPos((row + n).min(lineCount - 1).max(0), col)
    ).clampCursor
  def moveLeft(n: Int = 1): TextBuffer =
    if col == 0 && row > 0 then
      copy(cursor = CursorPos(row - 1, lines(row - 1).length))
    else copy(cursor = CursorPos(row, (col - n).max(0)))
  def moveRight(n: Int = 1): TextBuffer =
    if col >= currentLine.length && row < lineCount - 1 then
      copy(cursor = CursorPos(row + 1, 0))
    else
      copy(cursor = CursorPos(row, (col + n).min(currentLine.length)))
  def moveHome: TextBuffer = copy(cursor = CursorPos(row, 0))
  def moveEnd: TextBuffer =
    copy(cursor = CursorPos(row, currentLine.length))
  def moveFirstNonBlank: TextBuffer =
    val i = currentLine.text.indexWhere(!_.isWhitespace)
    copy(cursor =
      CursorPos(row, if i < 0 then currentLine.length else i)
    )
  def moveGg: TextBuffer = copy(cursor = CursorPos(0, 0))
  def moveG: TextBuffer = copy(cursor = CursorPos(lineCount - 1, 0))

  // ── Editing ──

  def insertChar(c: Char): TextBuffer =
    val nl = lines.updated(row, currentLine.insert(col, c))
    copy(
      lines = nl,
      modified = true,
      cursor = CursorPos(row, col + 1),
      undoStack = undoStack :+ this
    )

  def insertString(s: String): TextBuffer =
    s.foldLeft(this)((b, c) => b.insertChar(c))

  def backspace: TextBuffer =
    if col > 0 then
      copy(
        lines = lines.updated(row, currentLine.delete(col - 1)),
        modified = true,
        cursor = CursorPos(row, col - 1),
        undoStack = undoStack :+ this
      )
    else if row > 0 then
      val prevLen = lines(row - 1).length
      copy(
        lines = lines.patch(
          row - 1,
          Seq(TextLine(lines(row - 1).text + currentLine.text)),
          2
        ),
        modified = true,
        cursor = CursorPos(row - 1, prevLen),
        undoStack = undoStack :+ this
      )
    else this

  def splitLine: TextBuffer =
    val before = currentLine.text.substring(0, col)
    val after = currentLine.text.substring(col)
    val indent = before.takeWhile(_.isWhitespace)
    copy(
      lines =
        lines.patch(row, Seq(TextLine(before), TextLine(after)), 1),
      modified = true,
      cursor = CursorPos(row + 1, indent.length),
      undoStack = undoStack :+ this
    )

  def deleteLine: TextBuffer =
    if lines.length == 1 then
      copy(
        lines = Vector(TextLine.empty),
        modified = true,
        cursor = CursorPos(0, 0),
        undoStack = undoStack :+ this
      )
    else
      val nr = row.min(lines.length - 2)
      copy(
        lines = lines.patch(row, Seq.empty, 1),
        modified = true,
        cursor = CursorPos(nr, 0),
        undoStack = undoStack :+ this
      )

  def yankLine: String = currentLine.text + "\n"

  def insertLineBelow: TextBuffer =
    copy(
      lines = lines.patch(row + 1, Seq(TextLine.empty), 0),
      modified = true,
      cursor = CursorPos(row + 1, 0),
      undoStack = undoStack :+ this
    )

  def insertLineAbove: TextBuffer =
    copy(
      lines = lines.patch(row, Seq(TextLine.empty), 0),
      modified = true,
      cursor = CursorPos(row, 0),
      undoStack = undoStack :+ this
    )

  def indentLine(r: Int, amount: Int = 2): TextBuffer =
    copy(
      lines = lines.updated(r, TextLine(" " * amount + lines(r).text)),
      modified = true,
      undoStack = undoStack :+ this
    )

  def outdentLine(r: Int, amount: Int = 2): TextBuffer =
    val toRemove = lines(r).text.take(amount).count(_ == ' ')
    copy(
      lines =
        lines.updated(r, TextLine(lines(r).text.substring(toRemove))),
      modified = true,
      undoStack = undoStack :+ this
    )

  def toggleCaseAt: TextBuffer =
    if col < currentLine.length then
      val c = currentLine.text.charAt(col)
      val t = if c.isLower then c.toUpper else c.toLower
      val nl = TextLine(
        currentLine.text.substring(0, col) + t + currentLine.text
          .substring(
            col + 1
          )
      )
      copy(
        lines = lines.updated(row, nl),
        modified = true,
        cursor = CursorPos(row, col + 1),
        undoStack = undoStack :+ this
      )
    else this

  def undo: TextBuffer =
    if undoStack.isEmpty then this
    else undoStack.last.copy(undoStack = undoStack.last.undoStack)

  // ── Range operations ──

  def deleteRange(
      from: CursorPos,
      to: CursorPos
  ): (TextBuffer, String) =
    val (r1, c1) =
      if from < to then (from.row, from.col) else (to.row, to.col)
    val (r2, c2) =
      if from < to then (to.row, to.col) else (from.row, from.col)
    if r1 == r2 then
      val del = lines(r1).text.substring(c1, c2)
      val nl = TextLine(
        lines(r1).text.substring(0, c1) + lines(r1).text.substring(c2)
      )
      (
        copy(
          lines = lines.updated(r1, nl),
          modified = true,
          cursor = CursorPos(r1, c1),
          undoStack = undoStack :+ this
        ),
        del
      )
    else
      val sb = StringBuilder(lines(r1).text.substring(c1)).append('\n')
      for i <- (r1 + 1) until r2 do
        sb.append(lines(i).text).append('\n')
      sb.append(lines(r2).text.substring(0, c2))
      val merged = TextLine(
        lines(r1).text.substring(0, c1) + lines(r2).text.substring(c2)
      )
      (
        copy(
          lines = lines.patch(r1, Seq(merged), r2 - r1 + 1),
          modified = true,
          cursor = CursorPos(r1, c1),
          undoStack = undoStack :+ this
        ),
        sb.toString
      )

  def yankRange(from: CursorPos, to: CursorPos): String =
    val (r1, c1) =
      if from < to then (from.row, from.col) else (to.row, to.col)
    val (r2, c2) =
      if from < to then (to.row, to.col) else (from.row, from.col)
    val sb = StringBuilder()
    for i <- r1 to r2 do
      val ln = lines(i).text
      if i == r1 && i == r2 then sb.append(ln.substring(c1, c2))
      else if i == r1 then sb.append(ln.substring(c1)).append('\n')
      else if i == r2 then sb.append(ln.substring(0, c2))
      else sb.append(ln).append('\n')
    sb.toString

  // ── Motions ──

  def wordForward: TextBuffer =
    var r = row; var c = col
    val l = lines(r).text
    while c < l.length && l.charAt(c).isLetterOrDigit do c += 1
    while c < l.length && !l.charAt(c).isLetterOrDigit && l.charAt(
        c
      ) != ' '
    do c += 1
    while c < l.length && l.charAt(c) == ' ' do c += 1
    if c >= l.length && r < lineCount - 1 then
      r += 1; c = 0
      while c < lines(r).length && lines(r).text.charAt(c) == ' ' do
        c += 1
    copy(cursor = CursorPos(r, c.min(lines(r).length)))

  def wordEnd: TextBuffer =
    var r = row; var c = col.min(currentLine.length - 1).max(0)
    if c < currentLine.length && currentLine.text
        .charAt(c)
        .isLetterOrDigit
    then
      while c + 1 < lines(r).length && lines(r).text
          .charAt(c + 1)
          .isLetterOrDigit
      do c += 1
    else if c < currentLine.length && !currentLine.text
        .charAt(c)
        .isWhitespace
    then
      while c + 1 < lines(r).length && !lines(r).text
          .charAt(c + 1)
          .isLetterOrDigit &&
        !lines(r).text.charAt(c + 1).isWhitespace
      do c += 1
    copy(cursor = CursorPos(r, (c + 1).min(lines(r).length)))

  def wordBack: TextBuffer =
    var r = row; var c = col.min(currentLine.length).max(0)
    if c >= currentLine.length then c = currentLine.length - 1
    if c > 0 then c -= 1
    while c > 0 && !currentLine.text.charAt(c).isLetterOrDigit &&
      !currentLine.text.charAt(c - 1).isLetterOrDigit
    do c -= 1
    while c > 0 && currentLine.text.charAt(c).isLetterOrDigit &&
      currentLine.text.charAt(c - 1).isLetterOrDigit
    do c -= 1
    while c > 0 && currentLine.text.charAt(c) == ' ' &&
      currentLine.text.charAt(c - 1) == ' '
    do c -= 1
    if c == 0 && !currentLine.text.charAt(0).isLetterOrDigit &&
      !currentLine.text.charAt(0).isWhitespace
    then copy(cursor = CursorPos(r, 0))
    else if r > 0 && c == 0 && !currentLine.text
        .charAt(0)
        .isLetterOrDigit
    then copy(cursor = CursorPos(r - 1, lines(r - 1).length))
    else copy(cursor = CursorPos(r, c))

  def bigWordForward: TextBuffer =
    var r = row; var c = col
    while c < lines(r).length && !lines(r).text.charAt(c).isWhitespace
    do c += 1
    while c < lines(r).length && lines(r).text.charAt(c).isWhitespace do
      c += 1
    if c >= lines(r).length && r < lineCount - 1 then
      r += 1; c = 0
      while c < lines(r).length && lines(r).text.charAt(c).isWhitespace
      do c += 1
    copy(cursor = CursorPos(r, c.min(lines(r).length)))

  def bigWordEnd: TextBuffer =
    var r = row; var c = col.min(lines(row).length - 1).max(0)
    if c < lines(r).length && !lines(r).text.charAt(c).isWhitespace then
      while c + 1 < lines(r).length && !lines(r).text
          .charAt(c + 1)
          .isWhitespace
      do c += 1
    copy(cursor = CursorPos(r, (c + 1).min(lines(r).length)))

  def bigWordBack: TextBuffer =
    var r = row; var c = col.min(currentLine.length).max(0)
    if c >= currentLine.length then c = currentLine.length - 1
    if c > 0 then c -= 1
    while c > 0 && currentLine.text.charAt(c).isWhitespace &&
      currentLine.text.charAt(c - 1).isWhitespace
    do c -= 1
    while c > 0 && !currentLine.text.charAt(c).isWhitespace &&
      !currentLine.text.charAt(c - 1).isWhitespace
    do c -= 1
    if c > 0 && currentLine.text.charAt(c).isWhitespace then c += 1
    if c == 0 && currentLine.text.charAt(0).isWhitespace && r > 0 then
      copy(cursor = CursorPos(r - 1, lines(r - 1).length))
    else copy(cursor = CursorPos(r, c))

  def paragraphForward: TextBuffer =
    var r = (row + 1).min(lineCount - 1)
    while r < lineCount && lines(r).text.nonEmpty && lines(
        r
      ).text.trim.nonEmpty
    do r += 1
    while r < lineCount && lines(r).text.trim.isEmpty do r += 1
    copy(cursor = CursorPos(r.min(lineCount - 1).max(0), 0)).clampCursor

  def paragraphBack: TextBuffer =
    var r = (row - 1).max(0)
    while r > 0 && lines(r).text.trim.isEmpty do r -= 1
    while r > 0 && lines(r).text.nonEmpty && lines(r).text.trim.nonEmpty
    do r -= 1
    if r > 0 || lines(0).text.trim.isEmpty then
      copy(cursor = CursorPos(r.max(0), 0)).clampCursor
    else copy(cursor = CursorPos(0, 0))

  // ── Search ──

  def findForward(
      pattern: String,
      startRow: Int = -1,
      startCol: Int = -1
  ): Option[CursorPos] =
    scala.util.boundary[Option[CursorPos]] {
      val sr = if startRow < 0 then row else startRow
      val sc = if startCol < 0 then col else startCol
      for r <- sr until lineCount do
        val idx =
          lines(r).text.indexOf(pattern, if r == sr then sc else 0)
        if idx >= 0 then
          scala.util.boundary.break(Some(CursorPos(r, idx)))
      for r <- 0 until sr do
        val idx = lines(r).text.indexOf(pattern, 0)
        if idx >= 0 then
          scala.util.boundary.break(Some(CursorPos(r, idx)))
      None
    }

  def findBackward(
      pattern: String,
      startRow: Int = -1,
      startCol: Int = -1
  ): Option[CursorPos] =
    scala.util.boundary[Option[CursorPos]] {
      val sr = if startRow < 0 then row else startRow
      val sc = if startCol < 0 then col else startCol
      for r <- (0 to sr).reverse do
        val maxC = if r == sr then sc else lines(r).length
        val idx =
          lines(r).text.lastIndexOf(pattern, maxC - pattern.length)
        if idx >= 0 then
          scala.util.boundary.break(Some(CursorPos(r, idx)))
      for r <- (lineCount - 1) until sr by -1 do
        val idx = lines(r).text.lastIndexOf(pattern)
        if idx >= 0 then
          scala.util.boundary.break(Some(CursorPos(r, idx)))
      None
    }

  def findCharForward(ch: Char, startCol: Int = -1): Option[Int] =
    val sc = if startCol < 0 then col + 1 else startCol
    val idx = currentLine.text.indexOf(ch, sc)
    if idx >= 0 then Some(idx) else None

  def findCharBackward(ch: Char, startCol: Int = -1): Option[Int] =
    val sc = if startCol < 0 then col - 1 else startCol
    val idx = currentLine.text.lastIndexOf(ch, sc)
    if idx >= 0 then Some(idx) else None

  def findMatchingBracket: Option[CursorPos] =
    if col >= currentLine.length then None
    else
      val c = currentLine.text.charAt(col)
      val matching = c match
        case '(' => Some(')');
        case ')' => Some('(')
        case '[' => Some(']');
        case ']' => Some('[')
        case '{' => Some('}');
        case '}' => Some('{')
        case _   => None
      matching.flatMap { target =>
        scala.util.boundary[Option[CursorPos]] {
          val dir = if "([{".indexOf(c) >= 0 then 1 else -1
          var depth = 1; var r = row; var cc = col + dir
          while r >= 0 && r < lineCount do
            val line = lines(r).text
            while (if dir > 0 then cc < line.length else cc >= 0) do
              val ch = line.charAt(cc)
              if ch == c then depth += 1
              else if ch == target then {
                depth -= 1;
                if depth == 0 then
                  scala.util.boundary.break(Some(CursorPos(r, cc)))
              }
              cc += dir
            cc = if dir > 0 then 0
            else lines((r + dir).max(0).min(lineCount - 1)).length - 1
            r += dir
          None
        }
      }

  def wordUnderCursor: String =
    val l = currentLine.text; val c = col
    if c < l.length && l.charAt(c).isLetterOrDigit then
      var s = c;
      while s > 0 && l.charAt(s - 1).isLetterOrDigit do s -= 1
      var e = c;
      while e < l.length && l.charAt(e).isLetterOrDigit do e += 1
      l.substring(s, e)
    else ""

// ═══════════════════════════════════════════════════════════════════
//  EditorState — immutable snapshot of the entire editor
// ═══════════════════════════════════════════════════════════════════

final case class EditorState(
    buffer: TextBuffer = TextBuffer(),
    mode: Mode = Mode.Normal,
    message: String = "",
    commandBuffer: String = "",
    searchPattern: String = "",
    searchForward: Boolean = true,
    searchHighlight: Boolean = true,
    hlsearch: Boolean = true,
    smartcase: Boolean = true,
    yankRegister: String = "",
    pendingCount: Int = 1,
    pendingOp: Option[PendingOp] = None,
    pendingTextObject: Boolean = false,
    waitingForFChar: Option[Char] = None,
    waitingForFwd: Boolean = true,
    lastFCommand: Option[(String, Char)] = None,
    lastSearchPattern: String = "",
    lastSearchWasForward: Boolean = true,
    visualStart: Option[CursorPos] = None,
    dotRepeat: Option[DotRepeat] = None,
    insertAccum: String = "",
    scrollTop: Int = 0,
    termWidth: Int = 80,
    termHeight: Int = 24,
    keyMappings: Map[String, String] = Map.empty,
    lastKey: Int = 0
):
  def visibleLines: (Int, Int) =
    (scrollTop, (scrollTop + termHeight - 2).min(buffer.lineCount))

  def ensureCursorVisible: EditorState =
    val (s, e) = visibleLines
    if buffer.row < s then copy(scrollTop = buffer.row)
    else if buffer.row >= e then
      copy(scrollTop = buffer.row - termHeight + 3)
    else this

  def centerCursor: EditorState =
    copy(scrollTop = (buffer.row - termHeight / 2 + 1).max(0))
  def pageUp: EditorState =
    copy(scrollTop = (scrollTop - (termHeight / 2).max(1)).max(0))
  def pageDown: EditorState = copy(scrollTop =
    (scrollTop + (termHeight / 2).max(1))
      .min((buffer.lineCount - termHeight + 2).max(0))
  )

// ═══════════════════════════════════════════════════════════════════
//  InputHandler — pure state transitions
// ═══════════════════════════════════════════════════════════════════

object InputHandler:
  import Mode.*
  import dev.tamboui.tui.event.{KeyEvent, KeyCode}

  type ER = (EditorState, Boolean)

  def handle(ke: KeyEvent, s: EditorState): ER = s.mode match
    case Normal                                => onNormal(ke, s)
    case Insert                                => onInsert(ke, s)
    case Command                               => onCommand(ke, s)
    case Search                                => onSearch(ke, s)
    case VisualChar | VisualLine | VisualBlock => onVisual(ke, s)
    case OperatorPending                       => onOpPending(ke, s)

  // ── Normal ────────────────────────────────────────────────────────

  private def onNormal(ke: KeyEvent, s: EditorState): ER =
    if ke.code == KeyCode.ESCAPE then
      (s.copy(pendingOp = None, pendingCount = 1, lastKey = 0), true)
    else if s.pendingOp.isDefined then onMotionWithOp(ke, s)
    else if s.pendingTextObject then onTextObject(ke, s)
    else if s.waitingForFChar.isDefined then onFChar(ke, s)
    else if ke.code == KeyCode.CHAR then
      onNormalChar(ke.character.toChar, s)
    else onNormalSpecial(ke, s)

  private def onNormalChar(c: Char, s: EditorState): ER =
    val buf = s.buffer

    // two-key sequences
    if s.lastKey == 'g' then
      val s2 = s.copy(lastKey = 0)
      c match
        case 'g' => (s2.copy(buffer = buf.moveGg), true)
        case 'e' => (s2.copy(buffer = buf.wordBack.wordEnd), true)
        case 'E' => (s2.copy(buffer = buf.bigWordBack.bigWordEnd), true)
        case 'R' => (s2.copy(message = "Reloaded"), true)
        case _   => onNormalChar(c, s2)
    else if s.lastKey == 'z' then
      val s2 = s.copy(lastKey = 0)
      c match
        case 'z' => (s2.centerCursor, true)
        case 't' => (s2.copy(scrollTop = buf.row), true)
        case 'b' =>
          (
            s2.copy(scrollTop = (buf.row - s2.termHeight + 3).max(0)),
            true
          )
        case _ => onNormalChar(c, s2)

    // counts
    if c.isDigit && (s.pendingCount > 0 || c != '0') then
      val n = c - '0'
      return (
        s.copy(pendingCount = if s.pendingCount > 1 then
          s.pendingCount * 10 + n
        else n),
        true
      )

    c match
      // Insert
      case 'i' => enterInsert(s)
      case 'a' =>
        enterInsert(
          s.copy(buffer = if buf.col < buf.currentLine.length then
            buf.moveRight()
          else buf)
        )
      case 'I' => enterInsert(s.copy(buffer = buf.moveFirstNonBlank))
      case 'A' => enterInsert(s.copy(buffer = buf.moveEnd))
      case 'o' =>
        enterInsert(
          s.copy(buffer = buf.insertLineBelow, insertAccum = "")
        )
      case 'O' =>
        enterInsert(
          s.copy(buffer = buf.insertLineAbove, insertAccum = "")
        )

      // Motions
      case 'h' =>
        (
          s.copy(
            buffer = buf.moveLeft(s.pendingCount),
            pendingCount = 1
          ),
          true
        )
      case 'j' =>
        (
          s.copy(
            buffer = buf.moveDown(s.pendingCount),
            pendingCount = 1
          ),
          true
        )
      case 'k' =>
        (
          s.copy(buffer = buf.moveUp(s.pendingCount), pendingCount = 1),
          true
        )
      case 'l' =>
        (
          s.copy(
            buffer = buf.moveRight(s.pendingCount),
            pendingCount = 1
          ),
          true
        )
      case 'w' =>
        (s.copy(buffer = buf.wordForward, pendingCount = 1), true)
      case 'b' =>
        (s.copy(buffer = buf.wordBack, pendingCount = 1), true)
      case 'e' => (s.copy(buffer = buf.wordEnd, pendingCount = 1), true)
      case 'W' =>
        (s.copy(buffer = buf.bigWordForward, pendingCount = 1), true)
      case 'B' =>
        (s.copy(buffer = buf.bigWordBack, pendingCount = 1), true)
      case 'E' =>
        (s.copy(buffer = buf.bigWordEnd, pendingCount = 1), true)
      case 'G' => (s.copy(buffer = buf.moveG, pendingCount = 1), true)
      case '0' =>
        (s.copy(buffer = buf.moveHome, pendingCount = 1), true)
      case '$' => (s.copy(buffer = buf.moveEnd, pendingCount = 1), true)
      case '^' =>
        (s.copy(buffer = buf.moveFirstNonBlank, pendingCount = 1), true)
      case '{' =>
        (s.copy(buffer = buf.paragraphBack, pendingCount = 1), true)
      case '}' =>
        (s.copy(buffer = buf.paragraphForward, pendingCount = 1), true)
      case '%' =>
        buf.findMatchingBracket match
          case Some(p) => (s.copy(buffer = buf.copy(cursor = p)), true)
          case None    => (s, true)

      // Operators
      case 'x' =>
        val e =
          CursorPos(buf.row, (buf.col + 1).min(buf.currentLine.length))
        val (nb, d) = buf.deleteRange(buf.cursor, e)
        (
          s.copy(
            buffer = nb,
            yankRegister = d,
            dotRepeat = Some(DotRepeat("x")),
            pendingCount = 1
          ),
          true
        )
      case 'D' =>
        val e = CursorPos(buf.row, buf.currentLine.length)
        val (nb, d) = buf.deleteRange(buf.cursor, e)
        (
          s.copy(
            buffer = nb,
            yankRegister = d,
            dotRepeat = Some(DotRepeat("D")),
            pendingCount = 1
          ),
          true
        )
      case 'C' =>
        val e = CursorPos(buf.row, buf.currentLine.length)
        val (nb, d) = buf.deleteRange(buf.cursor, e)
        enterInsert(
          s.copy(buffer = nb, yankRegister = d, pendingCount = 1)
        )
      case 'Y' =>
        (
          s.copy(
            yankRegister = buf.yankLine,
            message = "Yanked",
            pendingCount = 1
          ),
          true
        )
      case 'd' => (s.copy(lastKey = 'd', pendingCount = 1), true)
      case 'c' =>
        (
          s.copy(
            pendingOp = Some(PendingOp("c")),
            mode = OperatorPending
          ),
          true
        )
      case 'y' =>
        (
          s.copy(
            pendingOp = Some(PendingOp("y")),
            mode = OperatorPending
          ),
          true
        )
      case '>' =>
        (
          s.copy(
            pendingOp = Some(PendingOp(">")),
            mode = OperatorPending
          ),
          true
        )
      case '<' =>
        (
          s.copy(
            pendingOp = Some(PendingOp("<")),
            mode = OperatorPending
          ),
          true
        )
      case '~' =>
        (s.copy(buffer = buf.toggleCaseAt, pendingCount = 1), true)

      // Linewise from lastKey
      case _ if s.lastKey == 'd' && c == 'd' =>
        val del = buf.yankLine
        (
          s.copy(
            buffer = buf.deleteLine,
            yankRegister = del,
            dotRepeat = Some(DotRepeat("dd")),
            lastKey = 0,
            pendingCount = 1
          ),
          true
        )
      case _ if s.lastKey == 'c' && c == 'c' =>
        val del = buf.yankLine
        enterInsert(
          s.copy(
            buffer = buf.deleteLine,
            yankRegister = del,
            dotRepeat = Some(DotRepeat("cc")),
            lastKey = 0,
            pendingCount = 1
          )
        )
      case _ if s.lastKey == 'y' && c == 'y' =>
        (
          s.copy(
            yankRegister = buf.yankLine,
            dotRepeat = Some(DotRepeat("yy")),
            lastKey = 0,
            pendingCount = 1
          ),
          true
        )

      // Visual
      case 'v' =>
        (
          s.copy(
            mode = VisualChar,
            visualStart = Some(buf.cursor),
            message = "-- VISUAL --",
            pendingCount = 1
          ),
          true
        )
      case 'V' =>
        (
          s.copy(
            mode = VisualLine,
            visualStart = Some(buf.cursor),
            message = "-- VISUAL LINE --",
            pendingCount = 1
          ),
          true
        )

      // Command / Search
      case ':' =>
        (
          s.copy(
            mode = Command,
            commandBuffer = "",
            message = ":",
            pendingCount = 1
          ),
          true
        )
      case '/' =>
        (
          s.copy(
            mode = Search,
            searchPattern = "",
            searchForward = true,
            message = "/",
            pendingCount = 1
          ),
          true
        )
      case '?' =>
        (
          s.copy(
            mode = Search,
            searchPattern = "",
            searchForward = false,
            message = "?",
            pendingCount = 1
          ),
          true
        )

      // Search
      case 'n' => doSearch(s, s.searchPattern, s.searchForward)
      case 'N' => doSearch(s, s.searchPattern, !s.searchForward)
      case '*' =>
        val w = buf.wordUnderCursor
        if w.nonEmpty then
          doSearch(
            s.copy(searchPattern = w, searchForward = true),
            w,
            true
          )
        else (s, true)
      case '#' =>
        val w = buf.wordUnderCursor
        if w.nonEmpty then
          doSearch(
            s.copy(searchPattern = w, searchForward = false),
            w,
            false
          )
        else (s, true)

      // Paste
      case 'p' => (execPasteAfter(s), true)
      case 'P' => (execPasteBefore(s), true)

      // Undo / Dot
      case 'u' => (s.copy(buffer = buf.undo, pendingCount = 1), true)
      case '.' => (execDotRepeat(s), true)

      // Prefix
      case 'g' => (s.copy(lastKey = 'g', pendingCount = 1), true)
      case 'z' => (s.copy(lastKey = 'z', pendingCount = 1), true)

      // f/F/t/T
      case 'f' =>
        (
          s.copy(waitingForFChar = Some('f'), waitingForFwd = true),
          true
        )
      case 'F' =>
        (
          s.copy(waitingForFChar = Some('F'), waitingForFwd = false),
          true
        )
      case 't' =>
        (
          s.copy(waitingForFChar = Some('t'), waitingForFwd = true),
          true
        )
      case 'T' =>
        (
          s.copy(waitingForFChar = Some('T'), waitingForFwd = false),
          true
        )
      case ';' => repeatLastF(s, forward = true)
      case ',' => repeatLastF(s, forward = false)

      // Key mappings / unknown
      case _ =>
        s.keyMappings.get(c.toString) match
          case Some(_) => (s, true)
          case None    => (s.copy(message = s"Unknown: $c"), true)

  private def onNormalSpecial(ke: KeyEvent, s: EditorState): ER =
    val buf = s.buffer
    ke.code match
      case KeyCode.UP =>
        (
          s.copy(buffer = buf.moveUp(s.pendingCount), pendingCount = 1),
          true
        )
      case KeyCode.DOWN =>
        (
          s.copy(
            buffer = buf.moveDown(s.pendingCount),
            pendingCount = 1
          ),
          true
        )
      case KeyCode.LEFT =>
        (
          s.copy(
            buffer = buf.moveLeft(s.pendingCount),
            pendingCount = 1
          ),
          true
        )
      case KeyCode.RIGHT =>
        (
          s.copy(
            buffer = buf.moveRight(s.pendingCount),
            pendingCount = 1
          ),
          true
        )
      case KeyCode.PAGE_DOWN if !ke.hasCtrl => (s.pageDown, true)
      case KeyCode.PAGE_UP if !ke.hasCtrl   => (s.pageUp, true)
      case _                                =>
        if ke.hasCtrl && ke.isChar('d') then (s.pageDown, true)
        else if ke.hasCtrl && ke.isChar('u') then (s.pageUp, true)
        else if ke.hasCtrl && ke.isChar('w') then
          (s.copy(message = "Ctrl-W"), true)
        else if ke.hasCtrl && ke.isChar('v') then
          (
            s.copy(
              mode = VisualBlock,
              visualStart = Some(buf.cursor),
              message = "-- VISUAL BLOCK --"
            ),
            true
          )
        else (s, true)

  // ── Insert ────────────────────────────────────────────────────────

  private def onInsert(ke: KeyEvent, s: EditorState): ER =
    if ke.code == KeyCode.ESCAPE then
      (
        s.copy(
          mode = Normal,
          message = "",
          dotRepeat = Some(DotRepeat("i", s.insertAccum.length.max(1))),
          pendingCount = 1
        ),
        true
      )
    else if ke.isDeleteBackward || ke.code == KeyCode.BACKSPACE then
      (s.copy(buffer = s.buffer.backspace), true)
    else if ke.code == KeyCode.ENTER || ke.isChar('\r') then
      (
        s.copy(
          buffer = s.buffer.splitLine,
          insertAccum = s.insertAccum + "\n"
        ),
        true
      )
    else if ke.hasCtrl && ke.isChar('w') then
      (s.copy(buffer = deleteWordBefore(s.buffer)), true)
    else if ke.code == KeyCode.CHAR then
      val c = ke.character
      if c >= 32 && c != 127 then
        (
          s.copy(
            buffer = s.buffer.insertChar(c.toChar),
            insertAccum = s.insertAccum + c.toChar
          ),
          true
        )
      else (s, true)
    else (s, true)

  private def deleteWordBefore(buf: TextBuffer): TextBuffer =
    var b = buf
    while b.col > 0 && b.currentLine.text.charAt(b.col - 1) == ' ' do
      b = b.backspace
    while b.col > 0 && b.currentLine.text
        .charAt(b.col - 1)
        .isLetterOrDigit
    do b = b.backspace
    b

  // ── Command ───────────────────────────────────────────────────────

  private def onCommand(ke: KeyEvent, s: EditorState): ER =
    if ke.code == KeyCode.ESCAPE then
      (s.copy(mode = Normal, commandBuffer = "", message = ""), true)
    else if ke.code == KeyCode.ENTER || ke.isChar('\r') then
      execCommand(s.commandBuffer.trim, s)
    else if ke.isDeleteBackward || ke.code == KeyCode.BACKSPACE then
      if s.commandBuffer.nonEmpty then
        val nc = s.commandBuffer.init;
        (s.copy(commandBuffer = nc, message = ":" + nc), true)
      else (s, true)
    else if ke.code == KeyCode.CHAR then
      if ke.character >= 32 then
        val nc = s.commandBuffer + ke.character.toChar
        (s.copy(commandBuffer = nc, message = ":" + nc), true)
      else (s, true)
    else (s, true)

  private def execCommand(cmd: String, s: EditorState): ER =
    val base = s.copy(mode = Normal, commandBuffer = "", message = "")
    cmd match
      case "q" | "q!" => (base.copy(message = "Quit"), true)
      case "w"        => (base.copy(message = "Saved"), true)
      case "wq"       => (base.copy(message = "Saved & quit"), true)
      case "nohl" | "nohlsearch" =>
        (base.copy(searchHighlight = false), true)
      case "set hlsearch"   => (base.copy(hlsearch = true), true)
      case "set nohlsearch" => (base.copy(hlsearch = false), true)
      case "help" | "h"     =>
        (
          base.copy(message =
            "Vila — minimal vim in Scala 3 via TamboUI"
          ),
          true
        )
      case x if x.startsWith("e ") || x.startsWith("edit ") =>
        (
          base.copy(message =
            s"Loading ${x.dropWhile(_ != ' ').trim}..."
          ),
          true
        )
      case x if x.startsWith("map ") =>
        x.drop(4).trim.split("\\s+", 2) match
          case Array(fr, to) =>
            (
              base.copy(
                keyMappings = base.keyMappings + (fr -> to),
                message = s"Mapped $fr -> $to"
              ),
              true
            )
          case _ =>
            (base.copy(message = "Usage: :map <from> <to>"), true)
      case _ => (base.copy(message = s"Unknown: :$cmd"), true)

  // ── Search ────────────────────────────────────────────────────────

  private def onSearch(ke: KeyEvent, s: EditorState): ER =
    if ke.code == KeyCode.ESCAPE then
      (s.copy(mode = Normal, message = ""), true)
    else if ke.code == KeyCode.ENTER || ke.isChar('\r') then
      val p = s.searchPattern
      if p.nonEmpty then
        doSearch(
          s.copy(
            lastSearchPattern = p,
            lastSearchWasForward = s.searchForward
          ),
          p,
          s.searchForward
        )
      else (s.copy(mode = Normal, message = ""), true)
    else if ke.isDeleteBackward || ke.code == KeyCode.BACKSPACE then
      if s.searchPattern.nonEmpty then
        val np = s.searchPattern.init;
        (
          s.copy(
            searchPattern = np,
            message = (if s.searchForward then "/" else "?") + np
          ),
          true
        )
      else (s, true)
    else if ke.code == KeyCode.CHAR then
      if ke.character >= 32 then
        val np = s.searchPattern + ke.character.toChar
        val s2 = s.copy(
          searchPattern = np,
          message = (if s.searchForward then "/" else "?") + np
        )
        if np.nonEmpty then doSearch(s2, np, s.searchForward)
        else (s2, true)
      else (s, true)
    else (s, true)

  private def doSearch(s: EditorState, pat: String, fwd: Boolean): ER =
    (if fwd then s.buffer.findForward(pat)
     else s.buffer.findBackward(pat)) match
      case Some(p) =>
        (
          s.copy(
            buffer = s.buffer.copy(cursor = p),
            searchPattern = pat,
            searchForward = fwd,
            searchHighlight = true
          ),
          true
        )
      case None =>
        (
          s.copy(message = s"Not found: $pat", searchPattern = pat),
          true
        )

  // ── Visual ────────────────────────────────────────────────────────

  private def onVisual(ke: KeyEvent, s: EditorState): ER =
    if ke.code == KeyCode.ESCAPE || (ke.isChar(
        'v'
      ) && s.mode == VisualChar)
    then (s.copy(mode = Normal, visualStart = None, message = ""), true)
    else if ke.isChar('V') then
      val nm = if s.mode == VisualLine then Normal else VisualLine
      (
        s.copy(
          mode = nm,
          message = if nm == VisualLine then "-- VISUAL LINE --" else ""
        ),
        true
      )
    else if ke.isChar('d') || ke.isChar('x') then
      val (st, en) = visRange(s);
      val (nb, del) = s.buffer.deleteRange(st, en)
      (
        s.copy(
          buffer = nb,
          yankRegister = del,
          mode = Normal,
          visualStart = None,
          message = ""
        ),
        true
      )
    else if ke.isChar('c') then
      val (st, en) = visRange(s);
      val (nb, del) = s.buffer.deleteRange(st, en)
      enterInsert(
        s.copy(
          buffer = nb,
          yankRegister = del,
          mode = Normal,
          visualStart = None
        )
      )
    else if ke.isChar('y') then
      val (st, en) = visRange(s)
      (
        s.copy(
          yankRegister = s.buffer.yankRange(st, en),
          mode = Normal,
          visualStart = None,
          message = "Yanked"
        ),
        true
      )
    else if ke.isChar('>') || ke.isChar('<') then
      val (st, en) = visRange(s)
      val buf = (st.row to en.row).foldLeft(s.buffer) { (b, r) =>
        if ke.isChar('>') then b.indentLine(r) else b.outdentLine(r)
      }
      (
        s.copy(
          buffer = buf,
          mode = Normal,
          visualStart = None,
          message = ""
        ),
        true
      )
    else if ke.isChar('~') then
      val (st, en) = visRange(s);
      (
        s.copy(
          buffer = toggleRange(st, en, s.buffer),
          mode = Normal,
          visualStart = None,
          message = ""
        ),
        true
      )
    else
      val (s2, _) = onNormal(ke, s);
      (s2.copy(visualStart = s.visualStart), true)

  private def visRange(s: EditorState): (CursorPos, CursorPos) =
    val start = s.visualStart.getOrElse(s.buffer.cursor)
    val end = s.buffer.cursor
    if s.mode == VisualLine then
      (
        CursorPos(start.row.min(end.row), 0),
        CursorPos(
          start.row.max(end.row),
          s.buffer.lines(start.row.max(end.row)).length
        )
      )
    else (start, end)

  private def toggleRange(
      from: CursorPos,
      to: CursorPos,
      buf: TextBuffer
  ): TextBuffer =
    val r1 = from.row.min(to.row); val r2 = from.row.max(to.row)
    val c1 = if from.row <= to.row then from.col else to.col
    val c2 = if from.row <= to.row then to.col else from.col
    (r1 to r2).foldLeft(buf) { (b, r) =>
      val ln = b.lines(r)
      val fc = if r == r1 then c1 else 0
      val tc = if r == r2 then c2 else ln.length
      val nt = (fc until tc).foldLeft(ln.text) { (t, i) =>
        val ch = t.charAt(i);
        val togg = if ch.isLower then ch.toUpper else ch.toLower
        t.substring(0, i) + togg + t.substring(i + 1)
      }
      b.copy(lines = b.lines.updated(r, TextLine(nt)))
    }

  // ── Operator Pending ──────────────────────────────────────────────

  private def onOpPending(ke: KeyEvent, s: EditorState): ER =
    if ke.code == KeyCode.ESCAPE then
      (s.copy(mode = Normal, pendingOp = None, pendingCount = 1), true)
    else if s.pendingTextObject then onTextObject(ke, s)
    else onMotionWithOp(ke, s)

  private def onMotionWithOp(ke: KeyEvent, s: EditorState): ER =
    val op = s.pendingOp.get.op
    if ke.code == KeyCode.CHAR && ke.character == 'i' then
      (s.copy(pendingTextObject = true), true)
    else if ke.code == KeyCode.CHAR && ke.character == 'a' then
      (s.copy(pendingTextObject = true), true)
    else
      motionTarget(ke, s) match
        case Some(end) =>
          applyOp(
            op,
            s.buffer.cursor,
            end,
            s.copy(pendingOp = None, mode = Normal, pendingCount = 1)
          )
        case None =>
          (
            s.copy(pendingOp = None, mode = Normal, pendingCount = 1),
            true
          )

  private def motionTarget(
      ke: KeyEvent,
      s: EditorState
  ): Option[CursorPos] =
    val buf = s.buffer
    if ke.code == KeyCode.CHAR then
      ke.character match
        case 'w'       => Some(buf.wordForward.cursor)
        case 'b'       => Some(buf.wordBack.cursor)
        case 'e'       => Some(buf.wordEnd.cursor)
        case 'W'       => Some(buf.bigWordForward.cursor)
        case 'B'       => Some(buf.bigWordBack.cursor)
        case 'E'       => Some(buf.bigWordEnd.cursor)
        case '$'       => Some(buf.moveEnd.cursor)
        case '0' | '^' => Some(CursorPos(buf.row, 0))
        case 'h'       => Some(buf.moveLeft().cursor)
        case 'j'       => Some(buf.moveDown().cursor)
        case 'k'       => Some(buf.moveUp().cursor)
        case 'l'       => Some(buf.moveRight().cursor)
        case 'G'       => Some(buf.moveG.cursor)
        case '{'       => Some(buf.paragraphBack.clampCursor.cursor)
        case '}'       => Some(buf.paragraphForward.clampCursor.cursor)
        case _         => None
    else None

  private def onTextObject(ke: KeyEvent, s: EditorState): ER =
    val op = s.pendingOp.get.op
    if ke.code == KeyCode.CHAR then
      val ch = ke.character
      val (st, en) = findTextObject(ch, s.buffer)
      applyOp(
        op,
        st,
        en,
        s.copy(
          pendingOp = None,
          pendingTextObject = false,
          mode = Normal,
          pendingCount = 1
        )
      )
    else
      (
        s.copy(
          pendingOp = None,
          pendingTextObject = false,
          mode = Normal
        ),
        true
      )

  private def findTextObject(
      ch: Char,
      buf: TextBuffer
  ): (CursorPos, CursorPos) =
    val r = buf.row; val line = buf.currentLine.text; val c = buf.col
    ch match
      case 'w' => (buf.wordBack.cursor, buf.wordForward.cursor)
      case 'W' => (buf.bigWordBack.cursor, buf.bigWordForward.cursor)
      case '"' | '\'' =>
        val s = line.lastIndexOf(ch, c - 1).max(0)
        val e = line.indexOf(ch, c + 1);
        val ee = if e < 0 then line.length else e
        (CursorPos(r, s + 1), CursorPos(r, ee))
      case '(' | ')' =>
        val s = line.lastIndexOf('(', c).max(0)
        val e = line.indexOf(')', c);
        val ee = if e < 0 then line.length else e
        (CursorPos(r, s + 1), CursorPos(r, ee))
      case '[' | ']' =>
        val s = line.lastIndexOf('[', c).max(0)
        val e = line.indexOf(']', c);
        val ee = if e < 0 then line.length else e
        (CursorPos(r, s + 1), CursorPos(r, ee))
      case '{' | '}' =>
        val s = line.lastIndexOf('{', c).max(0)
        val e = line.indexOf('}', c);
        val ee = if e < 0 then line.length else e
        (CursorPos(r, s + 1), CursorPos(r, ee))
      case '<' | '>' =>
        val s = line.lastIndexOf('<', c).max(0)
        val e = line.indexOf('>', c);
        val ee = if e < 0 then line.length else e
        (CursorPos(r, s + 1), CursorPos(r, ee))
      case _ => (buf.cursor, buf.cursor)

  private def applyOp(
      op: String,
      from: CursorPos,
      to: CursorPos,
      s: EditorState
  ): ER = op match
    case "d" =>
      val (nb, del) = s.buffer.deleteRange(from, to);
      (
        s.copy(
          buffer = nb,
          yankRegister = del,
          dotRepeat = Some(DotRepeat("d"))
        ),
        true
      )
    case "c" =>
      val (nb, del) = s.buffer.deleteRange(from, to);
      enterInsert(
        s.copy(
          buffer = nb,
          yankRegister = del,
          dotRepeat = Some(DotRepeat("c"))
        )
      )
    case "y" =>
      (
        s.copy(
          yankRegister = s.buffer.yankRange(from, to),
          message = "Yanked"
        ),
        true
      )
    case ">" =>
      val buf =
        (from.row.min(to.row) to from.row.max(to.row))
          .foldLeft(s.buffer)((b, r) => b.indentLine(r));
      (s.copy(buffer = buf), true)
    case "<" =>
      val buf =
        (from.row.min(to.row) to from.row.max(to.row))
          .foldLeft(s.buffer)((b, r) => b.outdentLine(r));
      (s.copy(buffer = buf), true)
    case _ => (s, true)

  // ── Helpers ───────────────────────────────────────────────────────

  private def enterInsert(s: EditorState): ER =
    (
      s.copy(mode = Insert, message = "-- INSERT --", insertAccum = ""),
      true
    )

  private def execPasteAfter(s: EditorState): EditorState =
    val t = s.yankRegister
    if t.isEmpty then s
    else if t.endsWith("\n") then
      val c = t.stripLineEnd; val b = s.buffer.insertLineBelow
      s.copy(buffer =
        b.copy(
          lines = b.lines.updated(b.row, TextLine(c)),
          cursor = CursorPos(b.row, c.length)
        )
      )
    else s.copy(buffer = s.buffer.insertString(t))

  private def execPasteBefore(s: EditorState): EditorState =
    val t = s.yankRegister
    if t.isEmpty then s
    else if t.endsWith("\n") then
      val c = t.stripLineEnd; val b = s.buffer.insertLineAbove
      s.copy(buffer =
        b.copy(
          lines = b.lines.updated(b.row, TextLine(c)),
          cursor = CursorPos(b.row, c.length)
        )
      )
    else s.copy(buffer = s.buffer.insertString(t))

  private def execDotRepeat(s: EditorState): EditorState =
    s.dotRepeat match
      case Some(DotRepeat("x", _)) =>
        val e = CursorPos(
          s.buffer.row,
          (s.buffer.col + 1).min(s.buffer.currentLine.length)
        )
        val (nb, _) = s.buffer.deleteRange(s.buffer.cursor, e);
        s.copy(buffer = nb)
      case Some(DotRepeat("dd", _)) =>
        s.copy(
          buffer = s.buffer.deleteLine,
          yankRegister = s.buffer.yankLine
        )
      case Some(DotRepeat("yy", _)) =>
        s.copy(yankRegister = s.buffer.yankLine)
      case Some(DotRepeat("p", _)) => execPasteAfter(s)
      case Some(DotRepeat("P", _)) => execPasteBefore(s)
      case Some(DotRepeat("D", _)) =>
        val e = CursorPos(s.buffer.row, s.buffer.currentLine.length)
        val (nb, _) = s.buffer.deleteRange(s.buffer.cursor, e);
        s.copy(buffer = nb)
      case Some(DotRepeat("i", _)) =>
        if s.insertAccum.nonEmpty then
          s.copy(buffer = s.buffer.insertString(s.insertAccum))
        else s
      case _ => s

  private def onFChar(ke: KeyEvent, s: EditorState): ER =
    if ke.code == KeyCode.CHAR then
      val ch = ke.character.toChar
      val cmd = s.waitingForFChar.get; val fwd = s.waitingForFwd;
      val till = cmd == 't' || cmd == 'T'
      val res = (if fwd then s.buffer.findCharForward(ch)
                 else s.buffer.findCharBackward(ch)).map { idx =>
        s.copy(
          buffer = s.buffer.copy(cursor =
            CursorPos(
              s.buffer.row,
              if (fwd && till && idx > 0) || (!fwd && till) then
                idx + (if fwd then -1 else 1)
              else idx
            )
          ),
          lastFCommand = Some((cmd.toString, ch)),
          waitingForFChar = None
        )
      }
      (res.getOrElse(s.copy(waitingForFChar = None)), true)
    else (s, true)

  private def repeatLastF(s: EditorState, forward: Boolean): ER =
    s.lastFCommand match
      case Some((cmd, ch)) =>
        val fwd = cmd == "f" || cmd == "t"
        val actualFwd = if forward then fwd else !fwd
        val till = cmd == "t" || cmd == "T"
        val res = (if actualFwd then s.buffer.findCharForward(ch)
                   else s.buffer.findCharBackward(ch)).map { idx =>
          s.copy(buffer =
            s.buffer.copy(cursor =
              CursorPos(
                s.buffer.row,
                if (actualFwd && till && idx > 0) || (!actualFwd && till)
                then idx + (if actualFwd then -1 else 1)
                else idx
              )
            )
          )
        }
        (res.getOrElse(s), true)
      case None => (s, true)

end InputHandler

// ═══════════════════════════════════════════════════════════════════
//  VimRenderer — pure rendering
// ═══════════════════════════════════════════════════════════════════

object VimRenderer:
  import dev.tamboui.terminal.Frame
  import dev.tamboui.buffer.Buffer
  import dev.tamboui.style.{Color, Style}
  import Mode.*

  def render(frame: Frame, state: EditorState): Unit =
    val buf = frame.buffer()
    val w = frame.width()
    val h = frame.height()
    val edH = h - 2
    val lnW = 4
    buf.clear(frame.area())

    val (sL, eL) = state.visibleLines

    // text lines
    for i <- sL until eL.min(state.buffer.lineCount) do
      val sr = i - sL; val txt = state.buffer.line(i).text
      if sr < edH then
        // line number
        buf.setString(
          0,
          sr,
          s"${i + 1}%${lnW - 1}s".format(" "),
          Style.EMPTY.fg(Color.GRAY).bold()
        )
        // content
        buf.setString(lnW, sr, txt.take((w - lnW).max(0)), Style.EMPTY)
        // search highlight
        if state.searchHighlight && state.searchPattern.nonEmpty then
          val hl = Style.EMPTY.bg(Color.YELLOW).fg(Color.BLACK).bold()
          var idx = txt.indexOf(state.searchPattern)
          while idx >= 0 do
            val sc = idx + lnW
            if sc < w then
              buf.setString(
                sc,
                sr,
                state.searchPattern.take((w - sc).max(0)),
                hl
              )
            idx = txt.indexOf(state.searchPattern, idx + 1)
        // visual selection
        for vs <- state.visualStart do
          val ve = state.buffer.cursor
          val r1 = vs.row.min(ve.row); val r2 = vs.row.max(ve.row)
          if i >= r1 && i <= r2 then
            val c1 = (if i == r1 then vs.col else 0) + lnW
            val c2 = (if i == r2 then ve.col else txt.length) + lnW
            val sel = Style.EMPTY.bg(Color.MAGENTA)
            for c <- c1 until c2.min(w) do
              buf.setString(c, sr, " ", sel)
            val vis = txt.substring(
              (c1 - lnW).max(0).min(txt.length),
              (c2 - lnW).min(txt.length)
            )
            if vis.nonEmpty then buf.setString(c1.max(0), sr, vis, sel)
        // cursor line
        if i == state.buffer.row then
          val curS = state.mode match
            case Insert => Style.EMPTY.bg(Color.GREEN)
            case VisualChar | VisualLine | VisualBlock =>
              Style.EMPTY.bg(Color.MAGENTA)
            case OperatorPending => Style.EMPTY.bg(Color.YELLOW)
            case _               => Style.EMPTY
          val cx = state.buffer.col + lnW
          if cx < w then
            val ch = if state.buffer.col < txt.length then
              txt.charAt(state.buffer.col).toString
            else " "
            buf.setString(cx, sr, ch, curS)

    // status bar
    val stS = Style.EMPTY.bg(Color.WHITE).fg(Color.BLACK).bold()
    val mi = state.mode match
      case Normal          => " NORMAL ";
      case Insert          => " INSERT "
      case VisualChar      => " VISUAL ";
      case VisualLine      => " VISUAL LINE "
      case VisualBlock     => " VISUAL BLOCK ";
      case Command         => " COMMAND "
      case Search          => " SEARCH ";
      case OperatorPending => " PENDING "
    val fn = state.buffer.filename.getOrElse("[No Name]")
    val md = if state.buffer.modified then " [+]" else ""
    val lt = s"$mi$fn$md";
    val rt = s"${state.buffer.row + 1},${state.buffer.col + 1}"
    buf.setString(0, h - 2, " " * w, stS)
    buf.setString(0, h - 2, lt.take(w / 2), stS)
    buf.setString((w - rt.length - 1).max(0), h - 2, rt, stS)

    // command line
    if state.mode == Command || state.mode == Search then
      buf.setString(
        0,
        h - 1,
        state.message.take(w),
        Style.EMPTY.bg(Color.YELLOW).fg(Color.BLACK)
      )
    else buf.setString(0, h - 1, state.message.take(w), Style.EMPTY)

    // cursor
    val cr = state.buffer.row - state.scrollTop;
    val cc = state.buffer.col + lnW
    if cr >= 0 && cr < edH then
      frame.setCursorPosition(cc.min(w - 1), cr)

// ═══════════════════════════════════════════════════════════════════
//  Application entry point
// ═══════════════════════════════════════════════════════════════════

@main def runVim(): Unit =
  // Directly create the JLine backend to bypass ServiceLoader (needed for GraalVM native image)
  val provider = new dev.tamboui.backend.jline3.JLineBackendProvider()
  val backend = provider.create()

  val config = dev.tamboui.tui.TuiConfig
    .builder()
    .backend(backend)
    .rawMode(true)
    .alternateScreen(true)
    .hideCursor(false)
    .build()

  val runner = dev.tamboui.tui.TuiRunner.create(config)
  try
    val app = new VimApp()
    runner.run(app, app)
  finally runner.close()

final class VimApp
    extends dev.tamboui.tui.EventHandler
    with dev.tamboui.tui.Renderer:
  private var state: EditorState = EditorState()

  def handle(
      event: dev.tamboui.tui.event.Event,
      runner: dev.tamboui.tui.TuiRunner
  ): Boolean =
    event match
      case ke: dev.tamboui.tui.event.KeyEvent =>
        if ke.isQuit then { runner.quit(); false }
        else
          val (ns, redraw) = InputHandler.handle(ke, state)
          state = ns.copy(
            termWidth = state.termWidth,
            termHeight = state.termHeight
          )
          redraw
      case _ => false

  def render(frame: dev.tamboui.terminal.Frame): Unit =
    state = state
      .copy(termWidth = frame.width(), termHeight = frame.height())
      .ensureCursorVisible
    VimRenderer.render(frame, state)
