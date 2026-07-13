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
    undoStack: Vector[TextBuffer] = Vector.empty,
    redoStack: Vector[TextBuffer] = Vector.empty
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
      undoStack = undoStack :+ this, redoStack = Vector.empty
    )

  def insertString(s: String): TextBuffer =
    s.foldLeft(this)((b, c) => b.insertChar(c))

  def backspace: TextBuffer =
    if col > 0 then
      copy(
        lines = lines.updated(row, currentLine.delete(col - 1)),
        modified = true,
        cursor = CursorPos(row, col - 1),
        undoStack = undoStack :+ this, redoStack = Vector.empty
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
        undoStack = undoStack :+ this, redoStack = Vector.empty
      )
    else
      this

  def splitLine: TextBuffer =
    val safeCol = col.min(currentLine.length)
    val before = currentLine.text.substring(0, safeCol)
    val after  = currentLine.text.substring(safeCol)
    val indent = before.takeWhile(_.isWhitespace)
    copy(
      lines =
        lines.patch(row, Seq(TextLine(before), TextLine(after)), 1),
      modified = true,
      cursor = CursorPos(row + 1, indent.length),
      undoStack = undoStack :+ this, redoStack = Vector.empty
    )

  def deleteLine: TextBuffer =
    if lines.length == 1 then
      copy(
        lines = Vector(TextLine.empty),
        modified = true,
        cursor = CursorPos(0, 0),
        undoStack = undoStack :+ this, redoStack = Vector.empty
      )
    else
      val nr = row.min(lines.length - 2)
      copy(
        lines = lines.patch(row, Seq.empty, 1),
        modified = true,
        cursor = CursorPos(nr, 0),
        undoStack = undoStack :+ this, redoStack = Vector.empty
      )

  def yankLine: String = currentLine.text + "\n"

  def insertLineBelow: TextBuffer =
    copy(
      lines = lines.patch(row + 1, Seq(TextLine.empty), 0),
      modified = true,
      cursor = CursorPos(row + 1, 0),
      undoStack = undoStack :+ this, redoStack = Vector.empty
    )

  def insertLineAbove: TextBuffer =
    copy(
      lines = lines.patch(row, Seq(TextLine.empty), 0),
      modified = true,
      cursor = CursorPos(row, 0),
      undoStack = undoStack :+ this, redoStack = Vector.empty
    )

  def indentLine(r: Int, amount: Int = 2): TextBuffer =
    copy(
      lines = lines.updated(r, TextLine(" " * amount + lines(r).text)),
      modified = true,
      undoStack = undoStack :+ this, redoStack = Vector.empty
    )

  def outdentLine(r: Int, amount: Int = 2): TextBuffer =
    val toRemove = lines(r).text.take(amount).count(_ == ' ')
    copy(
      lines =
        lines.updated(r, TextLine(lines(r).text.substring(toRemove))),
      modified = true,
      undoStack = undoStack :+ this, redoStack = Vector.empty
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
        undoStack = undoStack :+ this, redoStack = Vector.empty
      )
    else this

  def undo: TextBuffer =
    if undoStack.isEmpty then this
    else
      val prev = undoStack.last
      // prev is the state before this change. Push current state onto redo stack.
      prev.copy(redoStack = redoStack :+ this)

  def redo: TextBuffer =
    if redoStack.isEmpty then this
    else
      val next = redoStack.last
      // next is the state after this undo. Push current state onto undo stack.
      next.copy(undoStack = undoStack :+ this)

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
          undoStack = undoStack :+ this, redoStack = Vector.empty
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
          undoStack = undoStack :+ this, redoStack = Vector.empty
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

  /** Serialize the buffer content to a string suitable for writing to a file. */
  def serialize: String = lines.map(_.text).mkString("\n") + "\n"

// ═══════════════════════════════════════════════════════════════════
//  Split pane and layout types
// ═══════════════════════════════════════════════════════════════════

/** A simple split: one pane shares the single buffer model. */
final case class SplitPane(
    buffer: TextBuffer = TextBuffer(),
    scrollTop: Int = 0,
    yankRegister: String = "",
    searchPattern: String = "",
    searchForward: Boolean = true
)

/** Flat list of panes arranged in a grid. */
final case class SplitGrid(
    panes: Vector[SplitPane],
    active: Int,
    rows: Int,
    cols: Int
):
  def activePane: SplitPane = panes(active)
  def withPane(f: SplitPane => SplitPane): SplitGrid =
    copy(panes = panes.updated(active, f(activePane)))
  def paneAt(r: Int, c: Int): Option[SplitPane] =
    val idx = r * cols + c
    if idx < panes.length then Some(panes(idx)) else None
  def paneRowCol(idx: Int): (Int, Int) = (idx / cols, idx % cols)

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
    lastKey: Int = 0,
    splits: Option[SplitGrid] = None  // None = single pane mode
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

  // ── Split management ────────────────────────────────────────────

  /** Total number of panes across all splits. */
  def paneCount: Int = splits.map(_.panes.length).getOrElse(1)

  /** Height available per pane. */
  def paneHeight: Int =
    splits match
      case Some(g) => (termHeight - 2) / g.rows
      case None    => termHeight - 2

  /** Width available per pane. */
  def paneWidth: Int =
    splits match
      case Some(g) => termWidth / g.cols
      case None    => termWidth

  /** Row of the active pane on screen. */
  def activePaneRow: Int =
    splits match
      case Some(g) =>
        val (r, _) = g.paneRowCol(g.active)
        r * paneHeight
      case None => 0

  /** Column of the active pane on screen. */
  def activePaneCol: Int =
    splits match
      case Some(g) =>
        val (_, c) = g.paneRowCol(g.active)
        c * paneWidth
      case None => 0

  /** Set the active pane's buffer and scroll. */
  def updateActivePane(f: SplitPane => SplitPane): EditorState =
    splits match
      case Some(g) => copy(splits = Some(g.withPane(f)))
      case None    => this

  /** Get SplitPane state for the active pane. */
  def activeSplitPane: SplitPane =
    splits match
      case Some(g) => g.activePane
      case None => SplitPane(buffer = buffer, scrollTop = scrollTop,
        yankRegister = yankRegister, searchPattern = searchPattern,
        searchForward = searchForward)

  /** Sync EditorState fields from the active SplitPane. */
  def syncFromSplits: EditorState =
    splits match
      case Some(g) =>
        val p = g.activePane
        copy(buffer = p.buffer, scrollTop = p.scrollTop,
          yankRegister = p.yankRegister, searchPattern = p.searchPattern,
          searchForward = p.searchForward)
      case None => this

  /** Sync SplitPane from EditorState fields and return new state. */
  def syncToSplits: EditorState =
    splits match
      case Some(g) =>
        val upd = g.withPane(_ => SplitPane(buffer = buffer, scrollTop = scrollTop,
          yankRegister = yankRegister, searchPattern = searchPattern,
          searchForward = searchForward))
        copy(splits = Some(upd))
      case None => this

  /** Navigate to the pane in the given direction. Returns (newState, moved). */
  def navigateSplit(dir: Char): EditorState =
    splits match
      case Some(g) =>
        val (r, c) = g.paneRowCol(g.active)
        val (nr, nc) = dir match
          case 'h' => (r, (c - 1).max(0))
          case 'l' => (r, (c + 1).min(g.cols - 1))
          case 'k' => ((r - 1).max(0), c)
          case 'j' => ((r + 1).min(g.rows - 1), c)
          case _   => (r, c)
        val newIdx = nr * g.cols + nc
        if newIdx >= 0 && newIdx < g.panes.length && newIdx != g.active then
          val synced = syncToSplits
          val grid = synced.splits.get.copy(active = newIdx)
          synced.copy(splits = Some(grid)).syncFromSplits
        else this
      case None => this

  /** Create a horizontal split of the current pane. */
  def splitHorizontal: EditorState =
    val pane = SplitPane(buffer = buffer, scrollTop = scrollTop,
      yankRegister = yankRegister, searchPattern = searchPattern,
      searchForward = searchForward)
    splits match
      case Some(g) =>
        val newRows = g.rows + 1
        val newPanes = g.panes :+ pane
        copy(splits = Some(SplitGrid(newPanes, g.active, newRows, g.cols)))
      case None =>
        copy(splits = Some(SplitGrid(Vector(pane, pane), 0, 2, 1)))

  /** Create a vertical split of the current pane. */
  def splitVertical: EditorState =
    val pane = SplitPane(buffer = buffer, scrollTop = scrollTop,
      yankRegister = yankRegister, searchPattern = searchPattern,
      searchForward = searchForward)
    splits match
      case Some(g) =>
        val newCols = g.cols + 1
        val newPanes = g.panes :+ pane
        copy(splits = Some(SplitGrid(newPanes, g.active, g.rows, newCols)))
      case None =>
        copy(splits = Some(SplitGrid(Vector(pane, pane), 0, 1, 2)))

  /** Close the active split pane. */
  def closeSplit: EditorState =
    splits match
      case Some(g) if g.panes.length <= 1 => copy(splits = None)
      case Some(g) =>
        val newPanes = g.panes.patch(g.active, Seq.empty, 1)
        val newActive = g.active.min(newPanes.length - 1)
        val total = newPanes.length
        // Recompute rows/cols to keep roughly square
        val newCols = math.ceil(math.sqrt(total.toDouble)).toInt
        val newRows = (total + newCols - 1) / newCols
        val grid = SplitGrid(newPanes, newActive, newRows.max(1), newCols.max(1))
        copy(splits = Some(grid)).syncFromSplits
      case None => this

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
      // Some backends send Ctrl+V as character 'v' with Ctrl modifier set,
      // rather than as the raw ASCII control character (22). Check here.
      if ke.hasCtrl then
        val buf2 = s.buffer
        ke.character match
          case 'd' | 4  => return (s.pageDown, true)
          case 'u' | 21 => return (s.pageUp, true)
          case 'v' | 22 => return (s.copy(mode = VisualBlock,
            visualStart = Some(buf2.cursor),
            message = "-- VISUAL BLOCK --"), true)
          case 'w' | 23 => return (s.copy(lastKey = 'W', pendingCount = 1), true)
          case 'r' | 18 => return (s.copy(buffer = buf2.redo,
            message = "Redo"), true)
          case _ => ()
      onNormalChar(ke.character.toChar, s)
    else onNormalSpecial(ke, s)

  private def onNormalChar(c: Char, s: EditorState): ER =
    val buf = s.buffer

    // two-key sequences
    // Must return immediately — otherwise fall-through overwrites the result
    if s.lastKey == 'g' then
      val s2 = s.copy(lastKey = 0)
      return c match
        case 'g' => (s2.copy(buffer = buf.moveGg), true)
        case 'e' => (s2.copy(buffer = buf.wordBack.wordEnd), true)
        case 'E' => (s2.copy(buffer = buf.bigWordBack.bigWordEnd), true)
        case 'R' => (s2.copy(message = "Reloaded"), true)
        case _   => onNormalChar(c, s2)
    else if s.lastKey == 'z' then
      val s2 = s.copy(lastKey = 0)
      return c match
        case 'z' => (s2.centerCursor, true)                         // zz
        case 't' => (s2.copy(scrollTop = buf.row), true)            // zt
        case 'b' =>                                                  // zb
          (s2.copy(scrollTop = (buf.row - s2.termHeight + 3).max(0)), true)
        case _ => onNormalChar(c, s2)
    else if s.lastKey == 'Z' then
      val s2 = s.copy(lastKey = 0)
      return c match
        case 'Z' => (s2.copy(message = "Saved & quit"), true)      // ZZ
        case 'Q' => (s2.copy(message = "Quit"), true)              // ZQ
        case _ => onNormalChar(c, s2)
    else if s.lastKey == 'W' then
      val s2 = s.copy(lastKey = 0)
      return c match
        case 'h' => (s2.navigateSplit('h'), true)                    // Ctrl-W h
        case 'j' => (s2.navigateSplit('j'), true)                    // Ctrl-W j
        case 'k' => (s2.navigateSplit('k'), true)                    // Ctrl-W k
        case 'l' => (s2.navigateSplit('l'), true)                    // Ctrl-W l
        case 'v' => (s2.splitVertical, true)                         // Ctrl-W v
        case 's' => (s2.splitHorizontal, true)                       // Ctrl-W s
        case 'q' => (s2.closeSplit, true)                            // Ctrl-W q
        case 'w' =>                                                   // Ctrl-W w (next)
          if s2.paneCount > 1 then
            val nextPane = (0 until s2.paneCount - 1)
              .foldLeft(s2)((st, _) => st.navigateSplit('j'))
            (nextPane, true)
          else (s2, true)
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

    // Ctrl+letter (ASCII control codes 1-26) — handle before the main match
    // so they don't fall through to 'unknown key'
    val ctrlCode = c.toInt
    if ctrlCode >= 1 && ctrlCode <= 26 then
      val buf2 = s.buffer
      return ctrlCode match
        case 4  => (s.pageDown, true)                                 // Ctrl-D
        case 21 => (s.pageUp, true)                                   // Ctrl-U
        case 22 =>                                                    // Ctrl-V (visual block)
          (s.copy(mode = VisualBlock, visualStart = Some(buf2.cursor),
            message = "-- VISUAL BLOCK --"), true)
        case 18 =>                                                     // Ctrl-R (redo)
          (s.copy(buffer = buf2.redo, message = "Redo"), true)
        case 23 =>                                                     // Ctrl-W (enter ctrl-w pending)
          (s.copy(lastKey = 'W', pendingCount = 1), true)
        case _  => (s, true)

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
      case 'd' =>
        (
          s.copy(
            pendingOp = Some(PendingOp("d")),
            mode = OperatorPending
          ),
          true
        )
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
      case 'Z' => (s.copy(lastKey = 'Z', pendingCount = 1), true)

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
        (s, true)

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
    else if (ke.code == KeyCode.CHAR && ke.character == 23) ||
      (ke.hasCtrl && ke.isChar('w')) then
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
      case x if x == "w" || x.startsWith("w ") =>
        val fname = if x.startsWith("w ") then x.drop(2).trim else ""
        val withFname = if fname.nonEmpty then
          base.copy(buffer = base.buffer.copy(filename = Some(fname)))
        else base
        (withFname.copy(message = "Saved"), true)
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
      case x if x == "split" || x.startsWith("split ") =>
        val result = base.splitHorizontal
        (result.syncFromSplits.copy(message = "Split horizontal"), true)
      case x if x == "vsplit" || x.startsWith("vsplit ") =>
        val result = base.splitVertical
        (result.syncFromSplits.copy(message = "Split vertical"), true)
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
    // Detect Ctrl+V by ASCII control code 22 or modifier-based check
    val isCtrlV = (ke.code == KeyCode.CHAR && ke.character == 22) ||
      (ke.hasCtrl && ke.isChar('v'))

    // Check Ctrl+V before plain 'v' to avoid the backend that sends
    // Ctrl+V as character 'v' with Ctrl modifier from matching 'v' first.
    if isCtrlV then
      val newMode = if s.mode == VisualBlock then Normal else VisualBlock
      (s.copy(mode = newMode, message = if newMode == VisualBlock then
        "-- VISUAL BLOCK --" else ""), true)
    else if ke.code == KeyCode.ESCAPE || (ke.isChar(
        'v'
      ) && s.mode == VisualChar) then
      (s.copy(mode = Normal, visualStart = None, message = ""), true)
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
      val result =
        if s.mode == VisualBlock then deleteBlock(s)
        else
          val (st, en) = visRange(s)
          val (nb, del) = s.buffer.deleteRange(st, en)
          (nb, del)
      (
        s.copy(
          buffer = result._1,
          yankRegister = result._2,
          mode = Normal,
          visualStart = None,
          message = ""
        ),
        true
      )
    else if ke.isChar('c') then
      val result =
        if s.mode == VisualBlock then deleteBlock(s)
        else
          val (st, en) = visRange(s)
          s.buffer.deleteRange(st, en)
      enterInsert(
        s.copy(
          buffer = result._1,
          yankRegister = result._2,
          mode = Normal,
          visualStart = None
        )
      )
    else if ke.isChar('y') then
      val register =
        if s.mode == VisualBlock then yankBlock(s)
        else
          val (st, en) = visRange(s)
          s.buffer.yankRange(st, en)
      (
        s.copy(
          yankRegister = register,
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
      val result =
        if s.mode == VisualBlock then toggleBlock(s)
        else
          val (st, en) = visRange(s);
          toggleRange(st, en, s.buffer)
      (
        s.copy(
          buffer = result,
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
    else if ke.code == KeyCode.CHAR && ke.character == op.head then
      // Linewise operator (dd, cc, yy)
      op match
        case "d" =>
          val del = s.buffer.yankLine
          (s.copy(buffer = s.buffer.deleteLine, yankRegister = del,
            dotRepeat = Some(DotRepeat("dd")), pendingOp = None, mode = Normal,
            pendingCount = 1), true)
        case "c" =>
          val del = s.buffer.yankLine
          enterInsert(s.copy(buffer = s.buffer.deleteLine, yankRegister = del,
            dotRepeat = Some(DotRepeat("cc")), pendingOp = None,
            pendingCount = 1))
        case "y" =>
          (s.copy(yankRegister = s.buffer.yankLine,
            dotRepeat = Some(DotRepeat("yy")), message = "Yanked",
            pendingOp = None, mode = Normal, pendingCount = 1), true)
        case _ =>
          (s.copy(pendingOp = None, mode = Normal, pendingCount = 1), true)
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

  // ── Visual block helpers ────────────────────────────────────────────

  /** Get the column range for the current visual block. */
  private def blockColRange(s: EditorState): (Int, Int) =
    val vs = s.visualStart.getOrElse(s.buffer.cursor)
    val ve = s.buffer.cursor
    (vs.col.min(ve.col), vs.col.max(ve.col))

  /** Yank just the rectangular columns from each selected row. */
  private def yankBlock(s: EditorState): String =
    val vs = s.visualStart.getOrElse(s.buffer.cursor)
    val ve = s.buffer.cursor
    val r1 = vs.row.min(ve.row); val r2 = vs.row.max(ve.row)
    val (c1, c2) = (vs.col.min(ve.col), vs.col.max(ve.col))
    (r1 to r2).map { r =>
      val ln = s.buffer.lines(r).text
      if c1 < ln.length then ln.substring(c1, c2.min(ln.length))
      else ""
    }.mkString("\n") + "\n"

  /** Delete the rectangular block and return (newBuffer, deletedText). */
  private def deleteBlock(s: EditorState): (TextBuffer, String) =
    val vs = s.visualStart.getOrElse(s.buffer.cursor)
    val ve = s.buffer.cursor
    val r1 = vs.row.min(ve.row); val r2 = vs.row.max(ve.row)
    val (c1, c2) = (vs.col.min(ve.col), vs.col.max(ve.col))
    val deleted = (r1 to r2).map { r =>
      val ln = s.buffer.lines(r).text
      if c1 < ln.length then ln.substring(c1, c2.min(ln.length))
      else ""
    }.mkString("\n") + "\n"
    val newLines = s.buffer.lines.zipWithIndex.map { (ln, r) =>
      if r >= r1 && r <= r2 && c1 < ln.length then
        val right = if c2 < ln.length then ln.text.substring(c2) else ""
        TextLine(ln.text.substring(0, c1) + right)
      else ln
    }
    val newBuf = s.buffer.copy(lines = newLines, modified = true,
      cursor = CursorPos(r1, c1), undoStack = s.buffer.undoStack :+ s.buffer, redoStack = Vector.empty)
    (newBuf, deleted)

  /** Toggle case in a rectangular block. */
  private def toggleBlock(s: EditorState): TextBuffer =
    val vs = s.visualStart.getOrElse(s.buffer.cursor)
    val ve = s.buffer.cursor
    val r1 = vs.row.min(ve.row); val r2 = vs.row.max(ve.row)
    val (c1, c2) = (vs.col.min(ve.col), vs.col.max(ve.col))
    val newLines = s.buffer.lines.zipWithIndex.map { (ln, r) =>
      if r >= r1 && r <= r2 then
        val fc = c1.min(ln.length); val tc = c2.min(ln.length)
        val toggled = ln.text.substring(fc, tc).map { ch =>
          if ch.isLower then ch.toUpper else ch.toLower
        }
        TextLine(ln.text.substring(0, fc) + toggled + ln.text.substring(tc))
      else ln
    }
    s.buffer.copy(lines = newLines, modified = true,
      undoStack = s.buffer.undoStack :+ s.buffer, redoStack = Vector.empty)

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
      val parts = t.stripLineEnd.split("\n", -1).toVector
      if parts.length == 1 then
        val b = s.buffer.insertLineBelow
        s.copy(buffer = b.copy(
          lines = b.lines.updated(b.row, TextLine(parts.head)),
          cursor = CursorPos(b.row, parts.head.length)))
      else
        var buf = s.buffer; val col = buf.col
        for (line, i) <- parts.zipWithIndex do
          if i == 0 then
            buf = buf.insertLineBelow
            val r = buf.row
            val left  = buf.lines(r).text.substring(0, col.min(buf.lines(r).length))
            val right = buf.lines(r).text.substring(col.min(buf.lines(r).length))
            buf = buf.copy(lines = buf.lines.updated(r, TextLine(left + line + right)),
              cursor = CursorPos(r, col + line.length))
          else
            buf = buf.insertLineBelow
            val r = buf.row
            val pad = " " * (col - buf.lines(r).length.min(col))
            buf = buf.copy(lines = buf.lines.updated(r, TextLine(pad + line)),
              cursor = CursorPos(r, col + line.length))
        s.copy(buffer = buf)
    else s.copy(buffer = s.buffer.insertString(t))

  private def execPasteBefore(s: EditorState): EditorState =
    val t = s.yankRegister
    if t.isEmpty then s
    else if t.endsWith("\n") then
      val parts = t.stripLineEnd.split("\n", -1).toVector
      if parts.length == 1 then
        val b = s.buffer.insertLineAbove
        s.copy(buffer = b.copy(
          lines = b.lines.updated(b.row, TextLine(parts.head)),
          cursor = CursorPos(b.row, parts.head.length)))
      else
        var buf = s.buffer; val col = buf.col
        for (line, i) <- parts.zipWithIndex do
          buf = buf.insertLineAbove
          val r = buf.row
          val pad = " " * (col - buf.lines(r).length.min(col))
          buf = buf.copy(lines = buf.lines.updated(r, TextLine(pad + line)),
            cursor = CursorPos(r, col + line.length))
        s.copy(buffer = buf)
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
    buf.clear(frame.area())

    state.splits match
      case Some(grid) => renderSplits(frame, state, grid, w, edH)
      case None =>
        renderPane(buf, state, state.buffer, state.scrollTop,
          state.searchPattern, 0, 0, w, edH, lnW = 4, active = true,
          state.visualStart, state.mode, state.buffer.row)

    // status bar (always full width)
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
    val lt = s"$mi$fn$md"
    val rt = s"${state.buffer.row + 1},${state.buffer.col + 1}"
    buf.setString(0, h - 2, " " * w, stS)
    buf.setString(0, h - 2, lt.take(w / 2), stS)
    buf.setString((w - rt.length - 1).max(0), h - 2, rt, stS)

    // command line
    if state.mode == Command || state.mode == Search then
      buf.setString(0, h - 1, state.message.take(w),
        Style.EMPTY.bg(Color.YELLOW).fg(Color.BLACK))
    else buf.setString(0, h - 1, state.message.take(w), Style.EMPTY)

    // cursor in single-pane mode
    if state.splits.isEmpty then
      val cr = state.buffer.row - state.scrollTop
      val cc = state.buffer.col + 4
      if cr >= 0 && cr < edH then
        frame.setCursorPosition(cc.min(w - 1), cr)

  /** Render one pane. */
  private def renderPane(
    buf: Buffer, st: EditorState,
    buffer: TextBuffer, scrollTop: Int,
    searchPattern: String,
    offX: Int, offY: Int, pw: Int, ph: Int,
    lnW: Int, active: Boolean,
    visualStart: Option[CursorPos], mode: Mode, cursorRow: Int
  ): Unit =
    val (sL, eL) = (scrollTop, (scrollTop + ph).min(buffer.lineCount))
    for i <- sL until eL.min(buffer.lineCount) do
      val sr = i - sL + offY; val txt = buffer.line(i).text
      if sr - offY < ph then
        buf.setString(offX, sr,
          s"${i + 1}%${lnW - 1}s".format(" "),
          Style.EMPTY.fg(Color.GRAY).bold())
        val visible = txt.take((pw - lnW).max(0))
        buf.setString(offX + lnW, sr, visible, Style.EMPTY)
        // search highlight
        if searchPattern.nonEmpty then
          val hl = Style.EMPTY.bg(Color.YELLOW).fg(Color.BLACK).bold()
          var idx = txt.indexOf(searchPattern)
          while idx >= 0 do
            val sc = offX + idx + lnW
            if sc < offX + pw then
              buf.setString(sc, sr,
                searchPattern.take((pw + offX - sc).max(0)), hl)
            idx = txt.indexOf(searchPattern, idx + 1)
        // visual selection
        for vs <- visualStart do
          val ve = buffer.cursor
          val r1 = vs.row.min(ve.row); val r2 = vs.row.max(ve.row)
          if i >= r1 && i <= r2 then
            val (colFrom, colTo) =
              if mode == VisualBlock then
                (vs.col.min(ve.col), vs.col.max(ve.col))
              else
                (if i == r1 then vs.col else 0,
                 if i == r2 then ve.col else txt.length)
            val c1 = offX + colFrom + lnW
            val c2 = offX + colTo + lnW
            val sel = Style.EMPTY.bg(Color.MAGENTA)
            for c <- c1 until c2.min(offX + pw) do
              buf.setString(c, sr, " ", sel)
            val vis = txt.substring(colFrom.max(0).min(txt.length),
              colTo.min(txt.length))
            if vis.nonEmpty then buf.setString(c1.max(0), sr, vis, sel)
        // cursor
        if i == cursorRow && active then
          val curS = mode match
            case Insert => Style.EMPTY.bg(Color.GREEN)
            case VisualChar | VisualLine | VisualBlock =>
              Style.EMPTY.bg(Color.MAGENTA)
            case OperatorPending => Style.EMPTY.bg(Color.YELLOW)
            case _ => Style.EMPTY
          val cx = offX + buffer.col + lnW
          if cx < offX + pw then
            val ch = if buffer.col < txt.length then
              txt.charAt(buffer.col).toString else " "
            buf.setString(cx, sr, ch, curS)

  /** Render all split panes as a grid. */
  private def renderSplits(
    frame: Frame, state: EditorState, grid: SplitGrid, w: Int, edH: Int
  ): Unit =
    val buf = frame.buffer()
    val paneW = w / grid.cols
    val paneH = edH / grid.rows
    val lnW = 2

    for idx <- grid.panes.indices do
      val (r, c) = grid.paneRowCol(idx)
      val offX = c * paneW
      val offY = r * paneH
      val pane = grid.panes(idx)
      renderPane(buf, state, pane.buffer, pane.scrollTop,
        pane.searchPattern, offX, offY, paneW, paneH, lnW,
        active = idx == grid.active, None, state.mode, pane.buffer.row)
      // Draw separators
      if c < grid.cols - 1 then
        for y <- offY until (offY + paneH).min(edH) do
          buf.setString(offX + paneW - 1, y, "│", Style.EMPTY.fg(Color.GRAY))
      if r < grid.rows - 1 then
        for x <- offX until (offX + paneW).min(w) do
          buf.setString(x, offY + paneH - 1, "─", Style.EMPTY.fg(Color.GRAY))

    // Cursor in active pane
    val (ar, ac) = grid.paneRowCol(grid.active)
    val ap = grid.panes(grid.active)
    val cr = ap.buffer.row - ap.scrollTop + ar * paneH
    val cc = ap.buffer.col + lnW + ac * paneW
    if cr >= 0 && cr < edH then
      frame.setCursorPosition(cc.min(w - 1), cr)

// ═══════════════════════════════════════════════════════════════════
//  Application entry point
// ═══════════════════════════════════════════════════════════════════

@main def runVim(filename: String = ""): Unit =
  // Directly create the JLine backend to bypass ServiceLoader (needed for GraalVM native image)
  val provider = new dev.tamboui.backend.jline3.JLineBackendProvider()
  val backend = provider.create()

  val initialBuf = if filename.nonEmpty then
    val file = new java.io.File(filename)
    if file.exists then
      val text = scala.io.Source.fromFile(file).mkString
      val lines = text.split("\n", -1).map(l => TextLine(l)).toVector
      if lines.isEmpty then TextBuffer(filename = Some(filename))
      else TextBuffer(lines = lines, filename = Some(filename))
    else TextBuffer(filename = Some(filename))
  else TextBuffer()

  val config = dev.tamboui.tui.TuiConfig
    .builder()
    .backend(backend)
    .rawMode(true)
    .alternateScreen(true)
    .hideCursor(false)
    .build()

  val runner = dev.tamboui.tui.TuiRunner.create(config)
  try
    val app = new VimApp(initialBuf)
    runner.run(app, app)
  finally runner.close()

final class VimApp(initialBuffer: TextBuffer)
    extends dev.tamboui.tui.EventHandler
    with dev.tamboui.tui.Renderer:
  private var state: EditorState = EditorState(buffer = initialBuffer)

  def handle(
      event: dev.tamboui.tui.event.Event,
      runner: dev.tamboui.tui.TuiRunner
  ): Boolean =
    event match
      case ke: dev.tamboui.tui.event.KeyEvent =>
        if ke.isQuit then { runner.quit(); false }
        else
          // Sync from split panes, handle event, then sync back
          val synced = state.syncFromSplits
          val (ns, redraw) = InputHandler.handle(ke, synced)
          state = ns.copy(
            termWidth = state.termWidth,
            termHeight = state.termHeight
          ).syncToSplits
          // Handle file operations (impure side-effects in the app shell)
          if state.message == "Saved" || state.message == "Saved & quit" then
            saveFile()
          if state.message == "Saved & quit" || state.message == "Quit" then
            runner.quit()
          redraw
      case _ => false

  private def saveFile(): Unit =
    state.buffer.filename match
      case Some(path) =>
        try
          val writer = java.io.PrintWriter(java.io.File(path))
          try
            writer.print(state.buffer.serialize)
          finally
            writer.close()
          state = state.copy(buffer = state.buffer.copy(modified = false))
        catch case e: Exception =>
          state = state.copy(message = s"Can't save: ${e.getMessage}")
      case None =>
        state = state.copy(message = "No filename")

  def render(frame: dev.tamboui.terminal.Frame): Unit =
    state = state
      .copy(termWidth = frame.width(), termHeight = frame.height())
      .ensureCursorVisible
    VimRenderer.render(frame, state)
