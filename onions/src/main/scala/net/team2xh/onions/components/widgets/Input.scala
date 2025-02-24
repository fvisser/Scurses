package net.team2xh.onions.components.widgets

import net.team2xh.onions.Symbols
import net.team2xh.onions.Themes.ColorScheme
import net.team2xh.onions.components.{FramePanel, Widget}
import net.team2xh.onions.utils.{Drawing, Varying}
import net.team2xh.scurses.{Keys, Scurses}

final case class Input(parent: FramePanel, defaultText: String = "Input")(implicit screen: Scurses)
    extends Widget(parent) {

  var text: Varying[String] = ""
  def cursorIndex           = text.value.length

  override def redraw(focus: Boolean, theme: ColorScheme): Unit = {
    val cursorSymbol = if focus then Symbols.BLOCK else " "
    val limit        = innerWidth - 3
    val t            = if cursorIndex == 0 && !focus then "<" + defaultText + ">" else text.value
    val fg           = if cursorIndex == 0 && !focus then theme.background else theme.foreground(focus)
    val l            = t.length
    val clippedText  = Drawing.clipText(t, limit, before = true)
    screen.put(0,
               0,
               " " + clippedText + cursorSymbol + " " * (innerWidth - l - 3) + " ",
               foreground = fg,
               background = if focus then theme.background(focus) else theme.accent1
    )
  }

  override def handleKeypress(keypress: Int): Unit = {
    keypress match {
      case Keys.BACKSPACE =>
        if cursorIndex > 0 then
          text := text.value.init
      case _ => text := text.value + keypress.toChar
    }
    needsRedraw = true
  }

  override def focusable: Boolean = true
  override def innerHeight: Int   = 1
}
