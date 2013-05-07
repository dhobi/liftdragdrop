package code.util


import net.liftweb.http.{SHtml, S}
import net.liftweb.util.Helpers
import net.liftweb.http.js.{JsCmds, JsCmd}
import scala.xml._
import scala.Some
import net.liftweb.http.js.JE.JsRaw

/**
 * DragDrop widget components
 */


class DragDrop[T, V](val draggables: List[Draggable[T]], val droppables: List[Droppable[T, V]]) {

  def draggableJs: List[JsCmd] = draggables.map(_.jsCmd)

  def draggableHtml = draggables.map(_.html)

  def droppableJs: List[JsCmd] = droppables.map(_.jsCmd(draggables))

  def droppableHtml = droppables.map(_.html)
}

abstract class Draggable[T] {

  import DraggableOptions._

  val draggableClass = "liftdraggable"

  val defaultDraggableOptions = List(
    cursor("pointer"),
    zIndex(1000),
    ghosting(false),
    revert(true),
    opacity(0.7)
  )

  lazy val id = Helpers.nextFuncName

  def types: List[String]

  def value: String

  def wrappedObject: T

  def jsCmd: JsCmd = jsCmd(defaultDraggableOptions)

  def jsCmd(options: List[DragDroppableOption[_]]): JsCmd = JsRaw( """
    $('#""" + id + """').draggable({
                   			""" + options.map(option => option.key + ": " + option.value).mkString(", ") + """
		})""").cmd

  def html = addChild(elem, Text(value)) %
    new UnprefixedAttribute("class", draggableClass + " " + types.mkString(" "), Null) %
    new UnprefixedAttribute("id", id, Null)

  private def addChild(n: Elem, newChild: Node) = n match {
    case Elem(prefix, label, attribs, scope, child @ _*) => Elem(prefix, label, attribs, scope, child ++ newChild : _*)
    case _ => n
  }

  def elem = <div></div>
}

abstract class Droppable[T, V] {

  import DroppableOptions._

  val defaultDroppableOptions = List(
    tolerance("pointer"),
    activeClass("ui-state-highlight"),
    hoverClass("ui-state-highlight-over")
  )

  val droppableClass = "liftdroppable"

  lazy val id = Helpers.nextFuncName

  def value: String

  def acceptType: String

  def onDrop: (Draggable[T]) => JsCmd

  def wrappedObject: V

  def jsCmd(draggables: List[Draggable[T]]): JsCmd = jsCmd(draggables, defaultDroppableOptions)

  def dropHandler(draggables: List[Draggable[T]]): JsCmd = {
    val func = {
      (str: String) => {
        draggables.find(draggable => draggable.id.equals(str)) match {
          case Some(draggable) => onDrop(draggable)
          case _ => () //did not match anything
        }
      }
    }

    S.fmapFunc(S.SFuncHolder(func)) {
      funcName => SHtml.makeAjaxCall(JsRaw("'" + funcName + "=' + encodeURIComponent($(ui.draggable).attr(\"id\"))")).cmd
    }
  }

  def createHandler(draggables: List[Draggable[T]]): JsCmd = JsCmds.Noop

  def activateHandler(draggables: List[Draggable[T]]): JsCmd = JsCmds.Noop

  def deactivateHandler(draggables: List[Draggable[T]]): JsCmd = JsCmds.Noop

  def overHandler(draggables: List[Draggable[T]]): JsCmd = JsCmds.Noop

  def outHandler(draggables: List[Draggable[T]]): JsCmd = JsCmds.Noop

  def jsCmd(draggables: List[Draggable[T]], options: List[DragDroppableOption[_]]): JsCmd = JsRaw( """
    $('#""" + id + """').droppable({
				accept: '""" + acceptType + """',
                                    				""" + options.map(option => option.key + ": " + option.value).mkString(", ") + """,
				create: function(event, ui) {""" + createHandler(draggables).toJsCmd + """},
				activate: function(event, ui) {""" + activateHandler(draggables).toJsCmd + """},
				deactivate: function(event, ui) {""" + deactivateHandler(draggables).toJsCmd + """},
				over: function(event, ui) {""" + overHandler(draggables).toJsCmd + """},
				out: function(event, ui) {""" + outHandler(draggables).toJsCmd + """},
				drop:	function (event, ui) {""" + dropHandler(draggables).toJsCmd + """}
			})""").cmd

  private def addChild(n: Elem, newChild: Node) = n match {
    case Elem(prefix, label, attribs, scope, child @ _*) => Elem(prefix, label, attribs, scope, child ++ newChild : _*)
    case _ => n
  }

  def html = addChild(elem, Text(value)) %
    new UnprefixedAttribute("class", droppableClass + " " + acceptType.replace(".", ""), Null) %
    new UnprefixedAttribute("id", id, Null)

  def elem = <div></div>
}

case class DragDroppableOption[T: ClassManifest](key: String, v: T) {
  def value = classManifest[T] match {
    case ClassManifest.Int => String.valueOf(v.asInstanceOf[Int])
    case ClassManifest.Double => String.valueOf(v.asInstanceOf[Double])
    case ClassManifest.Boolean => String.valueOf(v.asInstanceOf[Boolean])
    case ClassManifest.Float => String.valueOf(v.asInstanceOf[Float])
    case _ => "'" + v.toString + "'"
  }
}

object DraggableOptions {
  def cursor(cursorType: String) = DragDroppableOption[String]("cursor", cursorType)

  def zIndex(index: Int) = DragDroppableOption[Int]("zIndex", index)

  def ghosting(bool: Boolean) = new DragDroppableOption[Boolean]("ghosting", bool)

  def revert(bool: Boolean) = new DragDroppableOption[Boolean]("revert", bool)

  def opacity(opacity: Double) = new DragDroppableOption[Double]("opacity", opacity)
}

object DroppableOptions {
  def tolerance(opacity: String) = new DragDroppableOption[String]("opacity", opacity)

  def activeClass(activeClass: String) = new DragDroppableOption[String]("activeClass", activeClass)

  def hoverClass(hoverClass: String) = new DragDroppableOption[String]("hoverClass", hoverClass)
}

