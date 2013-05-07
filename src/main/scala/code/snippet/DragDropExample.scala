package code.snippet

import code.util.{Droppable, Draggable, DragDrop}
import code.model._
import net.liftweb.http.js.JsCmds
import scala.xml.NodeSeq

class DragDropExample {

  private def createDraggablesFrom(fruits: List[Fruit]) = fruits.map(fruit => {
    new Draggable[Fruit] {
      def types = List("")

      def value = fruit.description

      def wrappedObject = fruit
    }
  })

  private def createDroppablesFrom(animals: List[Animal]) = animals.map(animal => {
    new Droppable[Fruit, Animal] {
      def value = animal.description

      def acceptType = "*"

      def onDrop = (draggable => {
        JsCmds.Alert("Me: " + animal.description + " has got a: " + draggable.wrappedObject.description)
      })

      def wrappedObject = animal
    }
  })

  def render = {
    val fruits = new Banana :: new Strawberry :: Nil
    val animals = new Dog :: new Cat :: Nil

    val draggables = createDraggablesFrom(fruits)
    val droppables = createDroppablesFrom(animals)

    val dragDrop = new DragDrop[Fruit, Animal](draggables, droppables)
    val js = (dragDrop.draggableJs ::: dragDrop.droppableJs).foldLeft(JsCmds.Noop)(_ & _)

    (ns: NodeSeq) => (<head>
      <script src="//ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"></script>
      <script src="//ajax.googleapis.com/ajax/libs/jqueryui/1.8.15/jquery-ui.js"></script>
      <script>
        {JsCmds.OnLoad(js).toJsCmd}
      </script>
    </head> ++
      dragDrop.draggableHtml ++
        <hr/> ++
      dragDrop.droppableHtml)

  }
}
