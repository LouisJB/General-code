package xml

import scala.xml.{Node, XML, Text, Elem}
import scala.xml.transform


object XmlUtils {

  def main(args : Array[String]) {
    val input   = <foo><bar>test</bar></foo>
    val output  = topDown({ case <bar>{_*}</bar> => Text("hello, world!") }, input)
    println(output)
  }

  def bottomUp(change:PartialFunction[Node,Seq[Node]], node:Node):Seq[Node] =
    transform(change, node match {
      case Elem(prefix, label, attributes, scope, children@_*)
              => Elem(prefix, label, attributes, scope, (children flatMap { it => bottomUp(change, it) }):_*)
      case x  => x
    })

  def topDown(change:PartialFunction[Node,Seq[Node]], node:Node):Seq[Node] =
    transform(change, node) flatMap {
      case Elem(prefix, label, attributes, scope, children@_*)
              => Elem(prefix, label, attributes, scope, (children flatMap { it => topDown(change, it) }):_*)
      case x  => x
    }

  def transform(change:PartialFunction[Node,Seq[Node]], nodes:Seq[Node]) = nodes flatMap {
    case x if change isDefinedAt x  => change(x)
    case x                          => x
  }
}
