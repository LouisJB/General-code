package xml

import java.io.File
import java.io.PrintWriter
import java.io.Writer

import scala.xml.{Node, XML}


object XmlTest {

  def main(args : Array[String]) {

    val rootXML = rootXml
    val subXML = subelementXml


    val xml = XmlUtils.topDown( { case <container>{_*}</container> => subXML }, rootXML)
    println(xml)

    val element = rootXml \\ "container" // findElement(rootXML)

    /*
    def add(p : Node, newEntry : Node ) : Node = p match {
       case <container>{ ch @ _* }</container> =>
         <container>{ ch }{ newEntry }</container>
    }
    */
   
    val rootXML2 = rootXML.copy(child = subXML +: rootXML.child)

   // val rootXML2 = add(rootXML, subXML)

/*
    val writer = new PrintWriter(System.out)

    val elem = rootXml match {

      case <subelement> _ </subelement> => true
      case _ => false
    }

    println(elem.toString)
*/

    println("XML = " + rootXML.toString)

    println("XML2 = " + rootXML2.toString)
    //xml.write(writer)
  }

  def findElement(xml : Node) = xml match {

    case <subelement> _ </subelement> => true
    case _ => false
  }

  def rootXml() = {

    val file = new File("src/templates/root-template.xml");
    val path = file.toURI().toString;
    XML.loadFile(file)
  }

  def subelementXml() = {

    val file = new File("src/templates/subelement-template.xml");
    val path = file.toURI().toString;
    XML.loadFile(file)
  }
}
