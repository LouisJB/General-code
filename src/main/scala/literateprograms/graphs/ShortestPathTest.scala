/* Copyright (c) 2010 the authors listed at the following URL, and/or
the authors of referenced articles or incorporated external code:
http://en.literateprograms.org/Dijkstra's_algorithm_(Scala)?action=history&offset=20080416205621

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Retrieved from: http://en.literateprograms.org/Dijkstra's_algorithm_(Scala)?oldid=13109
*/

package literateprograms.graphs

import scala.collection.mutable.{HashSet, HashMap}
import scala.xml.XML

object ShortestPathTest {

  class MyException( msg: String) extends java.lang.RuntimeException( msg)

  
  class WeightedDiGraph[V]() { 

    val arcs = new HashMap[V, HashMap[ V, Float]]
    val vertices = new HashSet[V]
    
    def adjacents( u: V) = arcs( u).keys
    def cost( u: V, v: V) = arcs(u)(v)

    def addArc( from: V, to: V, kost: Float) = {
      
      if (arcs.contains(from)) {
          val adjMap = arcs( from)
          adjMap += to -> kost
      }
      else {
        val adjMap = new HashMap[ V, Float]
        adjMap += to -> kost ;
        arcs += from -> adjMap
      }
      
      vertices += from ;
      vertices += to 
    }
     
    /**
    *  @param start
    *  @param end
    *  @return  route as list of pairs( node, shortest_distance), not including start
    *           Nil if end not reached ( unconnected graph)
    */

    def shortestPath( start: V, end: V): List[Pair[V, Float]] = {

      if (!arcs.contains( start)) 
        throw new MyException( "shortestPath: "+start+" not in graph edge origins") ;
     
      if (!vertices.contains( end)) 
        throw new MyException( "shortestPath: "+end+" not in graph vertices") ;

      // get shortest-distances, predecessors, end result
      val Triple( dist, pred, endReached) = dijkstra( start, end) ;
      
      // build path from end based on predecessors

      var path: List[Pair[V, Float]] = Nil ;

      if (endReached) {

        var v = end

        while ( v != start) {
          path = Pair(v, dist(v)) :: path ;

          // iterate on predecessor      
          v = pred( v)
        }
      }
      
      // return path
      path
    }


    /**
    *  @param start
    *  @param end  (end vertex or null)
    *  @return  Triple( distances, predecessors, endReached)
    */

    def dijkstra( source: V, end: V) = {
      assume( arcs.contains( source), "source not in arcs origins")
      if (end != null) assume( vertices.contains( end), "end not in graph")
        
      // initialize
      val dist = new HashMap[V, Float] ;  // distances
      val Q = new HashSet[V] ;            // priority queue
      val Settled = new HashSet[V] ; // settled vertices
      val pred = new HashMap[V, V] ;    // predecessors
     
      
      def minimumDistVertex( Q: HashSet[V]): V = {

        assume( ! Q.isEmpty && Q.iterator.forall( dist.isDefinedAt )) ;
                 
        val iterator = Q.iterator ;
        val w = iterator.next ;          // first element, because Q is not empty

        // calculate and return
        iterator.foldLeft( w) {(u, v) => if (dist( u) <= dist( v)) u else v}
      }

      
      // start with source vertex

      dist += source -> 0 ;
      Q += source ;

      var endReached = false ;
      
      while (! Q.isEmpty && ! endReached) {
     
        // extract minimumDistVertex from Q, add to Settled ones

        val u = minimumDistVertex( Q) ;
        Q -= u ;
        Settled += u ;
     
        if (end != null) endReached = (u == end) ;
        
        // update neighbours distances
        // and add updated ones to Q
        
        if (! endReached)
          for (v <- adjacents(u) if !Settled.contains(v)) {
            
            val vNewDist = dist( u) + cost(u, v) ;

            if ( ! dist.isDefinedAt( v) || vNewDist < dist(v)) {
              
              dist += v -> vNewDist ;
              pred += v -> u ;
              Q += v ;
            }
          }
      }
      
      // return distances, predecessors, endReached
      Triple( dist, pred, endReached)
    }

  }

  
  def main( args: Array[String]) = {
  
      val roadMap = new WeightedDiGraph[String]
                                                   
      val test_data = XML.loadFile( "test_data.xml") ;
      
      for( arc <- test_data \ "graph" \ "arc") {

        val from = arc.attribute("from") ;
        val to = arc.attribute("to") ;
        val cost = java.lang.Float.parseFloat( arc.attribute("cost").get.toString) ;

      roadMap.addArc(from.get.toString, to.get.toString, cost) ;
        roadMap.addArc( to.get.toString, from.get.toString, cost) ;
      }
  
      // for each test entry in test_data
    
      for( test <- test_data \ "sources" \ "test") {
        
        val source = test.attribute("source") ;
        val dest = test.attribute("dest") ;
        
        Console.println("from " + source + "\n") ;

        try {
          val route = roadMap.shortestPath(source.get.toString, dest.get.toString) ;

          if (route == Nil) {
            Console.println( "No route to " + dest)
          }
          else for( Pair( city, distance) <- route) {
            Console.println( city + ": " + distance)
          }
        }
        catch {
          case m: MyException => Console.println( "MyException: "+ m.getMessage)
          case e: Throwable => Console.println( e.getMessage) ;
        }
          
        Console.println( "--") ;  
      }
  }
}

