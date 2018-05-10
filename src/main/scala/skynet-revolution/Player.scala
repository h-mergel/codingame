import scala.io.StdIn._

/**
  * @author Heinrich Mergel
  **/
object Player extends App {
    // n: the total number of nodes in the level, including the gateways
    // l: the number of links
    // e: the number of exit gateways
    val Array(n, l, e) = for(i <- readLine split " ") yield i.toInt

    val edges = List.fill(l)(readLine split " ").map{case Array(a, b) => new Edge(a.toInt, b.toInt)}
    val exits = List.fill(e)(readInt)

    while(true) {
        val visitedNodes = new Array[Boolean](n)
        def getEdges(node:Int): Stream[Edge] = {
            visitedNodes(node) = true
            edges.filter(e => e.hasNode(node) && !visitedNodes(e.otherNode(node)))
              .map(e => Edge(node, e.otherNode(node)))
              .toStream
        }

        def bfs(edges:Stream[Edge]):Stream[Edge]  = {
            if(edges.isEmpty) Stream.empty
            else edges.head #:: bfs(edges.tail.append(getEdges(edges.head.b)))
        }

        val res = bfs(getEdges(readInt())).find(e => exits.contains(e.b)).get
        println(res.a + " " + res.b)
    }
}

case class Edge ( val a:Int, val b:Int) {
    def hasNode(node:Int): Boolean = a == node || b == node
    def otherNode(node:Int): Int = if (a == node) b else a
}