import scala.collection.immutable._

class Point(val x: Long, val y: Long)

class Edge(val start: Point, val end: Point) {
  val a = start.y - end.y
  val b = end.x - start.x
  val c = start.x * end.y - end.x * start.y

  lazy val isHorizontal: Boolean = start.y == end.y
  def containsPoint(p: Point): Boolean =
    a * p.x + b * p.y + c == 0 && boundsPointVertically(p) && boundsPointHorizontally(p)
  def boundsPointVertically(p: Point): Boolean =
    p.y <= start.y && p.y >= end.y || p.y <= end.y && p.y >= start.y
  def boundsPointHorizontally(p: Point): Boolean =
    p.x <= start.x && p.x >= end.x || p.x <= end.x && p.x >= start.x
}

object Main {

  def main(args: Array[String]): Unit = {
    try {
      val input = getInput()
      input._2 map (p =>
        if (polyContainsPoint(listToPoly(input._1))(p)) println("yes")
        else println("no"))
    } catch {
      case e: Exception => println("Error occurred. Details: " + e.getMessage)
    }
  }

  def getInput(): (List[Point], List[Point]) = {
    def strToPoint(str: String): Point = {
      val trimmed = str.replaceAll(" +", "").tail.init
      val x = trimmed.takeWhile(_ != ',').toLong
      val y = trimmed.dropWhile(_ != ',').tail.toLong
      new Point(x, y)
    }


    val linesNum = scala.io.Source.stdin.bufferedReader().readLine().toLong
    val rawPoly = for { i <- 0L until linesNum } yield scala.io.Source.stdin.bufferedReader().readLine()
    val pointsNum = scala.io.Source.stdin.bufferedReader().readLine().toLong
    val rawPoints = for { i <- 0L until pointsNum } yield scala.io.Source.stdin.bufferedReader().readLine()
    (rawPoly.toList.map(strToPoint), rawPoints.toList.map(strToPoint))
  }

  type NeighbEdge = (Edge, Edge, Edge)

  def listToPoly(lst: List[Point]): List[NeighbEdge] = {
    def toEdgesWithNeighbours(edges: List[Edge]): List[NeighbEdge] =
      (shiftRight(edges), edges, shiftLeft(edges)).zipped.toList

    def shiftLeft(edges: List[Edge]): List[Edge] = edges.tail :+ edges.head
    def shiftRight(edges: List[Edge]): List[Edge] = edges.last :: edges.init

    def removeHorizontalChains(edges: List[Edge]): List[Edge] = {
      def removeHorizontalChainsPartly(edges: List[Edge]): List[Edge] = {
        edges match {
          case (e1 :: e2 :: es) =>
            if(e1.isHorizontal && e2.isHorizontal)
              removeHorizontalChainsPartly(new Edge(e1.start, e2.end) :: es)
            else e1 :: removeHorizontalChainsPartly(e2 :: es)
          case (e :: Nil) =>  e :: Nil
          case Nil        => Nil
        }
      }
      if(edges.length < 2) edges
      else removeHorizontalChainsPartly(shiftLeft(removeHorizontalChainsPartly(edges)))
    }

    def toEdgesList(vertices: List[Point]): List[Edge] = {
      def toEdgesListImpl(verts: List[Point], fst: Point): List[Edge] =
        verts match {
          case (x :: y :: xs) => new Edge(x, y) :: toEdgesListImpl(y :: xs, fst)
          case (x :: xs)      => new Edge(x, fst) :: Nil
          case Nil            => throw new Exception("Empty vertices list")
        }
      if(vertices.length < 3) throw new Exception("At least 3 vertices needed")
      else toEdgesListImpl(vertices, vertices.head)
    }

    toEdgesWithNeighbours(removeHorizontalChains(toEdgesList(lst)))
  }

  def polyContainsPoint(edges: List[NeighbEdge])(p: Point): Boolean =
    polyBoundContainsPoint(edges)(p) || (iterateEdges(edges)(intersects(p)) % 2 != 0)

  def polyBoundContainsPoint(edges: List[NeighbEdge])(p: Point): Boolean =
    edges.foldRight(false)((es, b) => es._2.containsPoint(p) || b)

  type CheckerFun = (Edge, NeighbEdge, Edge) => Long

  def iterateEdges(edges: List[NeighbEdge])(f: CheckerFun): Long = {
    def itEdgesImpl(edges: List[NeighbEdge], fst: NeighbEdge, last: NeighbEdge): Long = {
      edges match {
        case (p1 :: p2 :: p3 :: rest) =>
          if(p1 == fst)
            f(last._1, p1, p2._3) + f(p1._1, p2, p3._3) + itEdgesImpl(p2 :: p3 :: rest, fst, last)
          else
            f(p1._1, p2, p3._3) + itEdgesImpl(p2 :: p3 :: rest, fst, last)
        case (p1 :: p2 :: Nil) => f(p1._1, p2, fst._3)
        case _                 => throw new Exception("Strange situation: itEdgesImpl is called with < 3 edges")
      }
    }
    if(edges.length < 3) throw new Exception("At least 3 edges are expected")
    else                 itEdgesImpl(edges, edges.head, edges.last)
  }

  def intersects(p0: Point)(prevPrev: Edge, cur: NeighbEdge, nextNext: Edge): Long = {
    if(cur._2.containsPoint(p0)) 1
    else if(cur._2.boundsPointVertically(p0))
      if(cur._2.isHorizontal)
        if(pointsOnTheSameSideVertically(cur._2.start)(cur._1.start, cur._3.end)) 0
        else 1
      else if(edgePointTriangleIsClockwise(cur._2.start, cur._2.end)(p0))
        if(p0.y == cur._2.end.y) {
          val nextNonSameLine = if(cur._3.isHorizontal) nextNext.end else cur._3.end
          if(pointsOnTheSameSideVertically(cur._2.end)(cur._2.start, nextNonSameLine)) 0
          else 1
        }
        else if(p0.y == cur._2.start.y) 0
        else 1
      else 0
    else 0
  }

  /**
   * Assumes that any 2 points can't lie on the same line
   */
  def pointsOnTheSameSideVertically(startPoint: Point)(p1: Point, p2: Point): Boolean =
    p1.y > startPoint.y && p2.y > startPoint.y || p1.y < startPoint.y && p2.y < startPoint.y

  def edgePointTriangleIsClockwise(e1: Point, e2: Point)(p: Point): Boolean =
    if(e1.y < e2.y) triangleIsClockwise(e1, p, e2)
    else              triangleIsClockwise(e2, p, e1)

  def triangleIsClockwise(p1: Point, p2: Point, p3: Point): Boolean =
    p1.x * p2.y + p2.x * p3.y + p1.y * p3.x - (p2.y * p3.x + p1.x * p3.y + p1.y * p2.x) < 0
}