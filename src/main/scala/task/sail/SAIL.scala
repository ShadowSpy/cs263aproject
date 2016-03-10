package task.sail

import java.io.File

import breeze.linalg.max
import breeze.numerics.abs
import epic.trees.AnnotatedLabel
import framework.fodor.{Feature, SimpleFeature}
import framework.fodor.graph._
import framework.igor.eval.EvalStats
import main.Config
import task._

import scala.collection.mutable
import scala.xml.XML

/**
 * @author jda
 */
object Sail extends TaskFactory {
  override def apply(dataRoot: File)(implicit config: Config): Task = new Sail(new File(dataRoot, "sail"), "en")
}

class Sail(root: File, language: String)(implicit config: Config) extends Task with Serializable {

  val transcriptDir = new File(root, "data")
  val mapDir = new File(root, "maps")
  val paragraphTranscriptFileName = if (language == "zh") "Paragraph_Mandarin.xml" else "Paragraph.xml"
  val sentenceTranscriptFileName = "SingleSentence.xml"

  final val Collapse = true

  val maps: Map[String,SailMap] = {
    mapDir.listFiles.map { mapFile =>
      val doc = XML.loadFile(mapFile)
      val mapName = (doc \@ "name").toLowerCase
      val nodeMap = (doc \ "nodes" \ "node").map { node =>
        val x = (node \@ "x").toInt
        val y = (node \@ "y").toInt
        val item = node \@ "item"
        val sailNode = SailNode(x, y, item)
        (x, y) -> sailNode
      }.toMap
      val edgeMap = (doc \ "edges" \ "edge").flatMap { edge =>
        val pos1 = (edge \@ "node1").split(",").map(_.toInt) match { case Array(x, y) => (x, y) }
        val pos2 = (edge \@ "node2").split(",").map(_.toInt) match { case Array(x, y) => (x, y) }
        val wall = edge \@ "wall"
        val floor = edge \@ "floor"
        val node1 = nodeMap(pos1)
        val node2 = nodeMap(pos2)
        val sailEdge = SailEdge(node1, node2, wall, floor)
        Seq(
          (pos1, pos2) -> sailEdge,
          (pos2, pos1) -> sailEdge
        )
      }.toMap
      mapName -> new SailMap(nodeMap, edgeMap)
    }.toMap
  }

  override def instances: IndexedSeq[Instance] = {
    val transcriptFile = new File(transcriptDir, sentenceTranscriptFileName)
    val doc = XML.loadFile(transcriptFile)
    (doc \ "example").map { example =>
      val mapName = (example \@ "map").toLowerCase
      val map = maps(mapName)
      val instruction = (example \ "instruction").text
      val sentences = instruction.split("""\.""").toSeq.map(_.trim.toLowerCase).filterNot(_.isEmpty).toIndexedSeq;
      val splitSentences = sentences.flatMap { sentence =>
        import framework.arbor.{parse,segmentWords}
        import framework.arbor.BerkeleyAnnotators.{parser,wordSegmenter}
        val words = segmentWords(sentence)
        val tree = parse(words)
        val vpCoordinators = tree.allChildren.flatMap { subtree =>
          val vpChildren = subtree.children.count(_.label.label == "VP")
          val ccChild = subtree.children.find(_.label.label == "CC")
          if (vpChildren == 2 && ccChild.isDefined && words(ccChild.get.begin) == "and") Some(ccChild)
          else None
        }.flatten
        if (vpCoordinators.nonEmpty) {
          val index = vpCoordinators.next().begin
          Seq(words.slice(0, index).mkString(" "), words.slice(index + 1, words.length).mkString(" "))
        } else {
          Some(sentence)
        }
      }
      val path = (example \ "path").text.trim.drop(1).dropRight(1)
      val sites = """\((\d+), (\d+), ([-\d]+)\)""".r.findAllIn(path)
      val posns = sites.matchData.map { mtch =>
        val x = mtch.group(1).toInt
        val y = mtch.group(2).toInt
        val rawOrientation = mtch.group(3).toInt
        val orientation = if (rawOrientation == -1) 0 else rawOrientation
        (x, y, orientation)
      }.toSeq
      val startState = SailState(map.nodes(posns.head._1, posns.head._2), posns.head._3, map)
      val states = posns.tail.foldLeft(startState :: Nil) { (prevStates, posn) =>
        val last = prevStates.head
        if (Collapse && prevStates.length >= 2 && prevStates.tail.head.orientation == last.orientation && last.orientation == posn._3) {
          val next = SailState(map.nodes(posn._1, posn._2), posn._3, map)
          next :: prevStates.tail
        } else {
          val next = SailState(map.nodes(posn._1, posn._2), posn._3, map)
          next :: prevStates
        }
      }.reverse.toIndexedSeq

      if (states.length == 1) {
        val state = states.head
        val act = VerifyAction(state)
        TaskInstance(IndexedSeq((state, act, state)), splitSentences)
      } else {
        val transitions = states.sliding(2).map { case IndexedSeq(s1, s2) =>
          val action: SailAction = if (s1.node.x == s2.node.x && s1.node.y == s2.node.y) {
            val rawAngle = s2.orientation - s1.orientation
            val angle = max(rawAngle, rawAngle + 360) % 360
            RotateAction(s1, angle)
          } else {
            assert { s1.orientation == s2.orientation }
            if (s1.node.x != s2.node.x && s1.node.y != s2.node.y) {
              println("!!! BAD PATH")
            }
            val dist = max(abs(s1.node.x - s2.node.x), abs(s1.node.y - s2.node.y))
            MoveAction(s1, dist)
          }
          (s1, action, s2)
        }.toIndexedSeq.map {
          case (s1, a: RotateAction, s2) if a.angle == 0 => {
            assert (s1.orientation == 0)
            val ns1 = s1.copy(orientation = 90)
            (ns1, RotateAction(ns1, 270), s2)
          }
          case triple => triple
        }


        val finalTransitions = transitions


        TaskInstance(finalTransitions, splitSentences)
      }
    }.toIndexedSeq
  }

  def idsAndMaps(fileName: String): IndexedSeq[(String,Int)] = {
    val transcriptFile = new File(transcriptDir, fileName)
    val doc = XML.loadFile(transcriptFile)
    (doc \ "example").zipWithIndex.map { case (example, id) =>
      val mapName = (example \@ "map").toLowerCase
      (mapName, id)
    }.toIndexedSeq
  }

  val trainIds0: IndexedSeq[Int] = idsAndMaps(sentenceTranscriptFileName).filter { case (name, id) => name == "jelly" || name == "grid" }.map(_._2)
  val testIds0: IndexedSeq[Int] = idsAndMaps(sentenceTranscriptFileName).filter { case (name, id) => name == "l" }.map(_._2)
  val trainIds1: IndexedSeq[Int] = idsAndMaps(sentenceTranscriptFileName).filter { case (name, id) => name == "grid" || name == "l" }.map(_._2)
  val testIds1: IndexedSeq[Int] = idsAndMaps(sentenceTranscriptFileName).filter { case (name, id) => name == "jelly" }.map(_._2)
  val trainIds2: IndexedSeq[Int] = idsAndMaps(sentenceTranscriptFileName).filter { case (name, id) => name == "l" || name == "jelly" }.map(_._2)
  val testIds2: IndexedSeq[Int] = idsAndMaps(sentenceTranscriptFileName).filter { case (name, id) => name == "grid" }.map(_._2)

  val allTrainIds = IndexedSeq(trainIds0, trainIds1, trainIds2)
  val allTestIds = IndexedSeq(testIds0, testIds1, testIds2)

  override val trainIds = allTrainIds(config.fold)
  override val testIds = allTestIds(config.fold)

  def stepDirection(posn: (Int, Int), orientation: Int, distance: Int): (Int, Int) = {
    orientation match {
      case 0 => (posn._1, posn._2 - distance)
      case 90 => (posn._1 + distance, posn._2)
      case 180 => (posn._1, posn._2 + distance)
      case 270 => (posn._1 - distance, posn._2)
    }
  }

  override def availableActions(state: State): Set[Action] = {
    val posn = (state.node.x, state.node.y)
    val actBuilder = Set.newBuilder[Action]

    if (Collapse) {
      var keepForward = true
      var last = posn
      var steps = 1
      while (keepForward) {
        val forward = stepDirection(last, state.orientation, 1)
        if (state.map.edges.contains(last, forward)) {
          actBuilder += MoveAction(state, steps)
          steps += 1
          last = forward
        } else {
          keepForward = false
        }
      }
    } else {
      val forward = stepDirection(posn, state.orientation, 1)
      if (state.map.edges.contains(posn, forward)) actBuilder += MoveAction(state, 1)
    }

    Set(90, 180, 270).foreach(angle => actBuilder += RotateAction(state, angle))

    actBuilder += VerifyAction(state)

    val r = actBuilder.result()
    r
  }

  override def doAction(state: State, action: Action): SailState = {
    action match {
      case a:MoveAction =>
        val posn = (state.node.x, state.node.y)
        val forward = stepDirection(posn, state.orientation, a.distance)
        SailState(state.map.nodes(forward), state.orientation, state.map)

      case a:RotateAction =>
        val newOrientation = (state.orientation + a.angle + 360) % 360
        SailState(state.node, newOrientation, state.map)

      case a:VerifyAction =>
        state.copy()
    }
  }

  override def visualize(pred: IndexedSeq[(State, Action, State)], gold: IndexedSeq[(State, Action, State)]): Unit = Unit

  override def score(pred: IndexedSeq[(State, Action, State)], gold: IndexedSeq[(State, Action, State)]): EvalStats = {
    // TODO FIXME
    if (gold.length == 0) return EvalStats(0, 1, 0)
    val predLast = pred.last._3.node
    val goldLast = gold.last._3.node
    val predLastDir = pred.last._3.orientation
    val goldLastDir = gold.last._3.orientation
    val correct = predLast == goldLast && predLastDir == goldLastDir
    val score = if (correct) 1 else 0
    EvalStats(score, 1-score, 0)
  }

  override type Action = SailAction
  override type State = SailState

  def represent2(s1: State, a: Action, s2: State): EventContext = {
    val eventFeats: Set[Feature] = a match {
      case MoveAction(startState, distance) => Set(SimpleFeature("action:move"), SimpleFeature("distance:" + distance))
      case RotateAction(startState, angle) => Set(SimpleFeature("action:rotate"), SimpleFeature("angle:" + angle))
      case VerifyAction(startState) => Set(SimpleFeature("action:verify"))
    }
    val relationBuilder = Set.newBuilder[Relation]
    val toNode = buildNode(s2.node, relationBuilder, "after", s2.map, s2.orientation)
    val allFeats = eventFeats | relationBuilder.result().flatMap { r => r.to.features | r.from.features }
    EventContext(Event(allFeats), GraphWorld(Set()))
  }

  override def represent(s1: State, a: Action, s2: State): EventContext = {
    val eventFeats: Set[Feature] = a match {
      case MoveAction(startState, distance) => Set(SimpleFeature("action:move"), SimpleFeature("distance:" + distance))
      case RotateAction(startState, angle) => Set(SimpleFeature("action:rotate"), SimpleFeature("angle:" + angle))
      case VerifyAction(startState) => Set(SimpleFeature("action:verify"))
    }
    val relationBuilder = Set.newBuilder[Relation]
    val event = Event(eventFeats)

    val toNode = buildNode(s2.node, relationBuilder, "after", s2.map, s2.orientation)

    relationBuilder += Relation(event, toNode, Set(SimpleFeature("after"))) // to")))
    if (s1.node != s2.node) {
      try {

      } catch {
        case e: NoSuchElementException =>
      }
    }
    EventContext(event, GraphWorld(relationBuilder.result()))
  }

  def buildNode(node: SailNode, relationBuilder: mutable.Builder[Relation,Set[Relation]], position: String, map: SailMap, orientation: Int, depth: Int = 1): Entity = {

    // TODO there is a better way
    val (leftX, leftY) = orientation match {
      case 0 => (-1, 0)
      case 90 => (0, -1)
      case 180 => (1, 0)
      case 270 => (0, 1)
    }
    val (frontX, frontY) = orientation match {
      case 0 => (0, -1)
      case 90 => (1, 0)
      case 180 => (0, 1)
      case 270 => (-1, 0)
    }
    val (rightX, rightY) = orientation match {
      case 0 => (1, 0)
      case 90 => (0, 1)
      case 180 => (-1, 0)
      case 270 => (0, -1)
    }
    val (backX, backY) = orientation match {
      case 0 => (0, 1)
      case 90 => (-1, 0)
      case 180 => (0, -1)
      case 270 => (1, 0)
    }

    val left = map.nodes.get(node.x + leftX, node.y + leftY)
    val front = map.nodes.get(node.x + frontX, node.y + frontY)
    val right = map.nodes.get(node.x + rightX, node.y + rightY)
    val back = map.nodes.get(node.x + backX, node.y + backY)

    val nodes = Seq(left, front, right, back)
    val names = Seq("left", "front", "right", "back")
    val edges = nodes map {
      case None => None
      case Some(nNode) => map.edges.get((node.x, node.y), (nNode.x, nNode.y))
    }

    val itemFeatsHere =
      if (!node.item.isEmpty) Set[Feature](SimpleFeature("item=" + node.item)) // SimpleFeature("has-item"))
      else Set[Feature]()
    val nNeighbors = nodes.flatten.length
    val neighborFeatsHere = // Set[Feature]() // Set[Feature](SimpleFeature("neighbors=" + nNeighbors))
          Set()
    val gHere = Entity(itemFeatsHere ++ neighborFeatsHere) // + SimpleFeature("pos=here")) // + SimpleFeature("step=" + position))

    val gNeighbors = nodes zip edges zip names map {
      case ((_, None), name) =>
        val gNeighbor = Entity(Set(SimpleFeature("wall"))) // ,  SimpleFeature("pos=here")))
//        val gNeighbor = Entity(Set(SimpleFeature("wall")))
        relationBuilder += Relation(gHere, gNeighbor, Set(SimpleFeature("edge"))) // name)))
      case ((Some(nNode), Some(nEdge)), name) =>
        val hallFeats = Set[Feature](SimpleFeature("wall=" + nEdge.wall),
                                     SimpleFeature("floor=" + nEdge.floor))
        val itemFeats =
          if (!nNode.item.isEmpty) Set[Feature](SimpleFeature("item=" + nNode.item))//Set[Feature](SimpleFeature("has-item"), SimpleFeature("item=" + nNode.item))
          else Set[Feature]()
        val gNeighbor = Entity(hallFeats ++ itemFeats + SimpleFeature("pos=" + name))
        relationBuilder += Relation(gHere, gNeighbor, Set(SimpleFeature("edge"))) // name)))
    }

    gHere



  }

}
sealed trait SailAction extends TaskAction
case class MoveAction(startState: SailState, distance: Int) extends SailAction {
}
case class RotateAction(startState: SailState, angle: Int) extends SailAction {
}

case class VerifyAction(startState: SailState) extends SailAction

case class SailState(node: SailNode, orientation: Int, map: SailMap) extends TaskState {
}

case class SailNode(x: Int, y: Int, item: String)
case class SailEdge(node1: SailNode, node2: SailNode, wall: String, floor: String) {
  def nodeAfter(node: SailNode): SailNode = {
    require { node == node1 || node == node2 }
    if (node == node1) node2 else node1
  }
}

class SailMap(val nodes: Map[(Int,Int),SailNode], val edges: Map[((Int,Int),(Int,Int)),SailEdge]) extends Serializable {
  def neighbors(node: SailNode): Seq[SailNode] = {
    val positions = Seq((node.x+1,node.y),(node.x-1,node.y),(node.x,node.y+1),(node.x,node.y-1))
    positions.flatMap(nodes.get)
  }
}
