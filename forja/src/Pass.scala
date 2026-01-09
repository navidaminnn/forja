package forja

import forja.Wf.TokenWf
import forja.util.ReflectiveEnumeration

import scala.annotation.tailrec
import scala.collection.mutable
import scala.compiletime.{asMatchable, deferred}
import scala.concurrent.ExecutionContext

import Pass.*

trait Pass extends ReflectiveEnumeration.Enumerable:
  final def perform(root: Node)(using tracer: PassTracer = PassTracer.Nop): Node =
    if root.containsError
    then root
    else performImpl(root)
  end perform

  protected def performImpl(root: Node)(using tracer: PassTracer): Node
end Pass

object Pass:
  trait RewritePass extends Pass, ReflectiveEnumeration:
    private lazy val rewritesAgg: Pattern[String] =
      valuesByType[Query.rewrite[?]].view
        .map: (fieldName, rw) =>
          rw.pattern.map(_ => fieldName)
        .reduceOption(_ | _)
        .getOrElse(Pattern.empty)
    end rewritesAgg

    final protected def performImpl(node: Node)(using tracer: PassTracer): Node =
      tracer.beforePass(this.getClass.getSimpleName, node)
      
      @tailrec
      def impl(
          nodeSpan: NodeSpan,
          madeChangesThisIteration: Boolean,
      ): NodeSpan =
        rewritesAgg.runPattern(nodeSpan) match
          case None =>
            assert(nodeSpan.isEmpty)
            def firstChild = nodeSpan
              .expandRightOption(1)
              .map(_.head.children.asEmptyNodeSpan)
            end firstChild

            def firstAvailableParentsSiblingOrRoot(
                nodeSpan: NodeSpan,
            ): NodeSpan =
              nodeSpan.parentOption match
                case None         => nodeSpan
                case Some(parent) =>
                  parent.rightSiblingOption match
                    case None =>
                      firstAvailableParentsSiblingOrRoot(
                        parent.emptyNodeSpanHere,
                      )
                    case Some(sibling) => sibling.emptyNodeSpanHere
            end firstAvailableParentsSiblingOrRoot

            val nextSpan =
              firstChild.getOrElse(firstAvailableParentsSiblingOrRoot(nodeSpan))
            if nextSpan.parentOption.isEmpty
            then
              if madeChangesThisIteration
              then impl(nextSpan, madeChangesThisIteration = false)
              else nextSpan
            else impl(nextSpan, madeChangesThisIteration)
          case Some((ruleName, nodeSpan)) =>
            tracer.onRewriteMatch(ruleName, nodeSpan)
            val resultSpan = nodeSpan.take(0)
            tracer.onRewriteComplete(ruleName, resultSpan)
            impl(resultSpan, madeChangesThisIteration = true)
      end impl

      val nodeSpan =
        impl(node.asOrphan.emptyNodeSpanHere, madeChangesThisIteration = false)
      val replacementNode =
        nodeSpan.headOption
          .getOrElse(nodeSpan.expandRightOption(1).get.head)
      // Put our parents back
      val result = node.replaceThis(replacementNode)
      tracer.afterPass(this.getClass.getSimpleName, result)
      result
    end performImpl
  end RewritePass

  object RewritePass:
    trait EmbedGenerator[T <: Matchable]
        extends ReflectiveEnumeration.Enumerable:
      given embed: Node.Embed[T] = deferred
      def generate: Iterator[T]
    end EmbedGenerator

    final case class MCState(fieldName: String, node: Node, passNum: Int):
      override def toString(): String =
        s"$fieldName\n${node.toString()}"
      override def hashCode(): Int = node.hashCode()
      override def equals(that: Any): Boolean =
        that.asMatchable match
          case that: MCState =>
            node.equals(that.node)
          case _ => false
        end match
      end equals
    end MCState

    trait ModelChecker extends forja.ModelChecker, ReflectiveEnumeration:
      def inputWf: TokenWf
      def outputWf: TokenWf

      type State = MCState
      type ErrorState = Node

      extension (state: MCState)
        final def checkErrorState: Option[ErrorState] =
          if !state.node.containsError
          then
            val errorState = outputWf.validate
              .perform(state.node)
            if errorState.containsError
            then Some(errorState)
            else None
          else None
        end checkErrorState
      end extension

      def initTreeLevels: Int
      def initRepMax: Int

      private val embedGenerators: Map[Node.Embed[
        ?,
      ], (name: String, gen: RewritePass.EmbedGenerator[?])] =
        valuesByType[RewritePass.EmbedGenerator[?]].iterator
          .map: (name, gen) =>
            gen.embed -> (name, gen)
          .toMap
      end embedGenerators

      private val rewritePasses: IArray[(String, RewritePass)] =
        valuesByType[RewritePass]

      def initStates(using ExecutionContext): Iterator[MCState] =
        type Ident = Token | Node.Embed[?]

        type StructureMap = Map[Ident, List[Node]]
        object StructureMap:
          def empty: StructureMap = Map.empty
        end StructureMap

        type StructureFn = StructureMap => Iterator[Node]

        def buildStructureFn(wf: TokenWf): StructureFn =
          val visited = mutable.HashSet[Ident]()
          val buf = mutable.ListBuffer[StructureFn]()
          def implForWf(wf: TokenWf): Unit =
            if !visited(wf.token)
            then
              visited += wf.token
              val fns = wf.stableShapeSeq.shapes
                .map: shape =>
                  shape match
                    case shape: (Wf.EmbedWf[?] | TokenWf | Wf.Choice) =>
                      val fn = implForShape(shape)
                      fn.andThen(_.map(List(_)))
                    case shape: Wf.RepeatedShape =>
                      val fn = implForShape(shape.shape)
                      structureMap =>
                        (0 until initRepMax).iterator
                          .map: len =>
                            (0 until len).foldLeft(List(Nil): List[List[Node]]):
                              (prefixes, _) =>
                                prefixes.flatMap: prefix =>
                                  fn(structureMap).map(_ :: prefix)
                          .flatten
                    case (token: Token, _) =>
                      ???
                  end match
              val fn: StructureFn = structureMap =>
                val childLists = fns.foldLeft(List(Nil): List[List[Node]]):
                  (prefixes, fn) =>
                    prefixes.flatMap: prefix =>
                      fn(structureMap).map(_ ::: prefix)
                childLists.iterator
                  .map: childList =>
                    Node(wf.token, childList.reverse)
              buf += fn
            end if
          end implForWf
          def implForShape(shape: Wf.Shape): StructureFn =
            shape match
              case shape: Wf.EmbedWf[?] =>
                embedGenerators.get(shape.embed) match
                  case None =>
                    throw AssertionError(
                      s"need to define embed generator for ${shape.embed}",
                    )
                  case Some((name, gen: RewritePass.EmbedGenerator[t])) =>
                    structureMap =>
                      gen.generate.map(Node.embed(_)(using gen.embed))
              case shape: Wf.TokenWf =>
                implForWf(shape)
                structureMap =>
                  structureMap
                    .getOrElse(shape.token, Nil)
                    .iterator
              case shape: Wf.Choice =>
                val impls = shape.choices
                  .map(implForShape)
                structureMap => impls.iterator.map(_(structureMap)).flatten
            end match
          end implForShape
          implForWf(wf)

          structureMap =>
            buf.iterator
              .map(_(structureMap))
              .flatten
        end buildStructureFn

        val structureFn = buildStructureFn(inputWf)

        Iterator
          .iterate(StructureMap.empty): structureMap =>
            structureFn(structureMap).foldLeft(structureMap):
              (structureMap, node) =>
                val token = node.tokenOption.get
                structureMap.updated(
                  token,
                  node :: structureMap.getOrElse(token, Nil),
                )
          .dropWhile(structureMap => !structureMap.contains(inputWf.token))
          .take(initTreeLevels)
          .reduce((l, r) => r)
          .apply(inputWf.token)
          .iterator
          .map: node =>
            MCState("<init>", node, 0)
      end initStates

      def nextStates(state: MCState)(using
          ExecutionContext,
      ): Iterator[MCState] =
        if state.passNum == rewritePasses.length
        then return Iterator.empty
        if state.node.containsError
        then return Iterator.empty

        val passNum = state.passNum
        val (passName, pass) = rewritePasses(state.passNum)

        def impl(state: Node): Iterator[MCState] =
          pass.rewritesAgg
            .runPattern(state.emptyNodeSpanHere)
            .map: (fieldName, nodeSpan) =>
              MCState(s"$passName.$fieldName", nodeSpan.root, passNum)
            .iterator
            ++ state.children.iterator
              .flatMap(impl)
            ++ state.attrs.iterator
              .map(_._2)
              .flatMap(impl)
        end impl

        val iter = impl(state.node)
        if iter.hasNext
        then iter
        else nextStates(MCState(state.fieldName, state.node, state.passNum + 1))
      end nextStates
    end ModelChecker
  end RewritePass

  trait MultiPass extends Pass, ReflectiveEnumeration:
    private lazy val passes = valuesByType[Pass].map(_._2)
    final protected def performImpl(root: Node)(using tracer: PassTracer): Node =
      var node = root
      passes.foreach: pass =>
        if !node.containsError
        then node = pass.perform(node)
      node
    end performImpl
  end MultiPass
end Pass
