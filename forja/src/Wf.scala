package forja

import scala.annotation.{publicInBinary, tailrec}

import Wf.*

trait Wf:
  protected given localContext: syntax.WfContext.type = syntax.WfContext
end Wf

object Wf:
  sealed trait Shape:
    def |(other: TokenWf | EmbedWf[?]): Shape
  end Shape

  final class EmbedWf[T <: Matchable] @publicInBinary private[forja] (using
      val embed: Node.Embed[T],
  ) extends Shape:
    def |(other: TokenWf | EmbedWf[?]): Shape =
      Choice(this, other)
    end |
  end EmbedWf

  final class TokenWf private[forja] (val token: Token, shapeSeq: => ShapeSeq)
      extends Shape:
    tokenWf =>
    def |(other: TokenWf | EmbedWf[?]): Shape = Choice(this, other)

    export token.apply

    def replace(shapes: => Shapes*): TokenWf =
      new TokenWf(token, ShapeSeq(shapes*))
    end replace

    private[forja] lazy val stableShapeSeq = shapeSeq

    object validate extends Pass:
      protected def performImpl(root: Node)(using tracer: PassTracer): Node =
        def implToken(node: Node, wf: TokenWf): Node =
          if node.containsError
          then return node

          node.tokenOption match
            case Some(wf.token) =>
              @tailrec
              def impl(node: Node, idx: Int, shapes: Seq[Shapes]): Node =
                def implSingle(node: Node, shape: Shape): Node =
                  if node.containsError
                  then node
                  else
                    shape match
                      case shape: EmbedWf[t] =>
                        given Node.Embed[t] = shape.embed
                        node.valueOption[t] match
                          case Some(_) => node
                          case None    =>
                            node.replaceThis:
                              Node.error(
                                s"expected ${shape.embed.getClass().getName()}",
                              )(node)
                      case shape: TokenWf =>
                        implToken(node, shape)
                      case shape: Choice =>
                        val ok = shape.choices.iterator
                          .exists:
                            case shape: EmbedWf[t] =>
                              given Node.Embed[t] = shape.embed
                              node.valueOption[t].nonEmpty
                            case shape: TokenWf =>
                              node.tokenOption.contains(shape.token)
                        if ok
                        then node
                        else
                          node.replaceThis:
                            val choicesStrs = shape.choices.map:
                              case shape: EmbedWf[t] =>
                                shape.embed.getClass().getName()
                              case shape: TokenWf =>
                                shape.token.fullName
                            Node.error(
                              s"expected one of ${choicesStrs.mkString(" | ")}",
                            )(node)
                    end match
                  end if
                end implSingle

                shapes match
                  case Seq() =>
                    if idx == node.children.size
                    then node
                    else
                      // TODO: consider appending the error onto the end of the node's children
                      node.replaceThis:
                        Node.error(
                          s"unmatched children remain, matched up to here (idx = $idx)",
                        )((node.children.lift(idx).toList :+ node)*)
                    end if
                  case shape +: restShapes =>
                    if !node.children.indices.contains(idx)
                    then
                      shape match
                        case _: RepeatedShape =>
                          // End of repetition, ok actually.
                          // It'll still fail if anything in restShapes requires elements though.
                          impl(
                            node = node,
                            idx = idx,
                            shapes = restShapes,
                          )
                        case _ =>
                          node.replaceThis:
                            Node.error(
                              s"wrong child count (index $idx out of range, shapes remaining)",
                            )(node)
                      end match
                    else
                      shape match
                        case shape: Shape =>
                          impl(
                            node = implSingle(
                              node.children(idx),
                              shape,
                            ).parentOption.get,
                            idx = idx + 1,
                            shapes = restShapes,
                          )
                        case shape: (Token, Shape) =>
                          ???
                        case shape: RepeatedShape =>
                          val resultNode =
                            implSingle(node.children(idx), shape.shape)
                          if resultNode.isError // not just contains, the error must be at top level
                          then
                            impl(
                              node = node,
                              idx = idx,
                              shapes = restShapes,
                            )
                          else
                            impl(
                              node = resultNode.parentOption.get,
                              idx = idx + 1,
                              shapes = shapes,
                            )
                          end if
                      end match
                    end if
                end match
              end impl

              impl(node = node, idx = 0, shapes = wf.stableShapeSeq.shapes)
            case Some(otherToken) =>
              node.replaceThis:
                Node.error(s"expected $token")(node)
            case None =>
              node.replaceThis:
                Node.error(s"expected $token")(node)
          end match
        end implToken

        root.replaceThis(implToken(root.asOrphan, tokenWf))
      end performImpl
    end validate
  end TokenWf

  private[forja] final class Choice(val choices: TokenWf | EmbedWf[?]*)
      extends Shape:
    def |(other: TokenWf | EmbedWf[?]): Shape = Choice((choices :+ other)*)
  end Choice

  type Shapes = Shape | (Token, Shape) | RepeatedShape

  private[forja] final class ShapeSeq(val shapes: Shapes*)

  private[forja] final class RepeatedShape @publicInBinary private[forja] (
      val shape: Shape,
  )
end Wf
