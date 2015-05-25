package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.meta.{Toolkit => MetaToolkit}
import org.scalameta.reflection._
import org.scalameta.invariants._
import org.scalameta.unreachable
import org.scalameta.convert.auto._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.internal.Flags._
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.reflect.runtime.universe.{Type => Pt}
import scala.meta.internal.{hygiene => h}
import scala.meta.internal.parsers.Helpers.{XtensionTermOps => _, _}
import scala.meta.dialects.Scala211

// This module exposes a method that can convert scala.reflect trees into high-fidelity scala.meta trees.
//
// It is capable of undoing a whole bunch of desugarings performed by scalac,
// but for some of these desugarings it needs additional tree attachments produced by custom analyzer
// provided by the scalahost plugin.

trait ToMtree extends GlobalToolkit with MetaToolkit {
  self: Api =>

  def toMtree(gtree: g.Tree): m.Tree = {
    import g.{Quasiquote => _, _}

    def inside(pat: Any, mtree: m.Tree)(block: => Unit): Unit = {
      val name = pat.getClass.getSimpleName
      val hashCode = pat.hashCode()

      println(s"in [$name] [$hashCode]")
//      println(s"mTree is [$mtree]")
      block
      println(s"out [$name] [$hashCode]")
    }

    def correlate(gtree: g.Tree, mtree: m.Tree): Unit /*m.Tree*/ = (gtree, mtree) match {
      case (pat @ Block(stats: List[Tree], expr: Tree), _mtree) =>
        inside(pat, _mtree) {
          stats.foreach(correlate(_, _mtree))
        }

      case (pat @ ClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], impl: Template), _mtree) =>
        inside(pat, _mtree) {
          impl.body.foreach(t => correlate(t.asInstanceOf[g.Tree], _mtree))
        }

      case (pat @ DefDef(mods: Modifiers, name: TermName, tparams: List[TypeDef], vparams: List[List[ValDef]], tpt: Tree, rhs: Tree), _mtree) =>
        inside(pat, _mtree) {

        }

      case (pat @ Import(expr, selectors), _mtree) =>
        inside(pat, _mtree) {

        }

      case (pat @ Literal(value), _mtree) =>
        inside(pat, _mtree) {

        }

      case (pat @ ModuleDef(mods: Modifiers, name: TermName, impl: Template), _mtree) =>
        inside(pat, _mtree) {

        }

      case (pat @ PackageDef(pid: RefTree, stats: List[Tree]), _mtree) =>
        inside(pat, _mtree) {
          stats.foreach(correlate(_, _mtree))
        }

      case (pat @ Select(qualifier: Tree, name: Name), _mtree) =>
        inside(pat, _mtree) {

        }

      case (pat @ ValDef(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree), _mtree) =>
        inside(pat, _mtree) {

        }

      case (pat @ TypeTree(), _mtree) =>
        inside(pat, _mtree) {

        }

      case (EmptyTree, _mtree) =>
        println("EmptyTree")

//      case _ =>
//        ???
    }

    //    val out = correlate(gtree, gtree.pos.source.content.parse[Source].asInstanceOf[m.Tree])
    //
    //    println(s"\nout.show[Code] = [${out.show[Code]}]")
    //    println(s"\nout.show[Raw] = [${out.show[Raw]}]")
    //    println(s"\nout.show[Semantics] = [${out.show[Semantics]}]")
    //
    //    out
    correlate(gtree, gtree.pos.source.content.parse[Source].asInstanceOf[m.Tree])
    gtree.pos.source.content.parse[Source].asInstanceOf[m.Tree]
  }
}