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

    def correlate(gtree: g.Tree, mtree: m.Tree): Unit /*m.Tree*/ = (gtree, mtree) match {
      case (PackageDef(_, stats: List[Tree]), mtree: m.Source) =>
        (stats zip mtree.stats).foreach(correlate _ tupled _)

      case (_: Import, _: m.Import) =>

      case (_: Import, _: m.Pkg) =>

      case (ModuleDef(mods: Modifiers, name: TermName, impl: Template), mtree) => mtree match {
        case mtree: m.Defn.Object if mtree.templ.stats.nonEmpty =>
          (impl.body zip mtree.templ.stats.get.map(_.asInstanceOf[m.Tree])).foreach(correlate _ tupled _)
      }

      case (ClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], impl: Template), mtree) => mtree match {
        case mtree: m.Defn.Class if mtree.templ.stats.nonEmpty =>
          (impl.body zip mtree.templ.stats.get.map(_.asInstanceOf[m.Tree])).foreach(correlate _ tupled _)
      }

      case (DefDef(mods: Modifiers, name: TermName, tparams: List[TypeDef], vparams: List[List[ValDef]], tpt: Tree, rhs: Tree), mtree) => mtree match {
        case mtree: m.Defn.Macro =>

        case mtree: m.Defn.Def =>

        case mtree: m.Defn.Val =>
      }

      case (ValDef(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree), mtree) => mtree match {
        case mtree: m.Defn.Val =>

        case mtree: m.Defn.Object =>
      }

      case (gtree, mtree) =>
        println(gtree.getClass)
        println(mtree.getClass)
        println


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