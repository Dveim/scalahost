package scala.meta
package internal.hosts.scalac
package converters

// todo clean imports
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

    // removes methods like constructor, getters/setters
    // note that ordering of methods does not change => useful for further matching with scala.meta trees
    def removeFakeMethods(gtrees: List[g.Tree]): List[g.Tree] = {
      def isCtor(gtree: g.Tree): Boolean = gtree match {
        case gtree: DefDef => gtree.name == g.nme.CONSTRUCTOR

        case _ => false
      }

      import scala.reflect.internal.ModifierFlags.{PRIVATE, LOCAL}
      val gsetters = gtrees.sliding(2)
                           .withFilter {
                              case List(vd: ValDef, dd: DefDef) =>
                                vd.mods.hasFlag(PRIVATE) && vd.mods.hasFlag(LOCAL) && dd.name.toString + " " == vd.name.toString

                              case _ => false
                            }
                           .flatten
                           .toSet

      for (t <- gtrees
           if !isCtor(t) && !gsetters.contains(t))
        yield t
    }

    def processLhsPat(pat: Pat): Unit = {
      pat match {
        case p: m.Pat.Var =>
          println("var")

        case p: m.Pat.Tuple =>
          println("tuple")
          println(p.elements)

        case p: m.Pat.Extract =>
          println(s"p.args = [${p.args}]\np.ref = [${p.ref}]\np.targs = [${p.targs}]\n")
          val a = p.args.head
          println(a.tokens)

        case p =>
          println(p.getClass)
      }
    }

    def correlate(gtree: g.Tree, mtree: m.Tree): Unit /*m.Tree*/ = (gtree, mtree) match {
      case (PackageDef(_, stats: List[Tree]), mtree: m.Source) =>
        (stats zip mtree.stats).foreach(correlate _ tupled _)

      case (_: Import, _: m.Import) =>

      case (_: Import, _: m.Pkg) =>

      case (ModuleDef(mods: Modifiers, name: TermName, impl: Template), mtree) => mtree match {
        case mtree: m.Defn.Object if mtree.templ.stats.nonEmpty =>
          (impl.body zip mtree.templ.stats.get.map(_.asInstanceOf[m.Tree])).foreach(correlate _ tupled _)
      }

      case (ClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], impl: Template), mtree) =>
        println(mtree.show[Raw] + "\n")
        removeFakeMethods(impl.body).foreach(showRaw(_))
        println(removeFakeMethods(impl.body).length)
        impl.body.foreach(x => println(showRaw(x) + "\n"))

        mtree match {
          case mtree: m.Defn.Class if mtree.templ.stats.nonEmpty =>
            println(mtree.templ.stats.get.map(_.asInstanceOf[m.Tree]).head.show[Raw])
//            (impl.body zip mtree.templ.stats.get.map(_.asInstanceOf[m.Tree])).foreach(correlate _ tupled _)
      }

      case (DefDef(mods: Modifiers, name: TermName, tparams: List[TypeDef], vparams: List[List[ValDef]], tpt: Tree, rhs: Tree), mtree) => mtree match {
        case mtree: m.Defn.Macro =>

        case mtree: m.Defn.Def =>

        case mtree: m.Defn.Val =>
      }

      case (valdef @ ValDef(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree), mtree) => mtree match {
        case mtree: m.Defn.Val =>
          println(s"DefDef mtree.decltpe = [${mtree.decltpe}]\nmtree.mods = [${mtree.mods}]\nmtree.pats = [${mtree.pats}]\nmtree.rhs = [${mtree.rhs}]")
          println(s"mtree sematics = [${mtree.show[Semantics]}}]")
          println(s"showRaw(ValDef) = [${showRaw(valdef)}}]")

          correlate(rhs, mtree.rhs)
          mtree.pats.foreach(processLhsPat)

        case mtree: m.Defn.Object =>
      }

      case (gtree, mtree) =>
        println(gtree.getClass)
        println(mtree.getClass)
        println
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