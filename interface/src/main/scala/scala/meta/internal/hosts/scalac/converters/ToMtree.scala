package scala.meta
package internal.hosts.scalac
package converters

// todo clean imports
import scala.language.postfixOps
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

    def processLhsPat(gtree: g.Tree, pat: Pat): Unit = (gtree, pat) match {
        case (gcasedef: g.CaseDef, mextract: m.Pat.Extract) =>
          println(s"p.args = [${mextract.args}]\np.ref = [${mextract.ref}]\np.targs = [${mextract.targs}]\n")
          gcasedef.pat // todo and what should I do? change p.ref ??? (e.g. specify List to scala.List)

        case p =>
          println(p.getClass)
      }

    def correlate(gtree: g.Tree, mtree: m.Tree): Unit /*m.Tree*/ = (gtree, mtree) match {
      case (PackageDef(_, stats: List[Tree]), mtree: m.Source) =>
        (stats zip mtree.stats).foreach(correlate _ tupled _)

      case (_: Import, _: m.Import) =>

      case (_: Import, _: m.Pkg) =>

      case (mdef @ ModuleDef(mods: Modifiers, name: TermName, impl: Template), mtree) => mtree match {
        case mtree: m.Defn.Object if mtree.templ.stats.nonEmpty =>
          (impl.body zip mtree.templ.stats.get.map(_.asInstanceOf[m.Tree])).foreach(correlate _ tupled _)

        case _ =>
      }

      case (ClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], impl: Template), mtree) => mtree match {
        case mtree: m.Defn.Class if mtree.templ.stats.nonEmpty =>
          (removeFakeMethods(impl.body) zip mtree.templ.stats.get.map(_.asInstanceOf[m.Tree])).foreach(correlate _ tupled) // todo extract in method
      }

      case (DefDef(mods: Modifiers, name: TermName, tparams: List[TypeDef], vparams: List[List[ValDef]], tpt: Tree, rhs: Tree), mtree) => mtree match {
        case mtree: m.Defn.Macro =>

        case mtree: m.Defn.Def =>

        case mtree: m.Defn.Val =>
      }

      case (valdef @ ValDef(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree), mtree) => mtree match {
        case mtree: m.Defn.Val =>
          println(s"mtree.show[Raw] = [${mtree.show[Raw]}}]\n")
          println(s"mtree semantics = [${mtree.show[Semantics]}}]\n")
          println(s"ValDef:\nmtree.decltpe = [${mtree.decltpe}]\nmtree.mods = [${mtree.mods}]\nmtree.pats = [${mtree.pats}]\nmtree.rhs = [${mtree.rhs.show[Raw]}]\n")
          println(s"showRaw(ValDef) = [${showRaw(valdef)}}]\n")

//          mtree.copy(decltpe = Some(valdef.tpe)) todo scala.reflect type to scala.meta type
          correlate(rhs, mtree.rhs)

          valdef.rhs match {
            case gmatch: g.Match =>
              mtree.pats.foreach(processLhsPat(gmatch, _))
          }

        case mtree: m.Defn.Object =>
      }

      case (gtree, mtree) =>
        println("MISSED")
        println(gtree.getClass)
        println(mtree.getClass)
        println
    }

    correlate(gtree, gtree.pos.source.content.parse[Source].asInstanceOf[m.Tree])

    gtree.pos.source.content.parse[Source].asInstanceOf[m.Tree]
  }
}