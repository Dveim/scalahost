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
//
// A lot of heavylifting is performed by the `@converter` macro annotation,
// which not only simplifies the notation of the converter (by providing convenience .cvt and .cvt_! methods),
// but also generates a strongly-typed conversion harness, produces boilerplate that simplifies debugging,
// ensures that all trees are covered by the converter, that there are no conversion ambiguities and much else.
//
// Even though this macro annotation does a lot, it's forcing us into
// a lot of notational weirdness and itself has an implementation whose LOC count
// surpasses the LOC count of this file. I think that, at a point when we can afford big refactorings,
// we need to drop the annotation and rewrite everything without its help.
trait ToMtree extends GlobalToolkit with MetaToolkit {
  self: Api =>

  def toMtree(in: g.Tree): m.Tree = {
    in.pos.source.content.parse[Source].asInstanceOf[m.Tree]
  }
}