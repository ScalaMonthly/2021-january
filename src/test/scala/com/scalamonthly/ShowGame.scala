package com.scalamonthly

import cats.data.NonEmptyList
import cats.Show
import cats.syntax.show._

/**
  * You can ignore this class. It is just used for turning the game model back
  * into a movetext for the sake of making testing full games simpler.
  */
object ShowGame {

    import model._

    private implicit def showNELWithIndex[A](implicit AA: Show[A]): Show[NonEmptyList[A]] = Show.show { nel =>
        nel.map(AA.show).toList.zipWithIndex.map { case (a, index) => s"${(index + 1)}. $a "}.mkString
    }

    private implicit def showOpt[A](implicit AA: Show[A]): Show[Option[A]] = Show.show {
        case Some(a) => AA.show(a)
        case None => ""
    }

    private implicit val showFile: Show[File] = Show.show { file =>
        import File._
        val files = Map(A -> "a", B -> "b", C -> "c", D -> "d", E -> "e", F -> "f", G -> "g", H -> "h")
        files.get(file).get
    }

    private implicit val showRank: Show[Rank] = Show.show { rank =>
        import Rank._
        val ranks = Map(One -> "1", Two -> "2", Three -> "3", Four -> "4", Five -> "5", Six -> "6", Seven -> "7", Eight -> "8")
        ranks.get(rank).get
    }

    private implicit val showPieceType: Show[PieceType] = Show.show { pt =>
        import PieceType._
        val pts = Map(King -> "K", Queen -> "Q", Rook -> "R", Bishop -> "B", Knight -> "N")
        pts.get(pt).get
    }

    private implicit val showSquare: Show[Square] = Show.show { square =>
        show"${square.file}${square.rank}"
    }

    private def isCapture(is: Boolean): String = {
        if (is) "x" else ""
    }

    private implicit val showDisambiguator: Show[Disambiguator] = Show.show {
        case d: Disambiguator.FileAndRankSource => show"${d.source}"
        case d: Disambiguator.FileSource => show"${d.source}"
        case d: Disambiguator.RankSource => show"${d.source}"
    }

    private implicit val showCheck: Show[CheckStatus] = Show.show {
        case CheckStatus.Check => "+"
        case CheckStatus.Checkmate => "#"
    }

    private implicit val showMove: Show[Move] = Show.show {
        case Move.Castle.KingSide => "O-O"
        case Move.Castle.QueenSide => "O-O-O"
        case m: Move.PawnMove => show"${m.sourceFile}${isCapture(m.isCapture)}${m.end}${m.checkStatus}"
        case m: Move.Promotion => show"${m.pawnMove.sourceFile}${isCapture(m.pawnMove.isCapture)}${m.pawnMove.end}=${m.newPiece}${m.pawnMove.checkStatus}"
        case m: Move.Standard => show"${m.piece}${m.disambiguator}${isCapture(m.isCapture)}${m.end}${m.checkStatus}"
    }

    private implicit val showTurn: Show[Turn] = Show.show {
        case t: Turn.FullTurn => show"${t.whiteMove} ${t.blackMove}"
        case t: Turn.PartialTurn =>
        show"${t.whiteMove}"
    }

    private implicit val showOutcome: Show[Outcome] = Show.show {
        case Outcome.BlackWins => "0-1"
        case Outcome.WhiteWins => "1-0"
        case Outcome.Draw => "1/2-1/2"
        case Outcome.Unknown => "*"
    }

    implicit val showGame: Show[Game] = Show.show { game =>
        show"${game.turns}${game.outcome}"
    }
}