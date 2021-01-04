package com.scalamonthly

import cats.data.NonEmptyList

object model {

  sealed abstract class File extends Product with Serializable
  object File {
    case object A extends File
    case object B extends File
    case object C extends File
    case object D extends File
    case object E extends File
    case object F extends File
    case object G extends File
    case object H extends File
  }

  sealed abstract class Rank extends Product with Serializable
  object Rank {
    case object One extends Rank
    case object Two extends Rank
    case object Three extends Rank
    case object Four extends Rank
    case object Five extends Rank
    case object Six extends Rank
    case object Seven extends Rank
    case object Eight extends Rank
  }

  sealed abstract class PieceType extends Product with Serializable
  object PieceType {
    case object King extends PieceType
    case object Queen extends PieceType
    case object Rook extends PieceType
    case object Bishop extends PieceType
    case object Knight extends PieceType
  }

  final case class Square(file: File, rank: Rank)

  sealed abstract class Disambiguator extends Product with Serializable
  object Disambiguator {
    final case class FileSource(source: File) extends Disambiguator
    final case class RankSource(source: Rank) extends Disambiguator
    final case class FileAndRankSource(source: Square) extends Disambiguator
  }

  sealed abstract class CheckStatus extends Product with Serializable
  object CheckStatus {
    case object Check extends CheckStatus
    case object Checkmate extends CheckStatus
  }

  sealed abstract class Move extends Product with Serializable
  object Move {
    sealed abstract class Castle extends Move
    object Castle {
      case object QueenSide extends Castle
      case object KingSide extends Castle
    }
    final case class Promotion(pawnMove: PawnMove, newPiece: PieceType) extends Move
    final case class Standard(piece: PieceType, end: Square, disambiguator: Option[Disambiguator], isCapture: Boolean, checkStatus: Option[CheckStatus]) extends Move
    final case class PawnMove(sourceFile: Option[File], end: Square, isCapture: Boolean, checkStatus: Option[CheckStatus]) extends Move
  }

  sealed abstract class Turn extends Product with Serializable
  object Turn {
    final case class FullTurn(whiteMove: Move, blackMove: Move) extends Turn
    final case class PartialTurn(whiteMove: Move) extends Turn
  }

  sealed abstract class Outcome extends Product with Serializable
  object Outcome {
    case object WhiteWins extends Outcome
    case object BlackWins extends Outcome
    case object Draw extends Outcome
    case object Unknown extends Outcome
  }

  final case class Game(turns: NonEmptyList[Turn], outcome: Outcome)

}
