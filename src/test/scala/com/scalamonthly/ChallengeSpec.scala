package com.scalamonthly

import cats.data.NonEmptyList
import cats.Show

final class ChallengeSpec extends munit.FunSuite {

    import model._
    import ShowGame._

    test("empty string") {
        val input = ""
        val output = challenge.parse(input)
        assert(output.isLeft, "An empty string should not parse correctly so it should be Left")
    }

    test("super basic game - draw") {
        val input = "1. e4 d5 1/2-1/2"
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.E, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.D, Rank.Five), isCapture = false, None)
            )
        ), Outcome.Draw)
        assertEquals(output, Right(expectedOutput))
    }

    test("super basic game - white wins") {
        val input = "1. e4 d5 1-0"
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.E, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.D, Rank.Five), isCapture = false, None)
            )
        ), Outcome.WhiteWins)
        assertEquals(output, Right(expectedOutput))
    }

    test("super basic game - black wins") {
        val input = "1. e4 d5 0-1"
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.E, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.D, Rank.Five), isCapture = false, None)
            )
        ), Outcome.BlackWins)
        assertEquals(output, Right(expectedOutput))
    }

    test("super basic game - unknown") {
        val input = "1. e4 d5 *"
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.E, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.D, Rank.Five), isCapture = false, None)
            )
        ), Outcome.Unknown)
        assertEquals(output, Right(expectedOutput))
    }

    test("game with two moves") {
        val input = "1. e4 d6 2. d4 Nf6 1-0"
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.E, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.D, Rank.Six), isCapture = false, None)
            ),
             Turn.FullTurn(
                Move.PawnMove(None, Square(File.D, Rank.Four), isCapture = false, None),
                Move.Standard(PieceType.Knight, Square(File.F, Rank.Six), None, checkStatus = None, isCapture = false)
            )
        ), Outcome.WhiteWins)
        assertEquals(output, Right(expectedOutput))
    }

    test("game with two moves and a capture") {
        val input = "1. d4 c5 2. dxc5 Na6 *"
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.D, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.C, Rank.Five), isCapture = false, None)
            ),
             Turn.FullTurn(
                Move.PawnMove(Some(File.D), Square(File.C, Rank.Five), isCapture = true, None),
                Move.Standard(PieceType.Knight, Square(File.A, Rank.Six), None, checkStatus = None, isCapture = false)
            )
        ), Outcome.Unknown)
        assertEquals(output, Right(expectedOutput))
    }

    test("game with new line lf") {
        val input = "1. e4 d6 \n2. d4 Nf6 1-0"
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.E, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.D, Rank.Six), isCapture = false, None)
            ),
             Turn.FullTurn(
                Move.PawnMove(None, Square(File.D, Rank.Four), isCapture = false, None),
                Move.Standard(PieceType.Knight, Square(File.F, Rank.Six), None, checkStatus = None, isCapture = false)
            )
        ), Outcome.WhiteWins)
        assertEquals(output, Right(expectedOutput))
    }

    test("game with file disambiguator") {
        val input = "1. f4 e5 2. d4 c5 3. dxe5 *"
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.F, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.E, Rank.Five), isCapture = false, None)
            ),
             Turn.FullTurn(
                Move.PawnMove(None, Square(File.D, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.C, Rank.Five), isCapture = false, None),
            ),
            Turn.PartialTurn(
                Move.PawnMove(Some(File.D), Square(File.E, Rank.Five), isCapture = true, None)
            )
        ), Outcome.Unknown)
        assertEquals(output, Right(expectedOutput))
    }

    test("game with file disambiguator and check but no capture") {
        val input = "1. c4 d5 2. Rfe8+ *" // Note: This is not a valid chess game, but is valid syntax
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.C, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.D, Rank.Five), isCapture = false, None)
            ),
             Turn.PartialTurn(
                Move.Standard(PieceType.Rook, Square(File.E, Rank.Eight),
                Some(Disambiguator.FileSource(File.F)), checkStatus = Some(CheckStatus.Check), isCapture = false)
            )
        ), Outcome.Unknown)
        assertEquals(output, Right(expectedOutput))
    }

    test("game with rank disambiguator") {
        val input = "1. a4 d6 2. R3xd4 *" // Note: This is not a valid chess game, but is valid syntax
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.A, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.D, Rank.Six), isCapture = false, None)
            ),
            Turn.PartialTurn(
                Move.Standard(PieceType.Rook, Square(File.D, Rank.Four), Some(Disambiguator.RankSource(Rank.Three)), isCapture = true, None)
            )
        ), Outcome.Unknown)
        assertEquals(output, Right(expectedOutput))
    }

    test("game with rank and file disambiguator") {
        val input = "1. a4 d6 2. Qh8xg7 *" // Note: This is not a valid chess game, but is valid syntax
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.A, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.D, Rank.Six), isCapture = false, None)
            ),
            Turn.PartialTurn(
                Move.Standard(PieceType.Queen, Square(File.G, Rank.Seven),
                Some(Disambiguator.FileAndRankSource(Square(File.H, Rank.Eight))), isCapture = true, None)
            )
        ), Outcome.Unknown)
        assertEquals(output, Right(expectedOutput))
    }

    test("game with pawn promotion") {
        val input = "1. a4 d6 2. b8=Q *" // Note: This is not a valid chess game, but is valid syntax
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.A, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.D, Rank.Six), isCapture = false, None)
            ),
            Turn.PartialTurn(
                Move.Promotion(Move.PawnMove(None, Square(File.B, Rank.Eight), isCapture = false, None), PieceType.Queen)
            )
        ), Outcome.Unknown)
        assertEquals(output, Right(expectedOutput))
    }

    test("game with pawn promotion and check") {
        val input = "1. a4 d6 2. b8=Q+ *" // Note: This is not a valid chess game, but is valid syntax
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.A, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.D, Rank.Six), isCapture = false, None)
            ),
            Turn.PartialTurn(
                Move.Promotion(Move.PawnMove(None, Square(File.B, Rank.Eight), isCapture = false, Some(CheckStatus.Check)), PieceType.Queen)
            )
        ), Outcome.Unknown)
        assertEquals(output, Right(expectedOutput))
    }

    test("game with pawn promotion and capture") {
        val input = "1. a4 d6 2. cxb8=Q *" // Note: This is not a valid chess game, but is valid syntax
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.A, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.D, Rank.Six), isCapture = false, None)
            ),
            Turn.PartialTurn(
                Move.Promotion(Move.PawnMove(Some(File.C), Square(File.B, Rank.Eight), isCapture = true, None), PieceType.Queen)
            )
        ), Outcome.Unknown)
        assertEquals(output, Right(expectedOutput))
    }

    test("game with check") {
        val input = "1. c4 d5 2. Qa4+ *"
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.C, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.D, Rank.Five), isCapture = false, None)
            ),
             Turn.PartialTurn(
                Move.Standard(PieceType.Queen, Square(File.A, Rank.Four), None, checkStatus = Some(CheckStatus.Check), isCapture = false)
            )
        ), Outcome.Unknown)
        assertEquals(output, Right(expectedOutput))
    }

    test("game with castle king side") {
        val input = "1. c4 d5 2. O-O *"
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.C, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.D, Rank.Five), isCapture = false, None)
            ),
             Turn.PartialTurn(
                Move.Castle.KingSide
            )
        ), Outcome.Unknown)
        assertEquals(output, Right(expectedOutput))
    }

    test("game with castle queen side") {
        val input = "1. c4 d5 2. O-O-O *"
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.C, Rank.Four), isCapture = false, None),
                Move.PawnMove(None, Square(File.D, Rank.Five), isCapture = false, None)
            ),
             Turn.PartialTurn(
                Move.Castle.QueenSide
            )
        ), Outcome.Unknown)
        assertEquals(output, Right(expectedOutput))
    }

    test("game with checkmate") {
        val input = "1. f3 e5 2. g4 Qh4# 0-1"
        val output = challenge.parse(input)
        val expectedOutput = Game(NonEmptyList.of(
            Turn.FullTurn(
                Move.PawnMove(None, Square(File.F, Rank.Three), isCapture = false, None),
                Move.PawnMove(None, Square(File.E, Rank.Five), isCapture = false, None)
            ),
             Turn.FullTurn(
                Move.PawnMove(None, Square(File.G, Rank.Four), isCapture = false, None),
                Move.Standard(PieceType.Queen, Square(File.H, Rank.Four), None, checkStatus = Some(CheckStatus.Checkmate), isCapture = false)
            )
        ), Outcome.BlackWins)
        assertEquals(output, Right(expectedOutput))
    }

    private def testFullGame(input: String): Unit = {
        val output = challenge.parse(input)
        val expectedOutput = output.map(Show[Game].show)
        assertEquals(expectedOutput, Right(input.replace("\n", " ")))
    }

    test("full game - Kasparov v Topalov 1999") {
        val game = """|1. e4 d6 2. d4 Nf6 3. Nc3 g6 4. Be3 Bg7 5. Qd2 c6 6. f3 b5 7. Nge2 Nbd7 8. Bh6
                      |Bxh6 9. Qxh6 Bb7 10. a3 e5 11. O-O-O Qe7 12. Kb1 a6 13. Nc1 O-O-O 14. Nb3 exd4
                      |15. Rxd4 c5 16. Rd1 Nb6 17. g3 Kb8 18. Na5 Ba8 19. Bh3 d5 20. Qf4+ Ka7 21. Rhe1
                      |d4 22. Nd5 Nbxd5 23. exd5 Qd6 24. Rxd4 cxd4 25. Re7+ Kb6 26. Qxd4+ Kxa5 27. b4+
                      |Ka4 28. Qc3 Qxd5 29. Ra7 Bb7 30. Rxb7 Qc4 31. Qxf6 Kxa3 32. Qxa6+ Kxb4 33. c3+
                      |Kxc3 34. Qa1+ Kd2 35. Qb2+ Kd1 36. Bf1 Rd2 37. Rd7 Rxd7 38. Bxc4 bxc4 39. Qxh8
                      |Rd3 40. Qa8 c3 41. Qa4+ Ke1 42. f4 f5 43. Kc1 Rd2 44. Qa7 1-0""".stripMargin
        testFullGame(game)
    }

    test("full game - Paul Morphy v Duke Karl Count Isouard 1858") {
        val game = """|1. e4 e5 2. Nf3 d6 3. d4 Bg4 4. dxe5 Bxf3 5. Qxf3 dxe5 6. Bc4 Nf6 7. Qb3 Qe7 8.
                      |Nc3 c6 9. Bg5 b5 10. Nxb5 cxb5 11. Bxb5+ Nbd7 12. O-O-O Rd8 13. Rxd7 Rxd7 14.
                      |Rd1 Qe6 15. Bxd7+ Nxd7 16. Qb8+ Nxb8 17. Rd8# 1-0""".stripMargin
        testFullGame(game)
    }

    test("full game - Aronian v Anand 2013") {
        val game = """|1. d4 d5 2. c4 c6 3. Nf3 Nf6 4. Nc3 e6 5. e3 Nbd7 6. Bd3 dxc4 7. Bxc4 b5 8. Bd3
                      |Bd6 9. O-O O-O 10. Qc2 Bb7 11. a3 Rc8 12. Ng5 c5 13. Nxh7 Ng4 14. f4 cxd4 15.
                      |exd4 Bc5 16. Be2 Nde5 17. Bxg4 Bxd4+ 18. Kh1 Nxg4 19. Nxf8 f5 20. Ng6 Qf6 21. h3
                      |Qxg6 22. Qe2 Qh5 23. Qd3 Be3 0-1""".stripMargin
        testFullGame(game)
    }

    test("full game - Byrne v Fischer 1956") {
        val game = """|1. Nf3 Nf6 2. c4 g6 3. Nc3 Bg7 4. d4 O-O 5. Bf4 d5 6. Qb3 dxc4 7. Qxc4 c6 8. e4
                      |Nbd7 9. Rd1 Nb6 10. Qc5 Bg4 11. Bg5 Na4 12. Qa3 Nxc3 13. bxc3 Nxe4 14. Bxe7 Qb6
                      |15. Bc4 Nxc3 16. Bc5 Rfe8+ 17. Kf1 Be6 18. Bxb6 Bxc4+ 19. Kg1 Ne2+ 20. Kf1 Nxd4+
                      |21. Kg1 Ne2+ 22. Kf1 Nc3+ 23. Kg1 axb6 24. Qb4 Ra4 25. Qxb6 Nxd1 26. h3 Rxa2 27.
                      |Kh2 Nxf2 28. Re1 Rxe1 29. Qd8+ Bf8 30. Nxe1 Bd5 31. Nf3 Ne4 32. Qb8 b5 33. h4 h5
                      |34. Ne5 Kg7 35. Kg1 Bc5+ 36. Kf1 Ng3+ 37. Ke1 Bb4+ 38. Kd1 Bb3+ 39. Kc1 Ne2+ 40.
                      |Kb1 Nc3+ 41. Kc1 Rc2# 0-1""".stripMargin
        testFullGame(game)
    }

    test("full game - Ivanchuk v Yusupov 1991") {
        val game = """|1. c4 e5 2. g3 d6 3. Bg2 g6 4. d4 Nd7 5. Nc3 Bg7 6. Nf3 Ngf6 7. O-O O-O 8. Qc2
                      |Re8 9. Rd1 c6 10. b3 Qe7 11. Ba3 e4 12. Ng5 e3 13. f4 Nf8 14. b4 Bf5 15. Qb3 h6
                      |16. Nf3 Ng4 17. b5 g5 18. bxc6 bxc6 19. Ne5 gxf4 20. Nxc6 Qg5 21. Bxd6 Ng6 22.
                      |Nd5 Qh5 23. h4 Nxh4 24. gxh4 Qxh4 25. Nde7+ Kh8 26. Nxf5 Qh2+ 27. Kf1 Re6 28.
                      |Qb7 Rg6 29. Qxa8+ Kh7 30. Qg8+ Kxg8 31. Nce7+ Kh7 32. Nxg6 fxg6 33. Nxg7 Nf2 34.
                      |Bxf4 Qxf4 35. Ne6 Qh2 36. Rdb1 Nh3 37. Rb7+ Kh8 38. Rb8+ Qxb8 39. Bxh3 Qg3 0-1""".stripMargin
        testFullGame(game)
    }

    test("full game - Short v Timman 1991") {
        val game = """|1. e4 Nf6 2. e5 Nd5 3. d4 d6 4. Nf3 g6 5. Bc4 Nb6 6. Bb3 Bg7 7. Qe2 Nc6 8. O-O
                      |O-O 9. h3 a5 10. a4 dxe5 11. dxe5 Nd4 12. Nxd4 Qxd4 13. Re1 e6 14. Nd2 Nd5 15.
                      |Nf3 Qc5 16. Qe4 Qb4 17. Bc4 Nb6 18. b3 Nxc4 19. bxc4 Re8 20. Rd1 Qc5 21. Qh4 b6
                      |22. Be3 Qc6 23. Bh6 Bh8 24. Rd8 Bb7 25. Rad1 Bg7 26. R8d7 Rf8 27. Bxg7 Kxg7 28.
                      |R1d4 Rae8 29. Qf6+ Kg8 30. h4 h5 31. Kh2 Rc8 32. Kg3 Rce8 33. Kf4 Bc8 34. Kg5
                      |1-0""".stripMargin
        testFullGame(game)
    }

    test("full game - Carlsen v Xiong 2020") {
        val game = """|1. d4 Nf6 2. c4 e6 3. Nf3 d5 4. Nc3 c5 5. cxd5 Nxd5 6. e4 Nxc3 7. bxc3 cxd4 8.
                      |cxd4 Bb4+ 9. Bd2 Bxd2+ 10. Qxd2 O-O 11. Bd3 Nc6 12. Bc2 Qe7 13. O-O Rd8 14. Qe3
                      |b6 15. Rfd1 Bb7 16. Rac1 Rac8 17. h4 Na5 18. Bd3 h6 19. g4 Nc6 20. Bb1 Qd7 21.
                      |Kh2 Qd6+ 22. Kg1 Na5 23. Bd3 Rxc1 24. Rxc1 Nc6 25. Bb5 Nxd4 26. Qxd4 Qxd4 27.
                      |Nxd4 Rxd4 28. f3 a6 29. Bf1 Rd7 30. a4 Kf8 31. Kf2 Ke7 32. Rb1 Rd6 33. e5 Rc6
                      |34. Bd3 g5 35. h5 Bc8 36. Ke3 Rc5 37. f4 Ra5 38. Rxb6 gxf4+ 39. Kxf4 Rxa4+ 40.
                      |Kf3 Ra3 41. Ke4 f6 42. Rb8 Bd7 43. Rb7 fxe5 44. g5 hxg5 45. h6 Kd6 46. Rb6+ Bc6+
                      |47. Rxc6+ Kxc6 48. h7 Ra4+ 49. Kxe5 Rh4 50. Kf6 g4 51. Kg5 Rh2 52. Kxg4 Kd5 53.
                      |Bg6 Rh6 54. Kg5 Rxh7 55. Bxh7 e5 56. Kg4 Kd4 1/2-1/2""".stripMargin
        testFullGame(game)
    }

    test("full game - Caruana v Carlsen 2019") {
        val game = """|1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6 5. O-O Nxe4 6. d4 Be7 7. Re1 b5 8. Rxe4
                      |d5 9. Nxe5 Nxe5 10. Rxe5 bxa4 11. Nc3 O-O 12. Re1 Bd6 13. Qh5 a3 14. Nxd5 axb2
                      |15. Bxb2 Rb8 16. Rab1 Re8 17. Ne3 Bf4 18. d5 Rb4 19. c4 Bxe3 20. Rxe3 Rxe3 21.
                      |fxe3 Rxc4 22. Bd4 c5 23. Ba1 f6 24. h3 Qe7 25. Qf3 h6 26. Re1 Re4 27. Kf2 Bd7
                      |28. Rd1 Kh7 29. Rd2 Qe8 30. Bb2 Ra4 31. a3 Rc4 32. Re2 Qg6 33. e4 Ba4 34. d6 Bc6
                      |35. Qg3 Qxg3+ 36. Kxg3 Kg6 37. e5 fxe5 38. Bxe5 h5 39. Bb2 h4+ 40. Kh2 Kf5 41.
                      |Bxg7 Re4 42. Rxe4 Kxe4 43. Bf6 c4 44. Bxh4 c3 45. Bf6 c2 46. Bb2 Kd5 47. g4 Kxd6
                      |48. Kg3 Bb5 49. Kf3 Ke7 50. h4 Kf8 51. h5 Be8 52. Kg3 Kg8 53. Kh4 Kh7 54. Kg5
                      |Bd7 55. Kf4 a5 56. Kg5 a4 57. Kf4 Be6 58. g5 Bf7 59. g6+ Bxg6 60. hxg6+ Kxg6 61.
                      |Ke5 Kf7 62. Kd6 Ke8 63. Bc1 Kd8 64. Kc6 Kc8 65. Bf4 c1=Q+ 66. Bxc1 Kb8 67. Kb6
                      |Ka8 68. Bf4 1/2-1/2""".stripMargin
        testFullGame(game)
    }

    test("full game - Mamedyarov v Carlsen 2019") {
        val game = """|1. d4 d5 2. c4 c6 3. Nc3 Nf6 4. e3 e6 5. Nf3 Nbd7 6. Qc2 Bd6 7. Bd3 dxc4 8. Bxc4
                      |b5 9. Be2 O-O 10. O-O a6 11. Rd1 Qc7 12. e4 e5 13. g3 Re8 14. a3 exd4 15. Nxd4
                      |Be5 16. Bf3 c5 17. Nde2 Bb7 18. Be3 c4 19. b4 cxb3 20. Qxb3 Nc5 21. Bxc5 Qxc5
                      |22. Rac1 Qb6 23. Na4 Qa7 24. Nc5 Rac8 25. Nd7 Rxc1 26. Nxc1 h6 27. Nxf6+ Bxf6
                      |28. Rd7 Re7 29. Rd8+ Kh7 30. Qd3 Be5 31. Rh8+ Kxh8 32. Qd8+ Kh7 33. Qxe7 Bd4 34.
                      |Qxf7 Bxf2+ 35. Kg2 Bc8 36. Qa2 Bb6 37. e5 Bf5 38. g4 Bg6 39. Ne2 Qc7 40. Nf4
                      |Qc2+ 41. Qxc2 Bxc2 42. Nd5 Bc5 43. Nb4 Bb3 44. Nxa6 Bxa3 45. Nc7 Bc4 46. e6 Bd6
                      |47. Bc6 b4 48. Nd5 Kg6 49. e7 Bxd5+ 50. Bxd5 Bxe7 51. Kf3 Kf6 52. Ke4 Kg5 53.
                      |Kf3 Kh4 54. Be6 Bg5 55. Kg2 Bf4 56. h3 Kg5 57. Kf3 Kf6 58. Bg8 Ke5 59. h4 g5 60.
                      |hxg5 hxg5 61. Ke2 Kd4 62. Kd1 Kc3 63. Bf7 b3 64. Be6 Be3 65. Bf7 Bf4 66. Be6 Be3
                      |67. Bf7 Bf4 68. Be6 1/2-1/2""".stripMargin
        testFullGame(game)
    }

    test("full game - Spassky v Fischer 1992") {
        val game = """|1. d4 Nf6 2. c4 g6 3. Nc3 Bg7 4. e4 d6 5. f3 O-O 6. Be3 Nc6 7. Nge2 a6 8. h4 h5
                      |9. Nc1 e5 10. d5 Ne7 11. Be2 Nh7 12. Nd3 f5 13. a4 Nf6 14. Nf2 a5 15. Qc2 c5 16.
                      |O-O-O b6 17. Rdg1 Nh7 18. Nb5 Kh8 19. g4 hxg4 20. fxg4 f4 21. Bd2 g5 22. hxg5
                      |Ng6 23. Rh5 Rf7 24. Rgh1 Bf8 25. Qb3 Rb8 26. Qh3 Rbb7 27. Nd3 Kg8 28. Ne1 Rg7
                      |29. Nf3 Rbf7 30. Rh6 Qd7 31. Qh5 Qxg4 32. Rxg6 Qxh5 33. Rxg7+ Rxg7 34. Rxh5 Bg4
                      |35. Rh4 Bxf3 36. Bxf3 Nxg5 37. Bg4 Rh7 38. Rxh7 Kxh7 39. Kc2 Be7 40. Kd3 Kg6 41.
                      |Nc7 Kf7 42. Ne6 Nh7 43. Bh5+ Kg8 44. Be1 Nf6 45. Bh4 Kh7 46. Bf7 Nxd5 47. cxd5
                      |Bxh4 48. Bh5 Kh6 49. Be2 Bf2 50. Kc4 Bd4 51. b3 Kg6 52. Kb5 Kf6 53. Kc6 Ke7 54.
                      |Ng7 1-0""".stripMargin
        testFullGame(game)
    }

    test("full game - Carlsen v Ernst 2004") {
        val game = """|1. e4 c6 2. d4 d5 3. Nc3 dxe4 4. Nxe4 Bf5 5. Ng3 Bg6 6. h4 h6 7. Nf3 Nd7 8. h5
                      |Bh7 9. Bd3 Bxd3 10. Qxd3 e6 11. Bf4 Ngf6 12. O-O-O Be7 13. Ne4 Qa5 14. Kb1 O-O
                      |15. Nxf6+ Nxf6 16. Ne5 Rad8 17. Qe2 c5 18. Ng6 fxg6 19. Qxe6+ Kh8 20. hxg6 Ng8
                      |21. Bxh6 gxh6 22. Rxh6+ Nxh6 23. Qxe7 Nf7 24. gxf7 Kg7 25. Rd3 Rd6 26. Rg3+ Rg6
                      |27. Qe5+ Kxf7 28. Qf5+ Rf6 29. Qd7# 1-0""".stripMargin
        testFullGame(game)
    }
    
}