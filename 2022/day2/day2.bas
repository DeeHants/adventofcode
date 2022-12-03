Rem Advent of Code 2022 day 2
Rem Deanna Earley - DeezyPuzzles
Rem BSD 3-clause Licence

Rem https://adventofcode.com/2022/day/2
Rem --- Day 2: Rock Paper Scissors ---
Rem The Elves begin to set up camp on the beach. To decide whose tent gets to
Rem be closest to the snack storage, a giant Rock Paper Scissors tournament is
Rem already in progress.
Rem
Rem Rock Paper Scissors is a game between two players. Each game contains many
Rem rounds; in each round, the players each simultaneously choose one of Rock,
Rem Paper, or Scissors using a hand shape. Then, a winner for that round is
Rem selected: Rock defeats Scissors, Scissors defeats Paper, and Paper defeats
Rem Rock. If both players choose the same shape, the round instead ends in a
Rem draw.
Rem
Rem Appreciative of your help yesterday, one Elf gives you an encrypted
Rem strategy guide (your puzzle input) that they say will be sure to help you
Rem win. "The first column is what your opponent is going to play: A for Rock,
Rem B for Paper, and C for Scissors. The second column--" Suddenly, the Elf is
Rem called away to help with someone's tent.
Rem
Rem The second column, you reason, must be what you should play in response: X
Rem for Rock, Y for Paper, and Z for Scissors. Winning every time would be
Rem suspicious, so the responses must have been carefully chosen.
Rem
Rem The winner of the whole tournament is the player with the highest score.
Rem Your total score is the sum of your scores for each round. The score for a
Rem single round is the score for the shape you selected (1 for Rock, 2 for
Rem Paper, and 3 for Scissors) plus the score for the outcome of the round (0
Rem if you lost, 3 if the round was a draw, and 6 if you won).
Rem
Rem Since you can't be sure if the Elf is trying to help you or trick you, you
Rem should calculate the score you would get if you were to follow the strategy
Rem guide.
Rem
Rem For example, suppose you were given the following strategy guide:
Rem
Rem   A Y
Rem   B X
Rem   C Z
Rem
Rem This strategy guide predicts and recommends the following:
Rem
Rem In the first round, your opponent will choose Rock (A), and you should
Rem choose Paper (Y). This ends in a win for you with a score of 8 (2 because
Rem you chose Paper + 6 because you won).
Rem In the second round, your opponent will choose Paper (B), and you should
Rem choose Rock (X). This ends in a loss for you with a score of 1 (1 + 0).
Rem The third round is a draw with both players choosing Scissors, giving you a
Rem score of 3 + 3 = 6.
Rem In this example, if you were to follow the strategy guide, you would get a
Rem total score of 15 (8 + 1 + 6).
Rem
Rem What would your total score be if everything goes exactly according to your
Rem strategy guide?

Rem --- Part Two ---
Rem The Elf finishes helping with the tent and sneaks back over to you.
Rem "Anyway, the second column says how the round needs to end: X means you
Rem need to lose, Y means you need to end the round in a draw, and Z means you
Rem need to win. Good luck!"
Rem
Rem The total score is still calculated in the same way, but now you need to
Rem figure out what shape to choose so the round ends as indicated. The example
Rem above now goes like this:
Rem
Rem - In the first round, your opponent will choose Rock (A), and you need the
Rem   round to end in a draw (Y), so you also choose Rock. This gives you a
Rem   score of 1 + 3 = 4.
Rem - In the second round, your opponent will choose Paper (B), and you choose
Rem   Rock so you lose (X) with a score of 1 + 0 = 1.
Rem - In the third round, you will defeat your opponent's Scissors with Rock
Rem   for a score of 1 + 6 = 7.
Rem
Rem Now that you're correctly decrypting the ultra top secret strategy guide,
Rem you would get a total score of 12.
Rem
Rem Following the Elf's instructions for the second column, what would your
Rem total score be if everything goes exactly according to your strategy guide?

$Debug
Option _Explicit
Option _ExplicitArray

Const ROCK = "A"
Const PAPER = "B"
Const SCISSORS = "C"
Const HANDSHIFT = 65 Rem ASCII code for A

Rem Round data arrays
Dim Shared roundmax%
roundmax% = 9
Dim Shared rounddata$(0 To roundmax%)
Dim Shared roundcount%

Rem Open the input file and read into an array
Call ReadInput("input.txt")

'Rem Output the round data
'Call PrintRounds

Rem Simulate the tournament
Dim tournamentresult&
tournamentresult& = SimulateTournamentBadly
Print "The simulated tournament result is ";: Color 12: Print LTrim$(Str$(tournamentresult&));: Color 7

tournamentresult& = SimulateTournamentCorrectly
Print "The correctly simulated tournament result is ";: Color 12: Print LTrim$(Str$(tournamentresult&));: Color 7

Sub ReadInput (file$)
    Dim roundindex%
    roundindex% = 0

    Rem Open the file
    Dim filenum%
    filenum% = FreeFile
    Open file$ For Input As filenum%

    Rem Read until we get to the end of the file
    While Not EOF(filenum%)
        Dim fileline$
        Line Input #filenum%, fileline$



        Rem If this is beyond the end of our array, extend and increase the max
        If roundindex% > roundmax% Then
            roundmax% = roundmax% + 10
            ReDim _Preserve rounddata$(0 To roundmax%)
        End If

        Rem Update the elf calories
        'Print "Found " + fileline$ + " for round " + Str$(roundindex%)
        Dim calories&
        rounddata$(roundindex%) = fileline$

        roundindex% = roundindex% + 1
    Wend

    roundcount% = roundindex%
End Sub

Sub PrintRounds
    Dim roundindex%
    For roundindex% = 0 To roundcount% - 1
        Print "Round " + LTrim$(Str$(roundindex% + 1)) + ": " + rounddata$(roundindex%)
    Next
End Sub

Function SimulateTournamentBadly&
    Dim total&
    total& = 0

    Dim roundindex%
    For roundindex% = 0 To roundcount% - 1
        Dim theirhand$
        Dim ourhand$
        theirhand$ = Left$(rounddata$(roundindex%), 1)
        ourhand$ = Right$(rounddata$(roundindex%), 1)

        Rem Normalize our hand
        ourhand$ = Chr$(Asc(ourhand$) - 23)

        Rem Determine if we won or not
        Dim winscore%
        winscore% = 0
        If theirhand$ = ourhand$ Then
            Rem Draw
            winscore% = 3
        ElseIf theirhand$ = ROCK And ourhand$ = PAPER Then
            Rem Rock beaten by paper
            winscore% = 6
        ElseIf theirhand$ = PAPER And ourhand$ = SCISSORS Then
            Rem Paper beaten by scissors
            winscore% = 6
        ElseIf theirhand$ = SCISSORS And ourhand$ = ROCK Then
            Rem Scissors beaten by rock
            winscore% = 6
        End If

        Rem And the score for the hand we played
        Dim handscore%
        Select Case ourhand$
            Case ROCK: handscore% = 1
            Case PAPER: handscore% = 2
            Case SCISSORS: handscore% = 3
        End Select

        Rem Keep score
        Dim roundscore%
        roundscore% = handscore% + winscore%
        'Print "Round " + LTrim$(Str$(roundindex% + 1)) + " (" + rounddata$(roundindex%) + ") scored " + LTrim$(Str$(roundscore%)) + " (win: " + LTrim$(Str$(winscore%)) + ", hand: " + LTrim$(Str$(handscore%)) + ")"
        total& = total& + roundscore%
    Next

    SimulateTournamentBadly = total&
End Function

Function SimulateTournamentCorrectly&
    Dim total&
    total& = 0

    Dim roundindex%
    For roundindex% = 0 To roundcount% - 1
        Dim theirhand$
        Dim ourplay$
        theirhand$ = Left$(rounddata$(roundindex%), 1)
        ourplay$ = Right$(rounddata$(roundindex%), 1)

        Rem Determine what we should do
        Rem Finding the correct hand works by shifting the character ASCII code to 0, adding 1 or subtracting 1 (by adding 2) then the remainder (to wrap) is shifted back to the correct ASCII code
        Dim winscore%
        winscore% = 0
        Dim ourhand$
        Select Case ourplay$
            Case "X"
                Rem Lose
                ourhand$ = Chr$(((Asc(theirhand$) - HANDSHIFT + 2) Mod 3) + HANDSHIFT)
            Case "Y"
                Rem Draw
                ourhand$ = theirhand$
                winscore% = 3
            Case "Z"
                Rem Win
                ourhand$ = Chr$(((Asc(theirhand$) - HANDSHIFT + 1) Mod 3) + HANDSHIFT)
                winscore% = 6
        End Select

        Rem And the score for the hand we played
        Dim handscore%
        Select Case ourhand$
            Case ROCK: handscore% = 1
            Case PAPER: handscore% = 2
            Case SCISSORS: handscore% = 3
        End Select

        Rem Keep score
        Dim roundscore%
        roundscore% = handscore% + winscore%
        'Print "Round " + LTrim$(Str$(roundindex% + 1)) + " (" + rounddata$(roundindex%) + ") scored " + LTrim$(Str$(roundscore%)) + " (win: " + LTrim$(Str$(winscore%)) + ", hand: " + LTrim$(Str$(handscore%)) + ") with " + ourhand$
        total& = total& + roundscore%
    Next

    SimulateTournamentCorrectly = total&
End Function


