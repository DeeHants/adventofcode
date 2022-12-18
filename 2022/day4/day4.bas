Rem Advent of Code 2022 day 4
Rem Deanna Earley - DeezyPuzzles
Rem BSD 3-clause Licence

Rem https://adventofcode.com/2022/day/4
Rem --- Day 4: Camp Cleanup ---
Rem Space needs to be cleared before the last supplies can be unloaded from the
Rem ships, and so several Elves have been assigned the job of cleaning up
Rem sections of the camp. Every section has a unique ID number, and each Elf is
Rem assigned a range of section IDs.
Rem
Rem However, as some of the Elves compare their section assignments with each
Rem other, they've noticed that many of the assignments overlap. To try to
Rem quickly find overlaps and reduce duplicated effort, the Elves pair up and
Rem make a big list of the section assignments for each pair (your puzzle
Rem input).
Rem
Rem For example, consider the following list of section assignment pairs:
Rem
Rem   2-4,6-8
Rem   2-3,4-5
Rem   5-7,7-9
Rem   2-8,3-7
Rem   6-6,4-6
Rem   2-6,4-8
Rem
Rem For the first few pairs, this list means:
Rem
Rem - Within the first pair of Elves, the first Elf was assigned
Rem   sections 2-4 (sections 2, 3, and 4), while the second Elf was assigned
Rem   sections 6-8 (sections 6, 7, 8).
Rem - The Elves in the second pair were each assigned two sections.
Rem - The Elves in the third pair were each assigned three sections: one got
Rem   sections 5, 6, and 7, while the other also got 7, plus 8 and 9.
Rem
Rem This example list uses single-digit section IDs to make it easier to draw;
Rem your actual list might contain larger numbers. Visually, these pairs of
Rem section assignments look like this:
Rem
Rem   .234.....  2-4
Rem   .....678.  6-8
Rem
Rem   .23......  2-3
Rem   ...45....  4-5
Rem
Rem   ....567..  5-7
Rem   ......789  7-9
Rem
Rem   .2345678.  2-8
Rem   ..34567..  3-7
Rem
Rem   .....6...  6-6
Rem   ...456...  4-6
Rem
Rem   .23456...  2-6
Rem   ...45678.  4-8
Rem
Rem Some of the pairs have noticed that one of their assignments fully contains
Rem the other. For example, 2-8 fully contains 3-7, and 6-6 is fully contained
Rem by 4-6. In pairs where one assignment fully contains the other, one Elf in
Rem the pair would be exclusively cleaning sections their partner will already
Rem be cleaning, so these seem like the most in need of reconsideration. In this
Rem example, there are 2 such pairs.
Rem
Rem In how many assignment pairs does one range fully contain the other?

Rem --- Part Two ---
Rem It seems like there is still quite a bit of duplicate work planned. Instead,
Rem the Elves would like to know the number of pairs that overlap at all.
Rem
Rem In the above example, the first two pairs (2-4,6-8 and 2-3,4-5) don't
Rem overlap, while the remaining four pairs (5-7,7-9, 2-8,3-7, 6-6,4-6, and
Rem 2-6,4-8) do overlap:
Rem
Rem - 5-7,7-9 overlaps in a single section, 7.
Rem - 2-8,3-7 overlaps all of the sections 3 through 7.
Rem - 6-6,4-6 overlaps in a single section, 6.
Rem - 2-6,4-8 overlaps in sections 4, 5, and 6.
Rem
Rem So, in this example, the number of overlapping assignment pairs is 4.
Rem
Rem In how many assignment pairs do the ranges overlap?

$Debug
Option _Explicit
Option _ExplicitArray

Dim AssignmentFullOverlapCount As Long
Dim AssignmentOverlapCount As Long

Open "input.txt" For Input As 1
While Not EOF(1)
    Rem Read each line
    Dim FileLine As String
    Line Input #1, FileLine
    'Print FileLine

    Rem Split in half to get each elf's assignments
    Dim CommaPos As Integer
    CommaPos = InStr(FileLine, ",")
    Dim Elf1Range As String
    Dim Elf2Range As String
    Elf1Range = Left$(FileLine, CommaPos - 1)
    Elf2Range = Right$(FileLine, Len(FileLine) - CommaPos)
    'Print Elf1Range, Elf2Range

    Rem Get the range for each
    Rem Curse you not having split()!
    Dim Elf1Start As Integer
    Dim Elf1End As Integer
    Dim Elf1DashPos As Integer
    Elf1DashPos = InStr(Elf1Range, "-")
    Elf1Start = Val(Left$(Elf1Range, Elf1DashPos - 1))
    Elf1End = Val(Right$(Elf1Range, Len(Elf1Range) - Elf1DashPos))

    Dim Elf2Start As Integer
    Dim Elf2End As Integer
    Dim Elf2DashPos As Integer
    Elf2DashPos = InStr(Elf2Range, "-")
    Elf2Start = Val(Left$(Elf2Range, Elf2DashPos - 1))
    Elf2End = Val(Right$(Elf2Range, Len(Elf2Range) - Elf2DashPos))
    Print FileLine, Elf1Start, Elf1End, Elf2Start, Elf2End

    Rem Check if any of their assignments fully encompasses another
    If (Elf1Start >= Elf2Start And Elf1End <= Elf2End) Then
        Print "Elf 1 is fully repeated by elf 2"
        AssignmentFullOverlapCount = AssignmentFullOverlapCount + 1
    ElseIf (Elf2Start >= Elf1Start And Elf2End <= Elf1End) Then
        Print "Elf 2 is fully repeated by elf 1"
        AssignmentFullOverlapCount = AssignmentFullOverlapCount + 1
    ElseIf (Elf1Start >= Elf2Start And Elf1Start <= Elf2End) Then
        Print "Elf 1 is partially repeated by elf 2"
        AssignmentOverlapCount = AssignmentOverlapCount + 1
    ElseIf (Elf2Start >= Elf1Start And Elf2Start <= Elf1End) Then
        Print "Elf 2 is partially repeated by elf 1"
        AssignmentOverlapCount = AssignmentOverlapCount + 1
    End If

Wend

Print "The number of pairs with fully overlapping assignments is ";: Color 12: Print LTrim$(Str$(AssignmentFullOverlapCount)): Color 7
Print "The number of pairs with overlapping assignments is ";: Color 12: Print LTrim$(Str$(AssignmentOverlapCount + AssignmentFullOverlapCount)): Color 7

