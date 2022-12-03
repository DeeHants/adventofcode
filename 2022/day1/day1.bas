Rem Advent of Code 2022 day 1
Rem Deanna Earley - DeezyPuzzles
Rem BSD 3-clause Licence

Rem https://adventofcode.com/2022/day/1
Rem --- Day 1: Calorie Counting ---
Rem Santa's reindeer typically eat regular reindeer food, but they need a lot
Rem of magical energy to deliver presents on Christmas. For that, their
Rem favorite snack is a special type of star fruit that only grows deep in the
Rem jungle. The Elves have brought you on their annual expedition to the grove
Rem where the fruit grows.
Rem
Rem To supply enough magical energy, the expedition needs to retrieve a minimum
Rem of fifty stars by December 25th. Although the Elves assure you that the
Rem grove has plenty of fruit, you decide to grab any fruit you see along the
Rem way, just in case.
Rem
Rem Collect stars by solving puzzles. Two puzzles will be made available on
Rem each day in the Advent calendar; the second puzzle is unlocked when you
Rem complete the first. Each puzzle grants one star. Good luck!
Rem
Rem The jungle must be too overgrown and difficult to navigate in vehicles or
Rem access from the air; the Elves' expedition traditionally goes on foot. As
Rem your boats approach land, the Elves begin taking inventory of their
Rem supplies. One important consideration is food - in particular, the number
Rem of Calories each Elf is carrying (your puzzle input).
Rem
Rem The Elves take turns writing down the number of Calories contained by the
Rem various meals, snacks, rations, etc. that they've brought with them, one
Rem item per line. Each Elf separates their own inventory from the previous
Rem Elf's inventory (if any) by a blank line.
Rem
Rem For example, suppose the Elves finish writing their items' Calories and end
Rem up with the following list:
Rem
Rem   1000
Rem   2000
Rem   3000
Rem
Rem   4000
Rem
Rem   5000
Rem   6000
Rem
Rem   7000
Rem   8000
Rem   9000
Rem
Rem   10000
Rem
Rem This list represents the Calories of the food carried by five Elves:
Rem
Rem - The first Elf is carrying food with 1000, 2000, and 3000 Calories, a
Rem   total of 6000 Calories.
Rem - The second Elf is carrying one food item with 4000 Calories.
Rem - The third Elf is carrying food with 5000 and 6000 Calories, a total of
Rem   11000 Calories.
Rem - The fourth Elf is carrying food with 7000, 8000, and 9000 Calories, a
Rem   total of 24000 Calories.
Rem - The fifth Elf is carrying one food item with 10000 Calories.
Rem
Rem In case the Elves get hungry and need extra snacks, they need to know which
Rem Elf to ask: they'd like to know how many Calories are being carried by the
Rem Elf carrying the most Calories. In the example above, this is 24000
Rem (carried by the fourth Elf).
Rem
Rem Find the Elf carrying the most Calories. How many total Calories is that
Rem Elf carrying?

Rem --- Part Two ---
Rem By the time you calculate the answer to the Elves' question, they've
Rem already realized that the Elf carrying the most Calories of food might
Rem eventually run out of snacks.
Rem
Rem To avoid this unacceptable situation, the Elves would instead like to know
Rem the total Calories carried by the top three Elves carrying the most
Rem Calories. That way, even if one of those Elves runs out of snacks, they
Rem still have two backups.
Rem
Rem In the example above, the top three Elves are the fourth Elf (with 24000
Rem Calories), then the third Elf (with 11000 Calories), then the fifth Elf
Rem (with 10000 Calories). The sum of the Calories carried by these three elves
Rem is 45000.
Rem
Rem Find the top three Elves carrying the most Calories. How many Calories are
Rem those Elves carrying in total?

$Debug
Option _Explicit
Option _ExplicitArray

Rem Elf data arrays
Dim Shared elfmax%
elfmax% = 9
Dim Shared elfcalories&(0 To elfmax%)
Dim Shared elfcount%

Rem Open the input file and read into an array splitting on blank lines
Call ReadInput("input.txt")

Rem Output the elves and their calories
Call PrintElves

Rem Find the elf with the highest calorie count
Dim highelfindex%
highelfindex% = FindHighestElf%

Print "The elf with the highest calories is Elf " + LTrim$(Str$(highelfindex% + 1)) + " with ";: Color 12: Print LTrim$(Str$(elfcalories&(highelfindex%)));: Color 7: Print " calories"

Rem Find the sum of the top 3 elves
Rem Do this (destructively) by blanking the top ones each time and searching again
Dim top3calories&
top3calories& = elfcalories&(highelfindex%)
elfcalories&(highelfindex%) = -1

Rem Search for the 2nd highest
highelfindex% = FindHighestElf%
top3calories& = top3calories& + elfcalories&(highelfindex%)
elfcalories&(highelfindex%) = -1

Rem And 3rd highest
highelfindex% = FindHighestElf%
top3calories& = top3calories& + elfcalories&(highelfindex%)
elfcalories&(highelfindex%) = -1

Print "The sum of the top 3 elves is ";: Color 12: Print LTrim$(Str$(top3calories&));: Color 7: Print " calories"


Sub ReadInput (file$)
    Dim elfindex%
    elfindex% = 0

    Rem Open the file
    Dim filenum%
    filenum% = FreeFile
    Open file$ For Input As filenum%

    Rem Read until we get to the end of the file
    While Not EOF(filenum%)
        Dim fileline$
        Line Input #filenum%, fileline$

        Rem If it's a blank line, increment to the next elf
        If fileline$ = "" Then
            elfindex% = elfindex% + 1

            Rem If this is beyond the end of our array, extend and increase the max
            If elfindex% > elfmax% Then
                elfmax% = elfmax% + 10
                ReDim _Preserve elfcalories&(0 To elfmax%)
            End If
        End If

        Rem Update the elf calories
        'Print "Found " + Str$(Val(fileline$)) + " for elf " + Str$(elfindex%)
        Dim calories&
        calories& = elfcalories&(elfindex%)
        calories& = calories& + Val(fileline$)
        elfcalories&(elfindex%) = calories&
    Wend

    elfcount% = elfindex% + 1
End Sub

Sub PrintElves
    Dim elfindex%
    For elfindex% = 0 To elfcount% - 1
        Print "Elf " + LTrim$(Str$(elfindex% + 1)) + ": " + LTrim$(Str$(elfcalories&(elfindex%)))
    Next
End Sub

Function FindHighestElf%
    Dim highelf%
    highelf% = -1
    Dim highcalories&
    highcalories& = 0

    Dim elfindex%
    For elfindex% = 0 To elfcount% - 1
        Dim calories&
        calories& = elfcalories&(elfindex%)
        If calories& > highcalories& Then
            highelf% = elfindex%
            highcalories& = calories&
        End If
    Next

    FindHighestElf% = highelf%
End Function
