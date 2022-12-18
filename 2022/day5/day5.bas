Rem Advent of Code 2022 day 5
Rem Deanna Earley - DeezyPuzzles
Rem BSD 3-clause Licence

Rem https://adventofcode.com/2022/day/5
Rem --- Day 5: Supply Stacks ---
Rem The expedition can depart as soon as the final supplies have been unloaded
Rem from the ships. Supplies are stored in stacks of marked crates, but because
Rem the needed supplies are buried under many other crates, the crates need to
Rem be rearranged.
Rem
Rem The ship has a giant cargo crane capable of moving crates between stacks. To
Rem ensure none of the crates get crushed or fall over, the crane operator will
Rem rearrange them in a series of carefully-planned steps. After the crates are
Rem rearranged, the desired crates will be at the top of each stack.
Rem
Rem The Elves don't want to interrupt the crane operator during this delicate
Rem procedure, but they forgot to ask her which crate will end up where, and
Rem they want to be ready to unload them as soon as possible so they can embark.
Rem
Rem They do, however, have a drawing of the starting stacks of crates and the
Rem rearrangement procedure (your puzzle input). For example:
Rem
Rem       [D]
Rem   [N] [C]
Rem   [Z] [M] [P]
Rem    1   2   3
Rem
Rem   move 1 from 2 to 1
Rem   move 3 from 1 to 3
Rem   move 2 from 2 to 1
Rem   move 1 from 1 to 2
Rem
Rem In this example, there are three stacks of crates. Stack 1 contains two
Rem crates: crate Z is on the bottom, and crate N is on top. Stack 2 contains
Rem three crates; from bottom to top, they are crates M, C, and D. Finally,
Rem stack 3 contains a single crate, P.
Rem
Rem Then, the rearrangement procedure is given. In each step of the procedure, a
Rem quantity of crates is moved from one stack to a different stack. In the
Rem first step of the above rearrangement procedure, one crate is moved from
Rem stack 2 to stack 1, resulting in this configuration:
Rem
Rem   [D]
Rem   [N] [C]
Rem   [Z] [M] [P]
Rem    1   2   3
Rem
Rem In the second step, three crates are moved from stack 1 to stack 3. Crates
Rem are moved one at a time, so the first crate to be moved (D) ends up below
Rem the second and third crates:
Rem
Rem           [Z]
Rem           [N]
Rem       [C] [D]
Rem       [M] [P]
Rem    1   2   3
Rem
Rem Then, both crates are moved from stack 2 to stack 1. Again, because crates
Rem are moved one at a time, crate C ends up below crate M:
Rem
Rem           [Z]
Rem           [N]
Rem   [M]     [D]
Rem   [C]     [P]
Rem    1   2   3
Rem
Rem Finally, one crate is moved from stack 1 to stack 2:
Rem
Rem           [Z]
Rem           [N]
Rem           [D]
Rem   [C] [M] [P]
Rem    1   2   3
Rem
Rem The Elves just need to know which crate will end up on top of each stack; in
Rem this example, the top crates are C in stack 1, M in stack 2, and Z in stack
Rem 3, so you should combine these together and give the Elves the message CMZ.
Rem
Rem After the rearrangement procedure completes, what crate ends up on top of
Rem each stack?

Rem --- Part Two ---
Rem As you watch the crane operator expertly rearrange the crates, you notice
Rem the process isn't following your prediction.
Rem
Rem Some mud was covering the writing on the side of the crane, and you quickly
Rem wipe it away. The crane isn't a CrateMover 9000 - it's a CrateMover 9001.
Rem
Rem The CrateMover 9001 is notable for many new and exciting features: air
Rem conditioning, leather seats, an extra cup holder, and the ability to pick up
Rem and move multiple crates at once.
Rem
Rem Again considering the example above, the crates begin in the same
Rem configuration:
Rem
Rem       [D]
Rem   [N] [C]
Rem   [Z] [M] [P]
Rem    1   2   3
Rem
Rem Moving a single crate from stack 2 to stack 1 behaves the same as before:
Rem
Rem   [D]
Rem   [N] [C]
Rem   [Z] [M] [P]
Rem    1   2   3
Rem
Rem However, the action of moving three crates from stack 1 to stack 3 means
Rem that those three moved crates stay in the same order, resulting in this new
Rem configuration:
Rem
Rem           [D]
Rem           [N]
Rem       [C] [Z]
Rem       [M] [P]
Rem    1   2   3
Rem
Rem Next, as both crates are moved from stack 2 to stack 1, they retain their
Rem order as well:
Rem
Rem           [D]
Rem           [N]
Rem   [C]     [Z]
Rem   [M]     [P]
Rem    1   2   3
Rem
Rem Finally, a single crate is still moved from stack 1 to stack 2, but now it's
Rem crate C that gets moved:
Rem
Rem           [D]
Rem           [N]
Rem           [Z]
Rem   [M] [C] [P]
Rem    1   2   3
Rem
Rem In this example, the CrateMover 9001 has put the crates in a totally
Rem different order: MCD.
Rem
Rem Before the rearrangement process finishes, update your simulation so that
Rem the Elves know where they should stand to be ready to unload the final
Rem supplies. After the rearrangement procedure completes, what crate ends up on
Rem top of each stack?

$Debug
Option _Explicit
Option _ExplicitArray

Const STACK_COUNT = 9
Dim Shared StackCrates(1 To STACK_COUNT) As String

Rem The file load loop has 2 states
Const STATE_LOADING = 0 Rem Loading the intial stacks
Const STATE_MOVING = 1 Rem Moving crates
Dim State As Integer
State = STATE_LOADING

Rem Part 2 needs a change of algorithm
Const CRATEMOVER_9000 = 9000
Const CRATEMOVER_9001 = 9001
Dim CraneModel As Integer
CraneModel = CRATEMOVER_9001

Open "input.txt" For Input As 1
While Not EOF(1)
    Rem Read each line
    Dim FileLine As String
    Line Input #1, FileLine
    'Print FileLine

    If FileLine = "" Then
        Rem Blank line, change state
        State = STATE_MOVING

        Print "Initial stacks"
        Call PrintStacks

    ElseIf State = STATE_LOADING Then
        Rem Load the initial stacks
        Dim StackIndex As Integer
        For StackIndex = 1 To STACK_COUNT
            Dim CharacterOffset As Integer
            CharacterOffset = (StackIndex * 4) - 2
            Dim CrateType As String
            CrateType = Mid$(FileLine, CharacterOffset, 1)

            Rem Ignore empty spaces and the stack numbers at the bottom of the diagram
            If CrateType >= "A" And CrateType <= "Z" Then
                StackCrates(StackIndex) = CrateType + StackCrates(StackIndex)
            End If
        Next

    ElseIf State = STATE_MOVING Then
        Rem Moving crates

        Dim MoveCount As Integer
        Dim MoveStackSize As Integer
        Dim OriginStackIndex As Integer
        Dim DestinationStackIndex As Integer

        Rem Parse the "move..." line
        Dim SpacePos As Integer
        SpacePos = InStr(6, FileLine, " ")
        MoveCount = Val(Mid$(FileLine, 6, SpacePos))
        OriginStackIndex = Val(Mid$(FileLine, SpacePos + 6, 1))
        DestinationStackIndex = Val(Mid$(FileLine, SpacePos + 11, 1))
        Print "Moving ", MoveCount, OriginStackIndex, DestinationStackIndex

        Rem Check which crane is being moved
        If CraneModel = CRATEMOVER_9000 Then
            Rem Move the crates 1 at a time
            Rem MoveCount stays as the read value
            MoveStackSize = 1

        ElseIf CraneModel = CRATEMOVER_9001 Then
            Rem Move the crates in bulk
            MoveStackSize = MoveCount
            MoveCount = 1

        End If

        Dim MoveIndex As Integer
        For MoveIndex = 0 To MoveCount - 1
            Dim MovedCrates As String
            Rem Get and remove the crate from the origin
            MovedCrates = Right$(StackCrates(OriginStackIndex), MoveStackSize)
            StackCrates(OriginStackIndex) = Left$(StackCrates(OriginStackIndex), Len(StackCrates(OriginStackIndex)) - MoveStackSize)

            Rem Place the crate on the destination stack
            StackCrates(DestinationStackIndex) = StackCrates(DestinationStackIndex) + MovedCrates
        Next

    End If
Wend

Print
Print "Final stacks"
PrintStacks

Rem Get the top crate on each stack
Dim TopCrates As String
For StackIndex = 1 To STACK_COUNT
    TopCrates = TopCrates + Right$(StackCrates(StackIndex), 1)
Next

Print "The top creates on each stack are '";: Color 12: Print TopCrates;: Color 7: Print "'"

Sub PrintStacks
    Dim StackIndex As Integer
    For StackIndex = 1 To STACK_COUNT
        Print "Stack " + LTrim$(Str$(StackIndex)) + ": " + StackCrates(StackIndex)
    Next
End Sub
