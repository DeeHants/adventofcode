Rem Advent of Code 2022 day 3
Rem Deanna Earley - DeezyPuzzles
Rem BSD 3-clause Licence

Rem https://adventofcode.com/2022/day/3
Rem --- Day 3: Rucksack Reorganization ---
Rem One Elf has the important job of loading all of the rucksacks with supplies
Rem for the jungle journey. Unfortunately, that Elf didn't quite follow the
Rem packing instructions, and so a few items now need to be rearranged.
Rem
Rem Each rucksack has two large compartments. All items of a given type are
Rem meant to go into exactly one of the two compartments. The Elf that did the
Rem packing failed to follow this rule for exactly one item type per rucksack.
Rem
Rem The Elves have made a list of all of the items currently in each rucksack
Rem (your puzzle input), but they need your help finding the errors. Every item
Rem type is identified by a single lowercase or uppercase letter (that is, a and
Rem A refer to different types of items).
Rem
Rem The list of items for each rucksack is given as characters all on a single
Rem line. A given rucksack always has the same number of items in each of its
Rem two compartments, so the first half of the characters represent items in the
Rem first compartment, while the second half of the characters represent items
Rem in the second compartment.
Rem
Rem For example, suppose you have the following list of contents from six
Rem rucksacks:
Rem
Rem   vJrwpWtwJgWrhcsFMMfFFhFp
Rem   jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
Rem   PmmdzqPrVvPwwTWBwg
Rem   wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
Rem   ttgJtRGJQctTZtZT
Rem   CrZsJsPPZsGzwwsLwLmpwMDw
Rem
Rem - The first rucksack contains the items vJrwpWtwJgWrhcsFMMfFFhFp, which
Rem   means its first compartment contains the items vJrwpWtwJgWr, while the
Rem   second compartment contains the items hcsFMMfFFhFp. The only item type
Rem   that appears in both compartments is lowercase p.
Rem - The second rucksack's compartments contain jqHRNqRjqzjGDLGL and
Rem   rsFMfFZSrLrFZsSL. The only item type that appears in both compartments is
Rem   uppercase L.
Rem - The third rucksack's compartments contain PmmdzqPrV and vPwwTWBwg; the
Rem   only common item type is uppercase P.
Rem - The fourth rucksack's compartments only share item type v.
Rem - The fifth rucksack's compartments only share item type t.
Rem - The sixth rucksack's compartments only share item type s.
Rem
Rem To help prioritize item rearrangement, every item type can be converted to a
Rem priority:
Rem
Rem - Lowercase item types a through z have priorities 1 through 26.
Rem - Uppercase item types A through Z have priorities 27 through 52.
Rem
Rem In the above example, the priority of the item type that appears in both
Rem compartments of each rucksack is 16 (p), 38 (L), 42 (P), 22 (v), 20 (t), and
Rem 19 (s); the sum of these is 157.
Rem
Rem Find the item type that appears in both compartments of each rucksack. What
Rem is the sum of the priorities of those item types?

Rem --- Part Two ---
Rem As you finish identifying the misplaced items, the Elves come to you with
Rem another issue.
Rem
Rem For safety, the Elves are divided into groups of three. Every Elf carries a
Rem badge that identifies their group. For efficiency, within each group of
Rem three Elves, the badge is the only item type carried by all three Elves.
Rem That is, if a group's badge is item type B, then all three Elves will have
Rem item type B somewhere in their rucksack, and at most two of the Elves will
Rem be carrying any other item type.
Rem
Rem The problem is that someone forgot to put this year's updated authenticity
Rem sticker on the badges. All of the badges need to be pulled out of the
Rem rucksacks so the new authenticity stickers can be attached.
Rem
Rem Additionally, nobody wrote down which item type corresponds to each group's
Rem badges. The only way to tell which item type is the right one is by finding
Rem the one item type that is common between all three Elves in each group.
Rem
Rem Every set of three lines in your list corresponds to a single group, but
Rem each group can have a different badge item type. So, in the above example,
Rem the first group's rucksacks are the first three lines:
Rem
Rem   vJrwpWtwJgWrhcsFMMfFFhFp
Rem   jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
Rem   PmmdzqPrVvPwwTWBwg
Rem
Rem And the second group's rucksacks are the next three lines:
Rem
Rem   wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
Rem   ttgJtRGJQctTZtZT
Rem   CrZsJsPPZsGzwwsLwLmpwMDw
Rem
Rem In the first group, the only item type that appears in all three rucksacks
Rem is lowercase r; this must be their badges. In the second group, their badge
Rem item type must be Z.
Rem
Rem Priorities for these items must still be found to organize the sticker
Rem attachment efforts: here, they are 18 (r) for the first group and 52 (Z) for
Rem the second group. The sum of these is 70.
Rem
Rem Find the item type that corresponds to the badges of each three-Elf group.
Rem What is the sum of the priorities of those item types?

$Debug
Option _Explicit
Option _ExplicitArray

Dim DuplicatePrioritySum As Long
DuplicatePrioritySum = 0

Dim ElfBag1 As String
Dim ElfBag2 As String
Dim ElfBag3 As String
Dim BadgePrioritySum As Long

Open "input.txt" For Input As 1
While Not EOF(1)
    Rem Read each line
    Dim FileLine As String
    Line Input #1, FileLine
    'Print FileLine

    Rem Split in half to get each compartment
    Dim Compartment1 As String
    Dim Compartment2 As String
    Compartment1 = Left$(FileLine, Len(FileLine) / 2)
    Compartment2 = Right$(FileLine, Len(FileLine) / 2)
    Print Compartment1, Compartment2
    'Print ItemPriority(Left$(Compartment1, 1))

    Dim CompartmentItemIndex As Integer
    For CompartmentItemIndex = 1 To Len(Compartment1)
        Dim CompartmentItemCode As String
        CompartmentItemCode = Mid$(Compartment1, CompartmentItemIndex, 1)
        'Print CompartmentItemCode, ItemPriority(CompartmentItemCode)

        If InStr(Compartment2, CompartmentItemCode) > 0 Then
            Print "Found duplicate: ", CompartmentItemCode, ItemPriority(CompartmentItemCode)
            DuplicatePrioritySum = DuplicatePrioritySum + ItemPriority(CompartmentItemCode)
            Rem Stop looking
            Exit For
        End If
    Next

    Rem Keep track of groups of 3 for badge calculation
    If ElfBag1 = "" Then
        ElfBag1 = FileLine
    ElseIf ElfBag2 = "" Then
        ElfBag2 = FileLine
    Else
        Rem 3rd elf! Determine the common item
        Dim BagItemIndex As Integer
        For BagItemIndex = 1 To Len(FileLine)
            Dim BagItemCode As String
            BagItemCode = Mid$(FileLine, BagItemIndex, 1)

            If InStr(ElfBag1, BagItemCode) > 0 And _
               InStr(ElfBag2, BagItemCode) > 0 Then
                Print "Found group badge: ", BagItemCode, ItemPriority(BagItemCode)
                BadgePrioritySum = BadgePrioritySum + ItemPriority(BagItemCode)
                Rem Stop looking
                Exit For
            End If
        Next

        Rem Clear the past bags for the next group
        ElfBag1 = ""
        ElfBag2 = ""
    End If
Wend

Print "The sum of the duplicated items is ";: Color 12: Print LTrim$(Str$(DuplicatePrioritySum)): Color 7
Print "The sum of the badges is ";: Color 12: Print LTrim$(Str$(BadgePrioritySum)): Color 7

Function ItemPriority% (ItemCode As String)
    If ItemCode >= "a" And ItemCode <= "z" Then
        Rem Map to 1 - 26
        ItemPriority = Asc(ItemCode) - 96
    ElseIf ItemCode >= "A" And ItemCode <= "Z" Then
        Rem Map to 27 - 52
        ItemPriority = Asc(ItemCode) - 38
    Else
        Error 5 Rem Bad input
    End If
End Function
