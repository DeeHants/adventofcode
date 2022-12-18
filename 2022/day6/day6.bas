Rem Advent of Code 2022 day 6
Rem Deanna Earley - DeezyPuzzles
Rem BSD 3-clause Licence

Rem https://adventofcode.com/2022/day/6
Rem --- Day 6: Tuning Trouble ---
Rem The preparations are finally complete; you and the Elves leave camp on foot
Rem and begin to make your way toward the star fruit grove.
Rem
Rem As you move through the dense undergrowth, one of the Elves gives you a
Rem handheld device. He says that it has many fancy features, but the most
Rem important one to set up right now is the communication system.
Rem
Rem However, because he's heard you have significant experience dealing with
Rem signal-based systems, he convinced the other Elves that it would be okay to
Rem give you their one malfunctioning device - surely you'll have no problem
Rem fixing it.
Rem
Rem As if inspired by comedic timing, the device emits a few colorful sparks.
Rem
Rem To be able to communicate with the Elves, the device needs to lock on to
Rem their signal. The signal is a series of seemingly-random characters that the
Rem device receives one at a time.
Rem
Rem To fix the communication system, you need to add a subroutine to the device
Rem that detects a start-of-packet marker in the datastream. In the protocol
Rem being used by the Elves, the start of a packet is indicated by a sequence of
Rem four characters that are all different.
Rem
Rem The device will send your subroutine a datastream buffer (your puzzle
Rem input); your subroutine needs to identify the first position where the four
Rem most recently received characters were all different. Specifically, it needs
Rem to report the number of characters from the beginning of the buffer to the
Rem end of the first such four-character marker.
Rem
Rem For example, suppose you receive the following datastream buffer:
Rem
Rem   mjqjpqmgbljsphdztnvjfqwrcgsmlb
Rem
Rem After the first three characters (mjq) have been received, there haven't
Rem been enough characters received yet to find the marker. The first time a
Rem marker could occur is after the fourth character is received, making the
Rem most recent four characters mjqj. Because j is repeated, this isn't a
Rem marker.
Rem
Rem The first time a marker appears is after the seventh character arrives. Once
Rem it does, the last four characters received are jpqm, which are all
Rem different. In this case, your subroutine should report the value 7, because
Rem the first start-of-packet marker is complete after 7 characters have been
Rem processed.
Rem
Rem Here are a few more examples:
Rem
Rem - bvwbjplbgvbhsrlpgdmjqwftvncz: first marker after character 5
Rem - nppdvjthqldpwncqszvftbrmjlhg: first marker after character 6
Rem - nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg: first marker after character 10
Rem - zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw: first marker after character 11
Rem
Rem How many characters need to be processed before the first start-of-packet
Rem marker is detected?

Rem --- Part Two ---
Rem Your device's communication system is correctly detecting packets, but still
Rem isn't working. It looks like it also needs to look for messages.
Rem
Rem A start-of-message marker is just like a start-of-packet marker, except it
Rem consists of 14 distinct characters rather than 4.
Rem
Rem Here are the first positions of start-of-message markers for all of the
Rem above examples:
Rem
Rem - mjqjpqmgbljsphdztnvjfqwrcgsmlb: first marker after character 19
Rem - bvwbjplbgvbhsrlpgdmjqwftvncz: first marker after character 23
Rem - nppdvjthqldpwncqszvftbrmjlhg: first marker after character 23
Rem - nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg: first marker after character 29
Rem - zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw: first marker after character 26
Rem
Rem How many characters need to be processed before the first start-of-message
Rem marker is detected?

$Debug
Option _Explicit
Option _ExplicitArray

Const MARKER_LENGTH = 14

Open "input.txt" For Input As 1
While Not EOF(1)
    Rem Read each line
    Dim FileLine As String
    Line Input #1, FileLine
    Print FileLine

    Rem Find the start of signal marker
    Dim Buffer As String
    Buffer = ""
    Dim CharacterIndex As Integer
    For CharacterIndex = 1 To Len(FileLine)
        Dim Character As String
        Character = Mid$(FileLine, CharacterIndex, 1)

        Rem Check the buffer
        While InStr(Buffer, Character)
            Rem Character is in the buffer, remove everything up to it
            Buffer = Mid$(Buffer, 2)
        Wend
        Buffer = Right$(Buffer + Character, MARKER_LENGTH)
        Print "Found", Character, Buffer

        Rem Do we have a full marker?
        If Len(Buffer) = MARKER_LENGTH Then
            Print "Found last character of marker (" + Buffer + ") at ";: Color 12: Print LTrim$(Str$(CharacterIndex)): Color 7
            Exit For
        End If
    Next

    'Rem Pause
    'Dim x As String
    'Line Input x
Wend

