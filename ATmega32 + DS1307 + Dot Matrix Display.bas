'======================================================================='

' Title: LCD Display Clock * Dot Matrix Display
' Last Updated :  04.2022
' Author : A.Hossein.Khalilian
' Program code  : BASCOM-AVR 2.0.8.5
' Hardware req. : ATmega32 + DS1307 + Dot Matrix Display

'======================================================================='

$regfile = "M32def.dat"
$crystal = 12000000

Dim Iloop As Word
Dim Row As Word
Dim A As Word
Dim N As Word
Dim K As Word

Dim Ar1(512) As Byte
Dim Strg1 As String * 2
Dim Strg2 As String * 2
Dim Strg3 As String * 2
Dim Strg As String * 12
Dim Digits(12) As Byte At Strg Overlay

Ddra = &B11110000
Ddrb = 255
Ddrc = 255
Ddrd = 255

Config Scl = Porta.0
Config Sda = Porta.1

Const Ds1307w = &HD0
Const Ds1307r = &HD1

Dim _sec As Byte , _min As Byte , _hour As Byte
Dim _day As Byte , _month As Byte , _year As Byte
Dim Weekday As Byte , _min1 As Byte , _hour1 As Byte
Dim _weekday As Byte

'-----------------------------------------------------------

Do

 Gosub Getdatetime

 Gosub Getdatetime
 Strg1 = Bcd(_sec)
 Strg2 = Bcd(_min)
 Strg3 = Bcd(_hour)

 Strg = Strg3 + ":" + Strg2 + ":" + Strg1

 A = 1
 For Iloop = 1 To 8
   Gosub Filling
   For K = 1 To 8
      Read Ar1(a)
      Incr A
   Next
 Next

 For N = 1 To 100

      For Row = 0 To 7

          Portd = Ar1(1 + Row)
          Set Portc.0
          nop
          Reset Portc.0

          Portd = Ar1(9 + Row)
          Set Portc.1
          nop
          Reset Portc.1

          Portd = Ar1(17 + Row)
          Set Portc.2
          nop
          Reset Portc.2

          Portd = Ar1(25 + Row)
          Set Portc.3
          nop
          Reset Portc.3

          Portd = Ar1(33 + Row)
          Set Portc.4
          nop
          Reset Portc.4

          Portd = Ar1(41 + Row)
          Set Portc.5
          nop
          Reset Portc.5

          Portd = Ar1(49 + Row)
          Set Porta.5
          nop
          Reset Porta.5

          Portd = Ar1(57 + Row)
          Set Porta.6
          nop
          Reset Porta.6

        Reset Portb.row
        Waitms 1
        Set Portb.row

      Next

 Next

Loop
end

'-----------------------------------------------------------

Filling:

   Select Case Digits(iloop)

   Case 32 : Gosub Space_char
   Case 46 : Gosub Dot_char
   Case 47 : Gosub Forward_char
   Case 48 : Gosub 0_char
   Case 49 : Gosub 1_char
   Case 50 : Gosub 2_char
   Case 51 : Gosub 3_char
   Case 52 : Gosub 4_char
   Case 53 : Gosub 5_char
   Case 54 : Gosub 6_char
   Case 55 : Gosub 7_char
   Case 56 : Gosub 8_char
   Case 57 : Gosub 9_char
   Case 58 : Gosub Colon_char

   Case Else : Gosub Space_char

   End Select

 Return

''''''''''''''''''''''''''''''

Space_char:
 Restore Space_dta
 Return

Dot_char:
 Restore Point_dta
 Return

Forward_char:
 Restore Division_dta
 Return

0_char:
 Restore 0_dta
 Return

1_char:
 Restore 1_dta
 Return

2_char:
 Restore 2_dta
 Return

3_char:
 Restore 3_dta
 Return

4_char:
 Restore 4_dta
 Return

5_char:
 Restore 5_dta
 Return

6_char:
 Restore 6_dta
 Return

7_char:
 Restore 7_dta
 Return

8_char:
 Restore 8_dta
 Return

9_char:
 Restore 9_dta
 Return

Colon_char:
 Restore Colon_dta
 Return

''''''''''''''''''''''''''''''

Getdatetime:
I2cstart
I2cwbyte Ds1307w
I2cwbyte 0

I2cstart
I2cwbyte Ds1307r
I2crbyte _sec , Ack
I2crbyte _min , Ack
I2crbyte _hour , Ack
I2crbyte Weekday , Ack
I2crbyte _day , Ack
I2crbyte _month , Ack
I2crbyte _year , Nack
I2cstop
_min1 = Makedec(_min)
_hour1 = Makedec(_hour)

Return

''''''''''''''''''''''''''''''

Setdate:
_day = Makebcd(_day)
_month = Makebcd(_month)
_year = Makebcd(_year)
I2cstart
I2cwbyte Ds1307w
I2cwbyte 3
I2cwbyte _weekday
I2cwbyte _day
I2cwbyte _month
I2cwbyte _year
I2cstop
Return

''''''''''''''''''''''''''''''

Settime:
_sec = Makebcd(_sec)
_min = Makebcd(_min)
_hour = Makebcd(_hour)
I2cstart
I2cwbyte Ds1307w
I2cwbyte 0
I2cwbyte _sec
I2cwbyte _min
I2cwbyte _hour
I2cstop
Return

End

''''''''''''''''''''''''''''''

Space_dta:
Data &H00 , &H00 , &H00 , &H00 , &H00 , &H00 , &H00 , &H00

Point_dta:
Data &H00 , &H00 , &H00 , &H00 , &H00 , &H18 , &H18 , &H00

Division_dta:
Data &H06 , &H0C , &H18 , &H30 , &H60 , &HC0 , &H80 , &H00

0_dta:
Data &H7C , &HCE , &HDE , &HF6 , &HE6 , &HC6 , &H7C , &H00

1_dta:
Data &H30 , &H70 , &H30 , &H30 , &H30 , &H30 , &HFC , &H00

2_dta:
Data &H78 , &HCC , &H0C , &H38 , &H60 , &HCC , &HFC , &H00

3_dta:
Data &H78 , &HCC , &H0C , &H38 , &H0C , &HCC , &H78 , &H00

4_dta:
Data &H1C , &H3C , &H6C , &HCC , &HFE , &H0C , &H1E , &H00

5_dta:
Data &HFC , &HC0 , &HF8 , &H0C , &H0C , &HCC , &H78 , &H00

6_dta:
Data &H38 , &H60 , &HC0 , &HF8 , &HCC , &HCC , &H78 , &H00

7_dta:
Data &HFC , &HCC , &H0C , &H18 , &H30 , &H30 , &H30 , &H00

8_dta:
Data &H78 , &HCC , &HCC , &H78 , &HCC , &HCC , &H78 , &H00

9_dta:
Data &H78 , &HCC , &HCC , &H7C , &H0C , &H18 , &H70 , &H00

Colon_dta:
Data &H00 , &H18 , &H18 , &H00 , &H00 , &H18 , &H18 , &H00

'-----------------------------------------------------------