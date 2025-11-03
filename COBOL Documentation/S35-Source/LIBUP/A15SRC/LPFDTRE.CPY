      * COPYMEMBER: LIBLP/LPFDTRE
      ************************************************************************
      *               TR-CODE  =  "E"
      *           REVOLVING SALES FINANCE NEW INPUT
      * REV:
      *  JTG 102898 NEW
      *  MJD 990910 CHANGED LN-INSELIGIBLE-O3 TO BE LN-INSELIGIBLE-O1.
      *             AS PER JOHN, IT WAS MISNAMED FROM THE START.
      ************************************************************************
       01  TE-REC.
           03  TE-DAY              PIC 99         COMP.
           03  TE-BRNO             PIC 9(4)       COMP.
           03  TE-CODE             PIC X.
           03  TE-CLASS            PIC 99         COMP.
           03  TE-NUMBER           PIC 9(6)       COMP.
           03  TE-SEQNO            PIC 99         COMP.
           03  TE-LAWCODE          PIC 99         COMP.
           03  TE-LINE             PIC 99         COMP.
           03  TE-FILLER2          PIC X(3).
      *----------------------------------------------------
           03  TE-BORRTBL         OCCURS 4 TIMES.
               05  TE-SSNO         PIC 9(9)       COMP-3.
               05  TE-MAKERCD      PIC X.
      *----------------------------------------------------
           03  TE-PURCD            PIC XXX.
           03  TE-SRCD             PIC XX.
           03  TE-COLLCD           PIC XX.
           03  TE-SERVACCT         PIC X.
           03  TE-LOANTYPE         PIC X.
      *----------------------------------------------------
           03  TE-LOANDATE         PIC 9(6)       COMP-3.
           03  TE-INTDATE          PIC 9(6)       COMP-3.
      *----------------------------------------------------
           03  TE-PROCEEDS         PIC S9(7)V99   COMP-3.
           03  TE-LNAMT            PIC S9(7)V99   COMP-3.
           03  TE-FINCHG           PIC S9(7)V99   COMP-3.
           03  TE-INTCHG           PIC S9(7)V99   COMP-3.
           03  TE-SERCHG           PIC S9(5)V99   COMP-3.
           03  TE-EXTCHG           PIC S9(5)V99   COMP-3.
           03  TE-MAINTFEE         PIC S999V99    COMP-3.
           03  TE-ORGTERM          PIC 9(3)       COMP-3.
           03  TE-1STPYAMT         PIC S9(7)V99   COMP-3.
           03  TE-NOREGPY          PIC 9(3)       COMP-3.
           03  TE-REGPYAMT         PIC S9(7)V99   COMP-3.
           03  TE-LASTPYAMT        PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TE-FEETBL          OCCURS 10 TIMES.
               05  TE-FEECODE      PIC XX.
               05  TE-FEEAMT       PIC S9(5)V99   COMP-3.
               05  TE-FEEOURS      PIC X.
      *----------------------------------------------------
           03  TE-INSCODES.
               05  TE-INSURED      PIC X.
               05  TE-INSTYPES     PIC X.
                   88  TE-INSTYPES-VALID   VALUE "A" THRU "Z"
                                                 "0" "1" "2" "3" "4"
                                                 "5" "6" "7" "8" "9".
                   88  TE-INSTYPES-DL      VALUE "C"
                                                 "G" "I" "J" "M" "P"
                                                 "T" "X" "Y" "5" "6"
                                                 "7".
                   88  TE-INSTYPES-LL      VALUE "B"
                                                 "A" "F" "H" "L" "O"
                                                 "S" "U" "W" "2" "3"
                                                 "4".
                   88  TE-INSTYPES-AH      VALUE "D"
                                                 "F" "G" "K" "L" "M"
                                                 "O" "P" "Q" "V" "W"
                                                 "X" "3" "4" "6" "7"
                                                 "9" "0".
                   88  TE-INSTYPES-PP      VALUE "E"
                                                 "H" "J" "K" "L" "M"
                                                 "U" "Y" "Z" "3" "6"
                                                 "9".
                   88  TE-INSTYPES-PP2     VALUE "R"
                                                 "S" "T" "V" "W" "X"
                                                 "2" "4" "5" "7" "8"
                                                 "0".
                   88  TE-INSTYPES-O1      VALUE "1"
                                                 "A" "I" "O" "P" "Q"
                                                 "U" "Y" "Z" "2" "3"
                                                 "4" "5" "6" "7" "8"
                                                 "9" "0".
               05  TE-INSELIGIBLE  PIC X.                  
                   88  TE-INSELIGIBLE-VALID  VALUE "A" THRU "Z"
                                                   "0" "1" "2" "3" "4"
                                                   "5" "6" "7" "8" "9".
                   88  TE-INSELIGIBLE-DL     VALUE "C"
                                                   "G" "I" "J" "M" "P"
                                                   "T" "X" "Y" "5" "6"
                                                   "7".
                   88  TE-INSELIGIBLE-LL     VALUE "B"
                                                   "A" "F" "H" "L" "O"
                                                   "S" "U" "W" "2" "3"
                                                   "4".
                   88  TE-INSELIGIBLE-AH     VALUE "D"
                                                   "F" "G" "K" "L" "M"
                                                   "O" "P" "Q" "V" "W"
                                                   "X" "3" "4" "6" "7"
                                                   "9" "0".
                   88  TE-INSELIGIBLE-PP     VALUE "E"
                                                   "H" "J" "K" "L" "M"
                                                   "U" "Y" "Z" "3" "6"
                                                   "9".
                   88  TE-INSELIGIBLE-PP2    VALUE "R"
                                                   "S" "T" "V" "W" "X"
                                                   "2" "4" "5" "7" "8"
                                                   "0".
                   88  TE-INSELIGIBLE-O1     VALUE "1"
                                                   "A" "I" "O" "P" "Q"
                                                   "U" "Y" "Z" "2" "3"
                                                   "4" "5" "6" "7" "8"
                                                   "9" "0".
           03  TE-INSTBL          OCCURS 10 TIMES.
               05  TE-INSCOMP      PIC 99.
               05  TE-INSOURS      PIC X.
               05  TE-INSEFF       PIC 9(6)       COMP-3.
               05  TE-INSTERM     REDEFINES TE-INSEFF
                                   PIC 9(6)       COMP-3.
               05  TE-INSEXP       PIC 9(6)       COMP-3.
               05  TE-INSPREM      PIC S9(5)V99   COMP-3.
               05  TE-INSCOMM      PIC S9(6)V99   COMP-3.
      *----------------------------------------------------
           03  TE-DLNO             PIC 9(4)V99    COMP-3.
           03  TE-DLRECOURSE       PIC X.
           03  TE-DLPARTRCAMT      PIC S9(7)V99   COMP-3.
           03  TE-PURBAL           PIC S9(7)V99   COMP-3.
           03  TE-DLRVPRIN        OCCURS 3
                                   PIC S9(7)V99   COMP-3.
           03  TE-DLRVCHGS        OCCURS 3
                                   PIC S9(6)V99   COMP-3.
           03  TE-DLPARVHELD       PIC S9(7)V99   COMP-3.
           03  TE-DLPARVPAID       PIC S9(6)V99   COMP-3.
           03  TE-DLDISC           PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TE-ANTICTERM        PIC 9(3)       COMP-3.
           03  TE-ANTICERN        OCCURS 6 TIMES
                                   PIC S9(7)V99   COMP-3.
           03  TE-EARNINGS        OCCURS 6 TIMES
                                   PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TE-PRLNNO           PIC 9(8)       COMP-3.
           03  TE-INSPREM-DLADJ    PIC S9(5)V99   COMP-3.
           03  TE-PRDLDISC-UNEARN  PIC S9(7)V99   COMP-3.
           03  FILLER              PIC X(8).
      *----------------------------------------------------
           03  TE-DL-RVCHGS-CODE  OCCURS 3
                                   PIC X.
           03  TE-DL-PARVHELD-CODE PIC X.
           03  TE-DL-PARVPAID-CODE PIC X.
           03  TE-SPRCLASS         PIC 99.
           03  TE-O3INS-DLPROC     PIC S9(6)V99   COMP-3.
           03  TE-USERID           PIC X(10).
           03  TE-POSTDATE         PIC 9(6)       COMP.
           03  TE-POSTTIME         PIC 9(6)       COMP.
