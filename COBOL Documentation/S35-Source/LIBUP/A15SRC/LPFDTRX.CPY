      * COPYMEMBER: LIBLP/LPFDTRX
      ************************************************************
      *               TR-CODE  =  "X"
      *               CLASS TRANSFERS
      *
      * REV:
      *  MJD 062795 ADDED TX-USERID, TX-POSTTIME, TX-POSTDATE
      *  MJD 990127 ----- UPDATED IN FLEXGEN -----
      ************************************************************
       01  TX-REC.
           03  TX-DAY              PIC 99         COMP.
           03  TX-BRNO             PIC 9(4)       COMP.
           03  TX-CODE             PIC X.
           03  TX-CLASS            PIC 99         COMP.
           03  TX-NUMBER           PIC 9(6)       COMP.
           03  TX-SEQNO            PIC 99         COMP.
           03  TX-LAWCODE          PIC 99         COMP.
           03  TX-LINE             PIC 99         COMP.
           03  TX-FILLER2          PIC X(3).
      *----------------------------------------------------
           03  TX-BORRTBL          OCCURS 4 TIMES.
               05  TX-SSNO         PIC 9(9)       COMP-3.
               05  TX-MAKERCD      PIC X.
      *----------------------------------------------------
           03  TX-PURCD            PIC XXX.
           03  TX-SRCD             PIC XX.
           03  TX-COLLCD           PIC XX.
           03  TX-SERVACCT         PIC X.
           03  TX-LOANTYPE         PIC X.
      *----------------------------------------------------
           03  TX-LOANDATE         PIC 9(6)       COMP-3.
      *----------------------------------------------------
           03  TX-PROCEEDS         PIC S9(7)V99   COMP-3.
           03  TX-LNAMT            PIC S9(7)V99   COMP-3.
           03  TX-FINCHG           PIC S9(7)V99   COMP-3.
           03  TX-INTCHG           PIC 9(7)V99    COMP-3.
           03  TX-SERCHG           PIC 9(5)V99    COMP-3.
           03  TX-EXTCHG           PIC 9(5)V99    COMP-3.
           03  TX-MAINTFEE         PIC 999V99     COMP-3.
           03  TX-ORGTERM          PIC 9(3)       COMP-3.
      *----------------------------------------------------
           03  TX-FEETBL           OCCURS 10 TIMES.
               05  TX-FEECODE      PIC XX.
               05  TX-FEEAMT       PIC S9(5)V99   COMP-3.
               05  TX-FEEOURS      PIC X.
      *----------------------------------------------------
           03  TX-INSCODES.
               05  TX-INSURED      PIC X.
               05  TX-INSTYPES     PIC X.
                   88  TX-INSTYPES-VALID   VALUE "A" THRU "Z"
                                                 "0" "1" "2" "3" "4"
                                                 "5" "6" "7" "8" "9".
                   88  TX-INSTYPES-DL      VALUE "C"
                                                 "G" "I" "J" "M" "P"
                                                 "T" "X" "Y" "5" "6"
                                                 "7".
                   88  TX-INSTYPES-LL      VALUE "B"
                                                 "A" "F" "H" "L" "O"
                                                 "S" "U" "W" "2" "3"
                                                 "4".
                   88  TX-INSTYPES-AH      VALUE "D"
                                                 "F" "G" "K" "L" "M"
                                                 "O" "P" "Q" "V" "W"
                                                 "X" "3" "4" "6" "7"
                                                 "9" "0".
                   88  TX-INSTYPES-PP      VALUE "E"
                                                 "H" "J" "K" "L" "M"
                                                 "U" "Y" "Z" "3" "6"
                                                 "9".
                   88  TX-INSTYPES-PP2     VALUE "R"
                                                 "S" "T" "V" "W" "X"
                                                 "2" "4" "5" "7" "8"
                                                 "0".
                   88  TX-INSTYPES-O1      VALUE "1"
                                                 "A" "I" "O" "P" "Q"
                                                 "U" "Y" "Z" "2" "3"
                                                 "4" "5" "6" "7" "8"
                                                 "9" "0".
               05  TX-INSELIGIBLE  PIC X.
                   88  TX-INSELIGIBLE-VALID  VALUE "A" THRU "Z"
                                                   "0" "1" "2" "3" "4"
                                                   "5" "6" "7" "8" "9".
                   88  TX-INSELIGIBLE-DL     VALUE "C"
                                                   "G" "I" "J" "M" "P"
                                                   "T" "X" "Y" "5" "6"
                                                   "7".
                   88  TX-INSELIGIBLE-LL     VALUE "B"
                                                   "A" "F" "H" "L" "O"
                                                   "S" "U" "W" "2" "3"
                                                   "4".
                   88  TX-INSELIGIBLE-AH     VALUE "D"
                                                   "F" "G" "K" "L" "M"
                                                   "O" "P" "Q" "V" "W"
                                                   "X" "3" "4" "6" "7"
                                                   "9" "0".
                   88  TX-INSELIGIBLE-PP     VALUE "E"
                                                   "H" "J" "K" "L" "M"
                                                   "U" "Y" "Z" "3" "6"
                                                   "9".
                   88  TX-INSELIGIBLE-PP2    VALUE "R"
                                                   "S" "T" "V" "W" "X"
                                                   "2" "4" "5" "7" "8"
                                                   "0".
                   88  TX-INSELIGIBLE-O1     VALUE "1"
                                                   "A" "I" "O" "P" "Q"
                                                   "U" "Y" "Z" "2" "3"
                                                   "4" "5" "6" "7" "8"
                                                   "9" "0".
           03  TX-INSTBL           OCCURS 10 TIMES.
               05  TX-INSCOMP      PIC 99.
               05  TX-INSOURS      PIC X.
               05  TX-INSEFF       PIC 9(6)       COMP-3.
               05  TX-INSTERM      REDEFINES TX-INSEFF
                                   PIC 9(6)       COMP-3.
               05  TX-INSEXP       PIC 9(6)       COMP-3.
               05  TX-INSPREM      PIC S9(5)V99   COMP-3.
               05  TX-INSCOMM      PIC S9(6)V99   COMP-3.
      *----------------------------------------------------
           03  TX-DLNO             PIC 9(4)V99    COMP-3.
           03  TX-DLRECOURSE       PIC X.
           03  TX-DLPARTRCAMT      PIC 9(6)V99    COMP-3.
           03  TX-PURBAL           PIC S9(7)V99   COMP-3.
           03  TX-DLRVPRIN         OCCURS 3
                                   PIC S9(7)V99   COMP-3.
           03  TX-DLRVCHGS         OCCURS 3
                                   PIC S9(6)V99   COMP-3.
           03  TX-DLPARVHELD       PIC S9(7)V99   COMP-3.
           03  TX-DLPARVPAID       PIC S9(6)V99   COMP-3.
           03  TX-DLDISC           PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TX-ANTICTERM        PIC 9(3)       COMP-3.
           03  TX-ANTICERN         OCCURS 6 TIMES
                                   PIC S9(7)V99   COMP-3.
           03  TX-EARNINGS         OCCURS 6 TIMES
                                   PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TX-PRLNNO           PIC 9(8)       COMP-3.
           03  TX-PRBAL            PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TX-TFRBAL           PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TX-SPRCLASS         PIC 99.
           03  TX-USERID           PIC X(10).
           03  TX-POSTDATE         PIC 9(6)       COMP-3.
           03  TX-POSTTIME         PIC 9(6)       COMP-3.
           03  FILLER              PIC X(36).
