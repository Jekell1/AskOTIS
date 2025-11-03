      * COPYMEMBER: LIBLP/LPFDTRJ
      ******************************************************************
      *               TR-CODE  =  "J"
      *               CLASS TRANSFERS (WITH IN ONE BRANCH)
      *
      * REV:
      *  MJD 062895 ADDED TJ-USERID, TJ-POSTDATE, AND TJ-POSTTIME.
      *  JTG 092096 ADDED TJ-PLCD & TJ-PLTYPE, FOR CLASS TRANSFER OF P&L
      *  JTG 030797 ADDED TJ-REPOCD, FOR CLASS TRANSFER OF REPO
      *  BAH 052897 FIXED TJ-REFNO
      *  MJD 990127 ----- UPDATED IN FLEXGEN -----
      ******************************************************************
       01  TJ-REC.
           03  TJ-DAY              PIC 99         COMP.
           03  TJ-BRNO             PIC 9(4)       COMP.
           03  TJ-CODE             PIC X.
           03  TJ-CLASS            PIC 99         COMP.
           03  TJ-NUMBER           PIC 9(6)       COMP.
           03  TJ-SEQNO            PIC 99         COMP.
           03  TJ-LAWCODE          PIC 99         COMP.
           03  TJ-LINE             PIC 99         COMP.
           03  TJ-FILLER2          PIC X(3).
      *-----------------------------------------------------------------
           03  TJ-BORRTBL          OCCURS 4 TIMES.
               05  TJ-SSNO         PIC 9(9)       COMP-3.
               05  TJ-MAKERCD      PIC X.
           03  TJ-REFNO.
               05  TJ-REF-FRLC     PIC 99.
               05  TJ-REF-X        PIC X.
               05  TJ-REF-TOLC     PIC 99.
           03  TJ-PURCD            PIC XXX.
           03  TJ-SRCD             PIC XX.
           03  TJ-COLLCD           PIC XX.
           03  TJ-SERVACCT         PIC X.
           03  TJ-LOANTYPE         PIC X.
      *-----------------------------------------------------------------
           03  TJ-LOANDATE         PIC 9(6)       COMP-3.
      *-----------------------------------------------------------------
           03  TJ-PROCEEDS         PIC S9(7)V99   COMP-3.
           03  TJ-LNAMT            PIC S9(7)V99   COMP-3.
           03  TJ-FINCHG           PIC S9(7)V99   COMP-3.
           03  TJ-INTCHG           PIC 9(7)V99    COMP-3.
           03  TJ-SERCHG           PIC 9(5)V99    COMP-3.
           03  TJ-EXTCHG           PIC 9(5)V99    COMP-3.
           03  TJ-MAINTFEE         PIC 999V99     COMP-3.
           03  TJ-ORGTERM          PIC 9(3)       COMP-3.
      *-----------------------------------------------------------------
           03  TJ-FEETBL           OCCURS 6 TIMES.
               05  TJ-FEECODE      PIC XX.
               05  TJ-FEEAMT       PIC S9(5)V99   COMP-3.
               05  TJ-FEEOURS      PIC X.
      *-----------------------------------------------------------------
           03  TJ-INSCODES.
               05  TJ-INSURED      PIC X.
               05  TJ-INSTYPES     PIC X.
                   88  TJ-INSTYPES-VALID  VALUE "A" THRU "Z"
                                                   "0" "1" "2" "3" "4"
                                                   "5" "6" "7" "8" "9".
                   88  TJ-INSTYPES-DL     VALUE "C"
                                                   "G" "I" "J" "M" "P"
                                                   "T" "X" "Y" "5" "6"
                                                   "7".
                   88  TJ-INSTYPES-LL     VALUE "B"
                                                   "A" "F" "H" "L" "O"
                                                   "S" "U" "W" "2" "3"
                                                   "4".
                   88  TJ-INSTYPES-AH     VALUE "D"
                                                   "F" "G" "K" "L" "M"
                                                   "O" "P" "Q" "V" "W"
                                                   "X" "3" "4" "6" "7"
                                                   "9" "0".
                   88  TJ-INSTYPES-PP     VALUE "E"
                                                   "H" "J" "K" "L" "M"
                                                   "U" "Y" "Z" "3" "6"
                                                   "9".
                   88  TJ-INSTYPES-PP2    VALUE "R"
                                                   "S" "T" "V" "W" "X"
                                                   "2" "4" "5" "7" "8"
                                                   "0".
                   88  TJ-INSTYPES-O1     VALUE "1"
                                                   "A" "I" "O" "P" "Q"
                                                   "U" "Y" "Z" "2" "3"
                                                   "4" "5" "6" "7" "8"
                                                   "9" "0".
               05  TJ-INSELIGIBLE  PIC X.
           03  TJ-INSTBL           OCCURS 5 TIMES.
               05  TJ-INSCOMP      PIC 99.
               05  TJ-INSOURS      PIC X.
               05  TJ-INSEFF       PIC 9(6)       COMP-3.
               05  TJ-INSTERM      REDEFINES TJ-INSEFF
                                   PIC 9(6)       COMP-3.
               05  TJ-INSEXP       PIC 9(6)       COMP-3.
               05  TJ-INSPREM      PIC S9(5)V99   COMP-3.
               05  TJ-INSCOMM      PIC S9(6)V99   COMP-3.
      *-----------------------------------------------------------------
           03  TJ-DLNO             PIC 9(4)V99    COMP-3.
           03  TJ-DLRV             PIC S9(7)V99   COMP-3.
           03  TJ-DLSPRV           PIC S9(7)V99   COMP-3.
           03  TJ-DLDISC           PIC S9(7)V99   COMP-3.
           03  TJ-DLPARV           PIC S9(7)V99   COMP-3.
           03  TJ-DLRECOURSE       PIC X.
           03  TJ-DLPARTRCAMT      PIC 9(6)V99    COMP-3.
           03  TJ-PURBAL           PIC S9(7)V99   COMP-3.
      *-----------------------------------------------------------------
           03  TJ-UNEARN           OCCURS 6 TIMES
                                   PIC S9(7)V99   COMP-3.
      *-----------------------------------------------------------------
           03  TJ-PRLNNO           PIC 9(8)       COMP-3.
      *-----------------------------------------------------------------

           03  TJ-DLRVPRIN         OCCURS 3 TIMES
                                   PIC S9(8)V99    COMP-3.
           03  TJ-DLRVCHGS         OCCURS 3 TIMES
                                   PIC S9(8)V99    COMP-3.
           03  TJ-DLPARVPAID       PIC S9(8)V99    COMP-3.
           03  TJ-DLPARVHELD       PIC S9(8)V99    COMP-3.
           03  TJ-USERID           PIC X(10).
           03  TJ-POSTDATE         PIC 9(6)        COMP-3.
           03  TJ-POSTTIME         PIC 9(6)        COMP-3.
           03  TJ-PLCD             PIC X.
           03  TJ-PLTYPE           PIC X.
           03  TJ-REPOCD           PIC X.
           03  FILLER              PIC X(177).
