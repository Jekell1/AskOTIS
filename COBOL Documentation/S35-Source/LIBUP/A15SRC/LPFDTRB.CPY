      * COPYMEMBER: LIBLP/LPFDTRB
      ******************************************************************
      *               TR-CODE  =  "B"
      *               BRANCH (BULK) TRANSFERS
      *
      * REV:
      *  MJD 062895 ADDED TB-USERID, TB-POSTDATE, AND TB-POSTTIME.
      *  JTG 092096 ADDED TB-PLCD & TB-PLTYPE, FOR BULK TRANSFER OF P&L
      *  JTG 030797 ADDED TB-REPOCD, FOR BULK TRANSFER OF REPO ACCOUNTS
      *  MJD 990127 ----- UPDATED IN FLEXGEN -----
      ******************************************************************
       01  TB-REC.
           03  TB-DAY              PIC 99         COMP.
           03  TB-BRNO             PIC 9(4)       COMP.
           03  TB-CODE             PIC X.
           03  TB-CLASS            PIC 99         COMP.
           03  TB-NUMBER           PIC 9(6)       COMP.
           03  TB-SEQNO            PIC 99         COMP.
           03  TB-LAWCODE          PIC 99         COMP.
           03  TB-LINE             PIC 99         COMP.
           03  TB-FILLER2          PIC X(3).
      *-----------------------------------------------------------------
           03  TB-BORRTBL          OCCURS 4 TIMES.
               05  TB-SSNO         PIC 9(9)       COMP-3.
               05  TB-MAKERCD      PIC X.
      *-----------------------------------------------------------------
      *   TB-REF-FRTO --> T : TRANSFERRED TO ANOTHER BRANCH.
      *                   F : TRANSFERRED FROM ANOTHER BRANCH.
      *-----------------------------------------------------------------
           03  TB-REFNO.
               05  TB-REF-FRTO     PIC X.
               05  TB-REFBR        PIC 9999.
           03  TB-PURCD            PIC XXX.
           03  TB-SRCD             PIC XX.
           03  TB-COLLCD           PIC XX.
           03  TB-SERVACCT         PIC X.
           03  TB-LOANTYPE         PIC X.
      *-----------------------------------------------------------------
           03  TB-LOANDATE         PIC 9(6)       COMP-3.
      *-----------------------------------------------------------------
           03  TB-PROCEEDS         PIC S9(7)V99   COMP-3.
           03  TB-LNAMT            PIC S9(7)V99   COMP-3.
           03  TB-FINCHG           PIC S9(7)V99   COMP-3.
           03  TB-INTCHG           PIC 9(7)V99    COMP-3.
           03  TB-SERCHG           PIC 9(5)V99    COMP-3.
           03  TB-EXTCHG           PIC 9(5)V99    COMP-3.
           03  TB-MAINTFEE         PIC 999V99     COMP-3.
           03  TB-ORGTERM          PIC 9(3)       COMP-3.
      *-----------------------------------------------------------------
           03  TB-FEETBL           OCCURS 6 TIMES.
               05  TB-FEECODE      PIC XX.
               05  TB-FEEAMT       PIC S9(5)V99   COMP-3.
               05  TB-FEEOURS      PIC X.
      *-----------------------------------------------------------------
           03  TB-INSCODES.
               05  TB-INSURED      PIC X.
               05  TB-INSTYPES     PIC X.
                   88  TB-INSTYPES-VALID  VALUE "A" THRU "Z"
                                                   "0" "1" "2" "3" "4"
                                                   "5" "6" "7" "8" "9".
                   88  TB-INSTYPES-DL     VALUE "C"
                                                   "G" "I" "J" "M" "P"
                                                   "T" "X" "Y" "5" "6"
                                                   "7".
                   88  TB-INSTYPES-LL     VALUE "B"
                                                   "A" "F" "H" "L" "O"
                                                   "S" "U" "W" "2" "3"
                                                   "4".
                   88  TB-INSTYPES-AH     VALUE "D"
                                                   "F" "G" "K" "L" "M"
                                                   "O" "P" "Q" "V" "W"
                                                   "X" "3" "4" "6" "7"
                                                   "9" "0".
                   88  TB-INSTYPES-PP     VALUE "E"
                                                   "H" "J" "K" "L" "M"
                                                   "U" "Y" "Z" "3" "6"
                                                   "9".
                   88  TB-INSTYPES-PP2    VALUE "R"
                                                   "S" "T" "V" "W" "X"
                                                   "2" "4" "5" "7" "8"
                                                   "0".
                   88  TB-INSTYPES-O1     VALUE "1"
                                                   "A" "I" "O" "P" "Q"
                                                   "U" "Y" "Z" "2" "3"
                                                   "4" "5" "6" "7" "8"
                                                   "9" "0".
               05  TB-INSELIGIBLE  PIC X.
           03  TB-INSTBL           OCCURS 5 TIMES.
               05  TB-INSCOMP      PIC 99.
               05  TB-INSOURS      PIC X.
               05  TB-INSEFF       PIC 9(6)       COMP-3.
               05  TB-INSTERM      REDEFINES TB-INSEFF
                                   PIC 9(6)       COMP-3.
               05  TB-INSEXP       PIC 9(6)       COMP-3.
               05  TB-INSPREM      PIC S9(5)V99   COMP-3.
               05  TB-INSCOMM      PIC S9(6)V99   COMP-3.
      *-----------------------------------------------------------------
           03  TB-DLNO             PIC 9(4)V99    COMP-3.
           03  TB-DLRV             PIC S9(7)V99   COMP-3.
           03  TB-DLSPRV           PIC S9(7)V99   COMP-3.
           03  TB-DLDISC           PIC S9(7)V99   COMP-3.
           03  TB-DLPARV           PIC S9(7)V99   COMP-3.
           03  TB-DLRECOURSE       PIC X.
           03  TB-DLPARTRCAMT      PIC 9(6)V99    COMP-3.
           03  TB-PURBAL           PIC S9(7)V99   COMP-3.
      *-----------------------------------------------------------------
           03  TB-UNEARN           OCCURS 6 TIMES
                                   PIC S9(7)V99   COMP-3.
      *-----------------------------------------------------------------
           03  TB-REF-ACCTNO           PIC 9(8)       COMP-3.
      *-----------------------------------------------------------------

           03  TB-DLRVPRIN         OCCURS 3 TIMES
                                   PIC S9(8)V99    COMP-3.
           03  TB-DLRVCHGS         OCCURS 3 TIMES
                                   PIC S9(8)V99    COMP-3.
           03  TB-DLPARVPAID       PIC S9(8)V99    COMP-3.
           03  TB-DLPARVHELD       PIC S9(8)V99    COMP-3.
           03  TB-USERID           PIC X(10).
           03  TB-POSTDATE         PIC 9(6)        COMP-3.
           03  TB-POSTTIME         PIC 9(6)        COMP-3.
           03  TB-PLCD             PIC X.
           03  TB-PLTYPE           PIC X.
           03  TB-REPOCD           PIC X.
           03  TB-ACTIONCD2        PIC X(4).
           03  FILLER              PIC X(173).
