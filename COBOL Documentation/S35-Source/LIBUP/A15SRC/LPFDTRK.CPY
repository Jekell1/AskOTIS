      * COPYMEMBER: LIBLP/LPFDTRK
      **************************************************************************
      *               TR-CODE  =  "K"
      *                  CONVERSIONS
      *
      * REV:
      *  MJD 062895 ADDED TK-USERID, TK-POSTDATE, AND TK-POSTTIME.
      *  JTG 092096 ADDED TK-PLCD & TK-PLTYPE, FOR BULK TRANSFER OF P&L
      *  JTG 080129 CHANGED FOR JUDY/CS/JOHN TO ALLOW BULK-CONVERSION-FG 
      *             TO TAKE UPFRONT EARNINGS 'ADDED TK-EARNED'  FINAUTO    #3141
      **************************************************************************
       01  TK-REC.
           03  TK-KEY.
               05  TK-DAY              PIC 99         COMP.
               05  TK-BRNO             PIC 9(4)       COMP.
               05  TK-CODE             PIC X.
               05  TK-CLASS            PIC 99         COMP.
               05  TK-NUMBER           PIC 9(6)       COMP.
               05  TK-SEQNO            PIC 99         COMP.
               05  TK-LAWCODE          PIC 99         COMP.
               05  TK-LINE             PIC 99         COMP.
               05  TK-FILLER2          PIC X(3).
      *-----------------------------------------------------------------
           03  TK-DATA.
               05  TK-BORRTBL          OCCURS 4 TIMES.
                   07  TK-SSNO         PIC 9(9)       COMP-3.
                   07  TK-MAKERCD      PIC X.
      *-----------------------------------------------------------------
      *   TK-REF-FRTO --> T : TRANSFERRED TO ANOTHER BRANCH.
      *                   F : TRANSFERRED FROM ANOTHER BRANCH.
      *-----------------------------------------------------------------
               05  TK-REFNO.
                   07  TK-REF-FRTO     PIC X.
                   07  TK-REFBR        PIC 9999.
               05  TK-PURCD            PIC XXX.
               05  TK-SRCD             PIC XX.
               05  TK-COLLCD           PIC XX.
               05  TK-SERVACCT         PIC X.
               05  TK-LOANTYPE         PIC X.
               05  TK-REPOCD           PIC X.
               05  TK-PLCD             PIC X.
               05  TK-PLTYPE           PIC X.
      *-----------------------------------------------------------------
               05  TK-LOANDATE         PIC 9(6)       COMP-3.
      *-----------------------------------------------------------------
               05  TK-PROCEEDS         PIC S9(7)V99   COMP-3.
               05  TK-LNAMT            PIC S9(7)V99   COMP-3.
               05  TK-FINCHG           PIC S9(7)V99   COMP-3.
               05  TK-INTCHG           PIC 9(7)V99    COMP-3.
               05  TK-SERCHG           PIC 9(5)V99    COMP-3.
               05  TK-EXTCHG           PIC 9(5)V99    COMP-3.
               05  TK-MAINTFEE         PIC 999V99     COMP-3.
               05  TK-ORGTERM          PIC 9(3)       COMP-3.
      *-----------------------------------------------------------------
               05  TK-FEETBL           OCCURS 6 TIMES.
                   07  TK-FEECODE      PIC XX.
                   07  TK-FEEAMT       PIC S9(5)V99   COMP-3.
                   07  TK-FEEOURS      PIC X.
      *-----------------------------------------------------------------
               05  TK-INSCODES.
                   07  TK-INSURED      PIC X.
                   07  TK-INSTYPES     PIC X.
                       88  TK-INSTYPES-VALID  VALUE "A" THRU "Z"
                                                   "0" "1" "2" "3" "4"
                                                   "5" "6" "7" "8" "9".
                       88  TK-INSTYPES-DL     VALUE "C"
                                                   "G" "I" "J" "M" "P"
                                                   "T" "X" "Y" "5" "6"
                                                   "7".
                       88  TK-INSTYPES-LL     VALUE "B"
                                                   "A" "F" "H" "L" "O"
                                                   "S" "U" "W" "2" "3"
                                                   "4".
                       88  TK-INSTYPES-AH     VALUE "D"
                                                   "F" "G" "K" "L" "M"
                                                   "O" "P" "Q" "V" "W"
                                                   "X" "3" "4" "6" "7"
                                                   "9" "0".
                       88  TK-INSTYPES-PP     VALUE "E"
                                                   "H" "J" "K" "L" "M"
                                                   "U" "Y" "Z" "3" "6"
                                                   "9".
                       88  TK-INSTYPES-PP2    VALUE "R"
                                                   "S" "T" "V" "W" "X"
                                                   "2" "4" "5" "7" "8"
                                                   "0".
                       88  TK-INSTYPES-O1     VALUE "1"
                                                   "A" "I" "O" "P" "Q"
                                                   "U" "Y" "Z" "2" "3"
                                                   "4" "5" "6" "7" "8"
                                                   "9" "0".
                   07  TK-INSELIGIBLE  PIC X.
               05  TK-INSTBL           OCCURS 5 TIMES.
                   07  TK-INSCOMP      PIC 99.
                   07  TK-INSOURS      PIC X.
                   07  TK-INSEFF       PIC 9(6)       COMP-3.
                   07  TK-INSTERM      REDEFINES TK-INSEFF
                                   PIC 9(6)       COMP-3.
                   07  TK-INSEXP       PIC 9(6)       COMP-3.
                   07  TK-INSPREM      PIC S9(5)V99   COMP-3.
                   07  TK-INSCOMM      PIC S9(6)V99   COMP-3.
      *-----------------------------------------------------------------
               05  TK-DLNO             PIC 9(4)V99    COMP-3.
               05  TK-DLRV             PIC S9(7)V99   COMP-3.
               05  TK-DLSPRV           PIC S9(7)V99   COMP-3.
               05  TK-DLDISC           PIC S9(7)V99   COMP-3.
               05  TK-DLPARV           PIC S9(7)V99   COMP-3.
               05  TK-DLRECOURSE       PIC X.
               05  TK-DLPARTRCAMT      PIC 9(6)V99    COMP-3.
               05  TK-PURBAL           PIC S9(7)V99   COMP-3.
      *-----------------------------------------------------------------
               05  TK-UNEARN           OCCURS 6 TIMES
                                       PIC S9(7)V99   COMP-3.
      *-----------------------------------------------------------------
               05  TK-PRLNNO           PIC 9(8)       COMP-3.
      *-----------------------------------------------------------------

               05  TK-DLRVPRIN         OCCURS 3 TIMES
                                       PIC S9(8)V99    COMP-3.
               05  TK-DLRVCHGS         OCCURS 3 TIMES
                                       PIC S9(8)V99    COMP-3.
               05  TK-DLPARVPAID       PIC S9(8)V99    COMP-3.
               05  TK-DLPARVHELD       PIC S9(8)V99    COMP-3.
               05  TK-USERID           PIC X(10).
               05  TK-POSTDATE         PIC 9(6)        COMP-3.
               05  TK-POSTTIME         PIC 9(6)        COMP-3.
      *-----------------------------------------------------------------
               05  TK-EARNED           OCCURS 6 TIMES
                                       PIC S9(7)V99    COMP-3.
      *-----------------------------------------------------------------
               05  FILLER              PIC X(147).
