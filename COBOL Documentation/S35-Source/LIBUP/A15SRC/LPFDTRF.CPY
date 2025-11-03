      * COPYMEMBER: LIBLP/LPFDTRF
      **************************************************************************
      *               TR-CODE  =  "F"
      *                 REVOLVING DIRECT LOAN
      *
      *  DESC: REVOLVING DIRECT LOAN TRANSACTION RECORD
      * REV:
      *  JTG 102898 NEW
      *  JTG 990611 ADDED TF-SP-LNCOST-FRMLA RE: TFC #008
      *  JTG 040501 ADDED LOGIC TO CAUSE THE UNEARNED LOAN COST TO ROLL TO
      *             THE NEW LOAN AFTER 'RENEWAL'. NOTE: THIS COST IS REFLECTIVE
      *             OF THE DISCOUNT WHEN PURCHASING LOAN PAPER NOT CARRIED AS
      *             SALES PAPER WITH DEALER DISCOUNT.
      *             ADDED SP-LNCOST-FRMLA = 'R'                    REGENCY #2287
      **************************************************************************

       01  TF-REC.
           03  TF-DAY              PIC 99         COMP.
           03  TF-BRNO             PIC 9(4)       COMP.
           03  TF-CODE             PIC X.
           03  TF-CLASS            PIC 99         COMP.
           03  TF-NUMBER           PIC 9(6)       COMP.
           03  TF-SEQNO            PIC 99         COMP.
           03  TF-LAWCODE          PIC 99         COMP.
           03  TF-LINE             PIC 99         COMP.
           03  TF-FILLER2          PIC X(3).
      *----------------------------------------------------
           03  TF-BORRTBL          OCCURS 4 TIMES.
               05  TF-SSNO         PIC 9(9)       COMP-3.
               05  TF-MAKERCD      PIC X.
      *----------------------------------------------------
           03  TF-PURCD            PIC XXX.
           03  TF-SRCD             PIC XX.
           03  TF-COLLCD           PIC XX.
           03  TF-SERVACCT         PIC X.
           03  TF-LOANTYPE         PIC X.
      *----------------------------------------------------
           03  TF-LOANDATE         PIC 9(6)       COMP-3.
           03  TF-INTDATE          PIC 9(6)       COMP-3.
      *----------------------------------------------------
           03  TF-PROCEEDS         PIC S9(7)V99   COMP-3.
           03  TF-LNAMT            PIC S9(7)V99   COMP-3.
           03  TF-FINCHG           PIC S9(7)V99   COMP-3.
           03  TF-INTCHG           PIC S9(7)V99   COMP-3.
           03  TF-SERCHG           PIC S9(5)V99   COMP-3.
           03  TF-EXTCHG           PIC S9(5)V99   COMP-3.
           03  TF-MAINTFEE         PIC S999V99    COMP-3.
           03  TF-ORGTERM          PIC 9(3)       COMP-3.
           03  TF-1STPYAMT         PIC S9(7)V99   COMP-3.
           03  TF-NOREGPY          PIC 9(3)       COMP-3.
           03  TF-REGPYAMT         PIC S9(7)V99   COMP-3.
           03  TF-LASTPYAMT        PIC S9(7)V99   COMP-3.
           03  TF-PROCASH          PIC S9(7)V99   COMP-3.
           03  TF-TILLNO           PIC X.
      *----------------------------------------------------
           03  TF-FEETBL           OCCURS 10 TIMES.
               05  TF-FEECODE      PIC XX.
               05  TF-FEEAMT       PIC S9(5)V99   COMP-3.
      *----------------------------------------------------
           03  TF-INSCODES.
               05  TF-INSURED          PIC X.
               05  TF-INSTYPES         PIC X.
                   88  TF-INSTYPES-VALID   VALUE "A" THRU "Z"
                                                 "0" "1" "2" "3" "4"
                                                 "5" "6" "7" "8" "9".
                   88  TF-INSTYPES-DL      VALUE "C"
                                                 "G" "I" "J" "M" "P"
                                                 "T" "X" "Y" "5" "6"
                                                 "7".
                   88  TF-INSTYPES-LL      VALUE "B"
                                                 "A" "F" "H" "L" "O"
                                                 "S" "U" "W" "2" "3"
                                                 "4".
                   88  TF-INSTYPES-AH      VALUE "D"
                                                 "F" "G" "K" "L" "M"
                                                 "O" "P" "Q" "V" "W"
                                                 "X" "3" "4" "6" "7"
                                                 "9" "0".
                   88  TF-INSTYPES-PP      VALUE "E"
                                                 "H" "J" "K" "L" "M"
                                                 "U" "Y" "Z" "3" "6"
                                                 "9".
                   88  TF-INSTYPES-PP2     VALUE "R"
                                                 "S" "T" "V" "W" "X"
                                                 "2" "4" "5" "7" "8"
                                                 "0".
                   88  TF-INSTYPES-O1      VALUE "1"
                                                 "A" "I" "O" "P" "Q"
                                                 "U" "Y" "Z" "2" "3"
                                                 "4" "5" "6" "7" "8"
                                                 "9" "0".
               05  TF-INSELIGIBLE  PIC X.                  
                   88  TF-INSELIGIBLE-VALID  VALUE "A" THRU "Z"
                                                   "0" "1" "2" "3" "4"
                                                   "5" "6" "7" "8" "9".
                   88  TF-INSELIGIBLE-DL     VALUE "C"
                                                   "G" "I" "J" "M" "P"
                                                   "T" "X" "Y" "5" "6"
                                                   "7".
                   88  TF-INSELIGIBLE-LL     VALUE "B"
                                                   "A" "F" "H" "L" "O"
                                                   "S" "U" "W" "2" "3"
                                                   "4".
                   88  TF-INSELIGIBLE-AH     VALUE "D"
                                                   "F" "G" "K" "L" "M"
                                                   "O" "P" "Q" "V" "W"
                                                   "X" "3" "4" "6" "7"
                                                   "9" "0".
                   88  TF-INSELIGIBLE-PP     VALUE "E"
                                                   "H" "J" "K" "L" "M"
                                                   "U" "Y" "Z" "3" "6"
                                                   "9".
                   88  TF-INSELIGIBLE-PP2    VALUE "R"
                                                   "S" "T" "V" "W" "X"
                                                   "2" "4" "5" "7" "8"
                                                   "0".
                   88  TF-INSELIGIBLE-O1     VALUE "1"
                                                   "A" "I" "O" "P" "Q"
                                                   "U" "Y" "Z" "2" "3"
                                                   "4" "5" "6" "7" "8"
                                                   "9" "0".
           03  TF-INSTBL           OCCURS 10 TIMES.
               05  TF-INSCOMP      PIC 99.
               05  TF-INSOURS      PIC X.
               05  TF-INSEFF       PIC 9(6)       COMP-3.
               05  TF-INSTERM      REDEFINES TF-INSEFF
                                   PIC 9(6)       COMP-3.
               05  TF-INSEXP       PIC 9(6)       COMP-3.
               05  TF-INSPREM      PIC S9(5)V99   COMP-3.
               05  TF-INSCOMM      PIC S9(6)V99   COMP-3.
      *----------------------------------------------------
           03  TF-DLNO             PIC 9(4)V99    COMP-3.
      *----------------------------------------------------
           03  TF-EARNINGS         OCCURS 6 TIMES
                                   PIC S9(7)V99   COMP-3.
           03  TF-ANTICERN         OCCURS 6 TIMES
                                   PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TF-PRLNNO           PIC 9(8)       COMP-3.
           03  TF-PRBAL            PIC S9(6)V99   COMP-3.
           03  TF-PRINTDUE         PIC S9(5)V99   COMP-3.
           03  TF-PRFINCHGRB       PIC S9(5)V99   COMP-3.
           03  TF-PRINSRB          PIC S9(5)V99   COMP-3.
      *----------------------------------------------------
      * NOTE: THIS FLAG IDENTIFIES HOW LPEOD5.C SHOULD
      *       UPDATE G/L WITH TF-STATE-WORKER:
      *       A - MISSISSIPPI REBL01.C
           03  TF-LPEOD5-FG        PIC X.
           03  TF-STATE-WORKER     PIC S9(7)V99   COMP-3.
           03  TF-RESTRICTD-ACCTNO PIC 9(8)       COMP-3.
           03  TF-SPRCLASS         PIC 99.
           03  TF-USERID           PIC X(10).
           03  TF-POSTDATE         PIC  9(6)     COMP-3.
           03  TF-POSTTIME         PIC  9(6)     COMP-3.
           03  TF-GIFTNET          PIC X.
           03  TF-APR              PIC 9(3)V999  COMP-3.
           03  TF-SP-LNCOST-FRMLA  PIC X.
2287       03  TF-PRDLDISC-UNEARN  PIC S9(7)V99   COMP-3.
           03  FILLER              PIC X(48).
