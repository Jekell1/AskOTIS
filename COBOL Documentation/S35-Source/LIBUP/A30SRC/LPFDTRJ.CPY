      * COPYMEMBER: LIBLP/LPFDTRJ.CPY
      ******************************************************************
      *
      * DESCRIPTION:  DAILY TRANSACTION FILE [CLASS TRANSFERS]
      *
      * FILE INFO  :  PRIMARY LAYOUT IN LIBLP/LPFDTR.CPY
      *               TR1-CODE = J - CLASS TRANSFER (WITH IN ONE BRANCH)
      *               LOCAL ED/TR###  (####=MMYY)              RSZ=950
      *
      *=================================================================
      * REV:
      * MJD 062895 ADDED TJ-USERID, TJ-POSTDATE, AND TJ-POSTTIME.
      * JTG 092096 ADDED TJ-PLCD & TJ-PLTYPE, FOR CLASS TRANSFER OF P&L
      * JTG 030797 ADDED TJ-REPOCD, FOR CLASS TRANSFER OF REPO
      * BAH 052897 FIXED TJ-REFNO
      * MJD 990127 ----- UPDATED IN FLEXGEN -----
      *  CS 160914 REMOVED TJ-SEQNO
      *  CS 160915 CHANGED TJ-CLASS 9(2) TO 9(3), TJ-NUMBER IS REDEFINE
      *             OF TJ-ACCTNO 9(8), MADE TJ-REF-FRCL/TJ-REF-TOLC
      *             SEPARATE FIELDS CLASS 9(3) NOW
      *            TJ-LOANDATE 9(6) TO 9(8)
      *            TJ-MAINTFEE 9(3)V99 TO 9(5)V99
      *            REMOVED TJ-FEETBL 
      *            REMOVED TJ-INSTYPES,TJ-INSELIBIBLE AND TJ-INSTBL, ITS
      *             NOT UPDATED ON CLASS TRANSFER
      *            ADDED TJ-INSUEARN OCCURS 10 S9(7)V99 WHEN WE UPDATE
      *             INSURANCE EARNINGS TO LOAN RECORD
      *            CHANGED TJ-UNEARN OCCURS 6 TO 10 FOR NEW EARNINGS
      *            CHANGED TJ-DLRVPRIN,TJ-DLRVCHGS,TJ-DLAPRVPAID,
      *             TRJ-DLRVCHGS FROM 9(8)V99 TO 9(7)V99
      *            CHANGED TJ-POSTDATE 9(6) TO 9(8)
      *            REMOVE COMP FIELDS FROM KEY
      *            SAVED VERSION LIBLP/LPFDTRJ.160921
      *  CS 161117 ADD TJ-SUBCLASS
      *  CS 170713 CHANGED TJ-LINE TO PIC 9(3) TO MATCH WK2-LINE, IN A15
      *            WAS COMP 9(2) WHICH ALLOWED A VALUE OF 248, I DONT THINK
      *            WE ALLOWED LINE >99 BUT CHANGED TO 999. FILLER TO 4
      *            SAVED LPFDTRJ.170713
      * BAH 180125 REDUCED FILLER AND FILE SIZE PER KM
      ******************************************************************
       01  TJ-REC.
           03  TJ-TRJ-KEY.
               05  TJ-DAY              PIC 99.
               05  TJ-BRNO             PIC 9(4).
               05  TJ-CODE             PIC X.

               05  TJ-CLASS            PIC 999.

               05  TJ-ACCTNO           PIC 9(8).
               05  TJ-LAWCODE          PIC 99.
               05  TJ-LINE             PIC 999.
               05  TJ-FILLER           PIC X(4).
      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  TJ-LTOUCH-DATE      PIC 9(8) COMP-3.

           03  TJ-BORRTBL          OCCURS 4 TIMES.
               05  TJ-SSNO         PIC 9(9)       COMP-3.
               05  TJ-MAKERCD      PIC X.
      *%%CHANGED TJ-REF-FRLC,TJ-REF-TOLC FROM 99 TO 999
           03  TJ-REFNO.                            
               05  TJ-REF-FRLC     PIC 99.
               05  TJ-REF-X        PIC X.
               05  TJ-REF-TOLC     PIC 99.

      * NEEDS TO BE SEPARATE FIELDS SINCE CLASS IS 9(3)
           03  TJ-FRLC         PIC 9(3) COMP-3.
           03  TJ-TOLC         PIC 9(3) COMP-3.

           03  TJ-PURCD            PIC XXX.
           03  TJ-SRCD             PIC XX.
           03  TJ-COLLCD           PIC XX.
           03  TJ-SERVACCT         PIC X.
           03  TJ-LOANTYPE         PIC X.
           03  TJ-SUBCLASS         PIC 99.
      *-----------------------------------------------------------------

      *>>XDD DATE=YYYYMMDD
           03  TJ-LOANDATE         PIC 9(8)       COMP-3.
      *-----------------------------------------------------------------
           03  TJ-PROCEEDS         PIC S9(7)V99   COMP-3.
           03  TJ-LNAMT            PIC S9(7)V99   COMP-3.
           03  TJ-FINCHG           PIC S9(7)V99   COMP-3.
           03  TJ-INTCHG           PIC 9(7)V99    COMP-3.
           03  TJ-SERCHG           PIC 9(5)V99    COMP-3.
           03  TJ-EXTCHG           PIC 9(5)V99    COMP-3.

           03  TJ-MAINTFEE         PIC 9(5)V99    COMP-3.
           03  TJ-ORGTERM          PIC 9(3)       COMP-3.
      *-----------------------------------------------------------------

      *TB-FEETBL WAS OCCURS 6, AND IN A15 THE FEE TABLE OCCURS 10,DID A CLASS 
      *%TRANSFER WITH INSURANCE AND FEE CODES AND THE FEECODES DID NOT TRANSFER
      *%%TO TRJ, MAKES SENSE THAT IT DIDNT OR EOD WOULD HAVE UDPATED GL.
      *  LEFT THESE COMMENTED IN CASE WE HAD TO ADD BACK TO THE RECORD
      *  THEY ARE NOT UPDATED
      *    03  TJ-FEETBL           OCCURS 6 TIMES.
      *        05  TJ-FEECODE      PIC XX.
      *        05  TJ-FEEAMT       PIC S9(5)V99   COMP-3.
      *        05  TJ-FEEOURS      PIC X.
      *-----------------------------------------------------------------
      *%TJ-INSTBL OCCURED 5 AND ITS NOT BEING UPDATED ON CLASS TRANSFER
      *%TJ-INSURED IS THE ONLY FIELD BEING UPDATED
           03  TJ-INSCODES.
               05  TJ-INSURED      PIC X.
      *        05  TJ-INSTYPES     PIC X.
      *            88  TJ-INSTYPES-VALID  VALUE "A" THRU "Z"
      *                                            "0" "1" "2" "3" "4"
      *                                            "5" "6" "7" "8" "9".
      *            88  TJ-INSTYPES-DL     VALUE "C"
      *                                            "G" "I" "J" "M" "P"
      *                                            "T" "X" "Y" "5" "6"
      *                                            "7".
      *            88  TJ-INSTYPES-LL     VALUE "B"
      *                                            "A" "F" "H" "L" "O"
      *                                            "S" "U" "W" "2" "3"
      *                                            "4".
      *            88  TJ-INSTYPES-AH     VALUE "D"
      *                                            "F" "G" "K" "L" "M"
      *                                            "O" "P" "Q" "V" "W"
      *                                            "X" "3" "4" "6" "7"
      *                                            "9" "0".
      *            88  TJ-INSTYPES-PP     VALUE "E"
      *                                            "H" "J" "K" "L" "M"
      *                                            "U" "Y" "Z" "3" "6"
      *                                            "9".
      *            88  TJ-INSTYPES-PP2    VALUE "R"
      *                                            "S" "T" "V" "W" "X"
      *                                            "2" "4" "5" "7" "8"
      *                                            "0".
      *            88  TJ-INSTYPES-O1     VALUE "1"
      *                                            "A" "I" "O" "P" "Q"
      *                                            "U" "Y" "Z" "2" "3"
      *                                            "4" "5" "6" "7" "8"
      *                                            "9" "0".
      *        05  TJ-INSELIGIBLE  PIC X.

      *  LEFT THESE COMMENTED IN CASE WE HAD TO ADD BACK TO THE RECORD
      *  THEY ARE NOT UPDATED
      *    03  TJ-INSTBL           OCCURS 5 TIMES.
      *        05  TJ-INSCOMP      PIC 99.
      *        05  TJ-INSOURS      PIC X.
      *        05  TJ-INSEFF       PIC 9(6)       COMP-3.
      *        05  TJ-INSTERM      REDEFINES TJ-INSEFF
      *                            PIC 9(6)       COMP-3.
      *        05  TJ-INSEXP       PIC 9(6)       COMP-3.
      *        05  TJ-INSPREM      PIC S9(5)V99   COMP-3.
      *        05  TJ-INSCOMM      PIC S9(6)V99   COMP-3.



      *NEW WHEN WE STORE INSURANCE UNEARNED
           03  TJ-INSUNEARN OCCURS 10
                                    PIC S9(7)V99 COMP-3.

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

           03  TJ-UNEARN           OCCURS 10 TIMES
                                   PIC S9(7)V99   COMP-3.
      *-----------------------------------------------------------------
           03  TJ-PRLNNO           PIC 9(8)       COMP-3.
      *-----------------------------------------------------------------


           03  TJ-DLRVPRIN         OCCURS 3 TIMES
                                   PIC S9(7)V99    COMP-3.
           03  TJ-DLRVCHGS         OCCURS 3 TIMES
                                   PIC S9(7)V99    COMP-3.
           03  TJ-DLPARVPAID       PIC S9(7)V99    COMP-3.
           03  TJ-DLPARVHELD       PIC S9(7)V99    COMP-3.
           03  TJ-USERID           PIC X(10).

      *>>XDD DATE=YYYYMMDD
           03  TJ-POSTDATE         PIC 9(8)        COMP-3.
           03  TJ-POSTTIME         PIC 9(6)        COMP-3.
           03  TJ-PLCD             PIC X.
           03  TJ-PLTYPE           PIC X.
           03  TJ-REPOCD           PIC X.
           03  FILLER              PIC X(632).
