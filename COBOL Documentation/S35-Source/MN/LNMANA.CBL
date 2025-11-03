       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNMANA.
      ******************************************************************
      *
      *   DIR :  MN
      *   DESC: LOAN MASTER FILE MAINTENANCE   PAGE 10
      *
      *=================================================================
      * REV:
      * MJD 112197 EXPANDED LN-REC TO 1800 BYTES
      * JTG 082198 ADDED LN-PRDLDISC-UNEARN FOR MERCURY TO INDICATED
      *            UNEARNED DISCOUNT CARRIED OVER FROM THE RENEWED LOAN
      * KEC 090398 DISPLAYING LN-REVOLVING FOR 'LNSTAT'.
      * KEC 091598 ADDED LOGIC TO ACCESS NEW PAGE 11.
      * JTG 120198 ADDED LOGIC TO ACCESS NEW PAGE 12.
      * MG  010228 ADDED 3 NEW FIELDS TO THE MAINTENANCE PER JOHN & KEN
      * JTG 010227 ADDED LN-EXTRA-PRIN-LFP LN-REMAIN-INT-SINCE-LFP
      *            LN-REMAIN-PRIN-SINCE-LFP  (LFP=LAST-FULL-PAYMENT)
      *            LENDMARK, PR#1045 & PR#1380
      * MG  010621 {DR#0001} MOVED 3 FIELDS FORM PAGE 12 TO PAGE 10.
      * BAH 011029 ADDED LN-INTERIM-COUPON FOR REGACC, PR# 1632
      * JTG 020715 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *             ADDED 23. LN-MIP-FG        24. LN-MIP-MTD-CLAHPAID,
      *                   25. LN-MIP-CLPDTH-DATE,   26. LN-MIP-TOT-CLPAID
      *                   27. LN-MIP-AHPDTH-DATE    28. LN-MIP-TOT-AHPAID
      * AMM 051018 ADDED DETAILED, FIELD-BY-FIELD MAINTENANCE LOG IN
      *            /USR/DATA/R1/LG/CHG.YYMMDD
      * MJD 121025 A30 CONVERSION COMPLETE.
      * MJD 121217 ADDED LNMAIN-WINDOW-HANDLE TO SCREEN DISPLAY
      * MJD 130117 CHANGED CALL TMMSG TO PERFORM CALL-TMMSG
      *  CS 160617 ADDED MOVE OF EXT-FILEPATH-BRNO PRIOR TO READ
      *  CS 170426 ACTIVATE 8 DIGIT DATES
      *
      * KEC 2017.0526 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED ORIGINAL-REC AND CHANGED-REC WHICH NOW USING
      *                  LNOR-REC AND LNCH-REC.
      *
      * BAH 181212 GLOBAL LNFILE
      * BAH 20190627 REMOVED LN-REVOLVING, PT0153
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/LNMANA.CBL S35 02/21/24-13:17:04 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

      *=================================================================
      *   LNCH-REC FROM 'REPLACING LEADING' ON LN-REC. [CHANGED-REC]
      *   LNOR-REC FROM 'REPLACING LEADING' ON LN-REC. [ORIGINAL-REC]
      *=================================================================
       COPY "LIBLP/LP01LN.CPY" REPLACING LEADING "LN" BY "LNCH".
       COPY "LIBLP/LP01LN.CPY" REPLACING LEADING "LN" BY "LNOR".

      *=================================================================
       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-NUMBER       PIC Z(7).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(8).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(13).
               05  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(9).99-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(13).
               05  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(9).99-.

       01  ONE               PIC S9.
       01  ELE               PIC 99     VALUE 0.
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).
       01  FORM-NUM1     REDEFINES ACTION.
           03  FORM-KEY1     PIC X(4).
           03  FORM-P1       PIC X.
           03  FORM-SEL1     PIC 9.
       01  FORM-NUM2      REDEFINES ACTION.
           03  FORM-KEY2     PIC X(3).
           03  FORM-P2       PIC X.
           03  FORM-SEL2     PIC 99.
               88  FORM-CUR-PAGE  VALUE 10.
               88  FORM-2-DIGITS  VALUE 10 11.
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-TIME        PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".

       01  LNMAIN-LINK.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(8).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  LNMAIN-WINDOW-HANDLE  PIC X(10).

       01  WS-ORGBR      PIC 9999.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01        PIC Z(8).
           03  F02        PIC Z(8).
           03  F03        PIC Z(6).ZZ-.
           03  F04        PIC Z(5).ZZ-.
           03  F05        PIC Z(5).ZZ-.
           03  F06        PIC Z(5).ZZ-.
           03  F07        PIC XX.
           03  F08        PIC 99/99/99 BLANK ZERO.
           03  F09        PIC 9(12).
           03  F10        PIC Z(6).ZZ.
           03  F11        PIC Z(5)-.
           03  F12        PIC Z(6).ZZ.
           03  F13        PIC X(01).
           03  F14        PIC 99/99/99.
           03  F15        PIC 99/99/99.
           03  F151       PIC 99/99/99.
           03  F16        PIC Z(7).ZZ-.
           03  F17        PIC Z(7).ZZ-.
           03  F18        PIC Z(7).ZZ-.
           03  FLNSTAT    PIC X(30).
           03  F19        PIC 99/99/99     BLANK ZERO.
           03  F20        PIC 99/99/99     BLANK ZERO.
           03  F21        PIC Z(5).
           03  F22        PIC X.
           03  F23        PIC X.
           03  F24        PIC Z(4)9.99-    BLANK ZERO.
           03  F25        PIC 99/99/99     BLANK ZERO.
           03  F26        PIC Z(6)9.99-    BLANK ZERO.
           03  F27        PIC 99/99/99     BLANK ZERO.
           03  F28        PIC Z(6)9.99-    BLANK ZERO.

       01  DISPLAY-LIST.
           03  FILLER     PIC X(20) VALUE
               "010 020 030 040 050 ".
           03  FILLER     PIC X(20) VALUE
               "060 070 080 090 100 ".
           03  FILLER     PIC X(24) VALUE
               "110 120 130 140 150 151 ".
           03  FILLER     PIC X(20) VALUE
               "160 170 180         ".
           03  FILLER     PIC X(47) VALUE
               "LNSTAT 190 200 210 220 230 240 250 260 270 280.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       01  MAX            PIC 99   VALUE 28.
       01  SPEC-TABLE.
           03  FILLER     PIC X(8) VALUE "EN N080N".
           03  FILLER     PIC X(8) VALUE "EN N080N".
           03  FILLER     PIC X(8) VALUE "EN S062N".
           03  FILLER     PIC X(8) VALUE "EN S052N".
           03  FILLER     PIC X(8) VALUE "EN S052N".
           03  FILLER     PIC X(8) VALUE "EN S052N".
           03  FILLER     PIC X(8) VALUE "EN A020N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "EN N120N".
           03  FILLER     PIC X(8) VALUE "ENNN062N".
           03  FILLER     PIC X(8) VALUE "ENNS050N".
           03  FILLER     PIC X(8) VALUE "ENNN062N".
           03  FILLER     PIC X(8) VALUE "E  A010N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ENNS072N".
           03  FILLER     PIC X(8) VALUE "ENNS072N".
           03  FILLER     PIC X(8) VALUE "ENNS072N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "EN N050N".
      * 22. LN-INTERIM-COUPON
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 23. MIP FLAG
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 24. MIP MTD CL/AH PAYMENTS
           03  FILLER     PIC X(8) VALUE "ENNS052N".
      * 25. MIP CL PDTH
           03  FILLER     PIC X(8) VALUE "ERNM060N".
      * 26. MIP TOT CL PAYMENTS
           03  FILLER     PIC X(8) VALUE "ENNS072N".
      * 27. MIP AH PDTH
           03  FILLER     PIC X(8) VALUE "ERNM060N".
      * 28. MIP TOT AH PAYMENTS
           03  FILLER     PIC X(8) VALUE "ENNS072N".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 28 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LNMANA_DEF.CPY".
       COPY "LIBMN/LNMANA_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/LNMANA_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *************************************************
       MAIN-PROGRAM SECTION.
      *************************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNMANA" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO LNMAIN-LINK.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           MOVE " " TO VDU-PGMENU-SELECTION.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME.

           PERFORM REC-LN-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P2
              MOVE 11 TO FORM-SEL2
              MOVE " " TO SAVEKEY.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE LN-ACCTNO TO SAVEKEY
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.
           IF FORM-SEL2 NOT = 0
              IF FORM-P2 = "P"
                 MOVE LN-ACCTNO TO SAVEKEY
                 MOVE FORM-SEL2 TO FORMSEL
                 GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      *******************************************
       REC-LN-MODULE SECTION.
      *******************************************
       REC-LN-MODULE-NO.
           MOVE 0 TO LN-ACCTNO LN-OWNBR FIRST-SCAN.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE "Y" TO PAGE-CHANGE-FLAG
              MOVE SAVEKEY TO LN-ACCTNO
              MOVE " " TO SAVEKEY
              PERFORM DISPLAY-SCAN-KEY
              GO TO READ-LNFILE.

           MOVE "N" TO PAGE-CHANGE-FLAG.
           MOVE 0 TO NEW-FLAG.

       REC-LN-ACCTNO.
           MOVE "EN N060LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-LN-MODULE.
           IF VDUSTATUS = "1<"
             PERFORM ENTER-SCANKEY
             GO TO END-REC-LN-MODULE.
           IF VDUSTATUS NOT = "00"
             GO TO REC-LN-ACCTNO.
           MOVE VDUNUM0 TO LN-ACCTNO.
           IF LN-ACCTNO = 0
              GO TO REC-LN-ACCTNO.
       READ-LNFILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-MODULE-NO.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-MODULE-NO
           ELSE
           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-MODULE-NO
           ELSE
              MOVE LN-REC TO LNOR-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
              IF NEW-REC = "Y"
                 MOVE 1 TO NEW-FLAG
              END-IF
              IF ( LN-POCD-INRESCISSION ) AND ( PAGE-CHANGE-NO )
                 MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
                 PERFORM SEND-MESS
              END-IF.

       END-REC-LN-MODULE.
           EXIT.

      ************************************************
       ENTER-SCANKEY SECTION.
      ************************************************
           MOVE " ENTER LOAN #, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-LN1-FILE.
 
           MOVE LN-PATH-OWNBR TO LN-OWNBR.

           PERFORM START-LN1-FILE.
       ENTER-KEY.
           MOVE "EN N080SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUNUM0 = 0
              IF FIRST-SCAN NOT = 0
                 MOVE LN-REC TO LNOR-REC
                 IF ( LN-POCD-INRESCISSION )
                    MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
                    PERFORM SEND-MESS
                 END-IF
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
           MOVE VDUNUM0       TO LN-ACCTNO.
           MOVE LN-PATH-OWNBR TO LN-OWNBR.

           PERFORM START-LN1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           IF ( LN-POCD-VOID ) OR ( LN-POCD-RESCIND )
              GO TO READ-FILES-NEXT.
      *    UNLOCK LN-FILE.
           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       DISPLAY-SCAN-KEY.
           MOVE "SN N060LNNO." TO VDU.
           MOVE LN-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-LN1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
           EXIT.

      ************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 1 OR FORM-SEL1 > 9
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
      * FORM-NUM2 CHECKS FOR PAGES 10 THRU 99
           IF FORM-KEY2 = " " AND FORM-P2 = "P" AND FORM-SEL2 NUMERIC
              IF FORM-CUR-PAGE OR NOT FORM-2-DIGITS
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
           GO TO CHECK-CHNO.
       CHECK-PAGE.
           MOVE "RWR" TO IO-TYPE.
           GO TO EXIT-ENTRY-MODULE.
       CHECK-CHNO.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF ( ELE = 15 )
              GO TO CREATE-ROUTINE.

           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF NEW-FLAG = 0
              IF CHFG(ELE) = "N"
                 IF NEW-FLAG = 1
                    GO TO CREATE-ROUTINE
                 ELSE
                   IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                      GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO A-01 A-02 A-03 A-04 A-05
                 A-06 A-07 A-08 A-09 A-10
                 A-11 A-12 A-13 A-14 A-15
                 A-16 A-17 A-18 A-19 A-20
                 A-21 A-22 A-23 A-24 A-25
                 A-26 A-27 A-28
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDUNUM0 TO LN-OLDACCTNO.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUNUM0 TO LN-PRLNNO.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUNUM2 TO LN-PRBAL.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUNUM2 TO LN-PRINTDUE.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUNUM2 TO LN-PRFINCHGRB.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUNUM2 TO LN-PRINSRB.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUBUF TO LN-PRPOCD.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDU-DATE-YYYYMMDD TO LN-PRPODATE.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDUNUM0 TO LN-PRCONPROFILE F09.
           INSPECT F09 REPLACING ALL '9' BY '-'.
           MOVE "S  A120090." TO VDU.
           MOVE F09 TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUNUM2 TO LN-PRLNAMT.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUNUM0 TO LN-PRPDUE-DAYS.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUNUM2 TO LN-PRDLDISC-UNEARN.
           GO TO CREATE-ROUTINE.

       A-13.
           IF NOT ( VDUBUF = "Y" OR "N" )
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-RESCISSION.
           GO TO CREATE-ROUTINE.

       A-14.
           MOVE VDU-DATE-YYYYMMDD TO LN-RESCISSION-DATE.
           GO TO CREATE-ROUTINE.

       A-15.
           GO TO CREATE-ROUTINE.

       A-16.
           MOVE VDUNUM2 TO LN-EXTRA-PRIN-LFP.
           GO TO CREATE-ROUTINE.

       A-17.
           MOVE VDUNUM2 TO LN-REMAIN-INT-SINCE-LFP.
           GO TO CREATE-ROUTINE.

       A-18.
           MOVE VDUNUM2 TO LN-REMAIN-PRIN-SINCE-LFP.
           GO TO CREATE-ROUTINE.

       A-19.
           MOVE VDU-DATE-YYYYMMDD TO LN-PRIOR-RN-FOR10-DATE.
           GO TO CREATE-ROUTINE.

       A-20.
           MOVE VDU-DATE-YYYYMMDD TO LN-LAST-RN-FOR10-DATE.
           GO TO CREATE-ROUTINE.

       A-21.
           MOVE VDUNUM0 TO LN-WCBC-CREDITLIM.
           GO TO CREATE-ROUTINE.

       A-22.
           MOVE VDUBUF TO LN-INTERIM-COUPON.
           IF LN-INTERIM-COUPON NOT = "Y" AND
                                 NOT = "N"
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       A-23.
           MOVE VDUBUF TO LN-MIP-FG.
           IF NOT (LN-MIP-FG = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       A-24.
           MOVE VDUNUM2 TO LN-MIP-MTD-CLAHPAID.
           GO TO CREATE-ROUTINE.

       A-25.
           MOVE VDU-DATE-YYYYMMDD TO LN-MIP-CLPDTH-DATE.
           GO TO CREATE-ROUTINE.

       A-26.
           MOVE VDUNUM2 TO LN-MIP-TOT-CLPAID.
           GO TO CREATE-ROUTINE.

       A-27.
           MOVE VDU-DATE-YYYYMMDD TO LN-MIP-AHPDTH-DATE.
           GO TO CREATE-ROUTINE.

       A-28.
           MOVE VDUNUM2 TO LN-MIP-TOT-AHPAID.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************************
       WRITE-MODULE SECTION.
      ************************************************
           PERFORM OPEN-LN1-FILE.

           MOVE LN-REC TO LNCH-REC.

           IF LNOR-REC = LNCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF LN-REC NOT = LNOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE LNCH-REC TO LN-REC.

           PERFORM REWRITE-LN1-FILE.

       END-WRITE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      ***********************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ***********************************************

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR  TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF LNOR-OLDACCTNO NOT = LNCH-OLDACCTNO
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "OLDACCTN"              TO WORK-ERRLOG-DESC
             MOVE LNOR-OLDACCTNO          TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-OLDACCTNO          TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PRLNNO NOT = LNCH-PRLNNO
             MOVE "02 "                   TO WORK-ERRLOG-FIELD
             MOVE "PRLNNO  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-PRLNNO             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-PRLNNO             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PRBAL NOT = LNCH-PRBAL
             MOVE "03 "                   TO WORK-ERRLOG-FIELD
             MOVE "PRBAL   "              TO WORK-ERRLOG-DESC
             MOVE LNOR-PRBAL              TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-PRBAL              TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PRINTDUE NOT = LNCH-PRINTDUE
             MOVE "04 "                   TO WORK-ERRLOG-FIELD
             MOVE "PRINTDUE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PRINTDUE           TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-PRINTDUE           TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PRFINCHGRB NOT = LNCH-PRFINCHGRB
             MOVE "05 "                   TO WORK-ERRLOG-FIELD
             MOVE "PRFINCHG"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PRFINCHGRB         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-PRFINCHGRB         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PRINSRB NOT = LNCH-PRINSRB
             MOVE "06 "                   TO WORK-ERRLOG-FIELD
             MOVE "PRINSRB "              TO WORK-ERRLOG-DESC
             MOVE LNOR-PRINSRB            TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-PRINSRB            TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PRPOCD NOT = LNCH-PRPOCD
             MOVE "07 "                   TO WORK-ERRLOG-FIELD
             MOVE "PRPOCD  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-PRPOCD             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PRPOCD             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PRPODATE NOT = LNCH-PRPODATE
             MOVE "08 "                   TO WORK-ERRLOG-FIELD
             MOVE "PRPODATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PRPODATE           TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PRPODATE           TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PRCONPROFILE NOT = LNCH-PRCONPROFILE
             MOVE "09 "                   TO WORK-ERRLOG-FIELD
             MOVE "PROFILE "              TO WORK-ERRLOG-DESC
             MOVE LNOR-PRCONPROFILE       TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-PRCONPROFILE       TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PRLNAMT NOT = LNCH-PRLNAMT
             MOVE "10 "                   TO WORK-ERRLOG-FIELD
             MOVE "PRLNAMT "              TO WORK-ERRLOG-DESC
             MOVE LNOR-PRLNAMT            TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-PRLNAMT            TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PRPDUE-DAYS NOT = LNCH-PRPDUE-DAYS
             MOVE "11 "                   TO WORK-ERRLOG-FIELD
             MOVE "PRPDUEDA"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PRPDUE-DAYS        TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-PRPDUE-DAYS        TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PRDLDISC-UNEARN NOT = LNCH-PRDLDISC-UNEARN
             MOVE "12 "                   TO WORK-ERRLOG-FIELD
             MOVE "PRDLDISC"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PRDLDISC-UNEARN    TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-PRDLDISC-UNEARN    TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-RESCISSION NOT = LNCH-RESCISSION
             MOVE "13 "                   TO WORK-ERRLOG-FIELD
             MOVE "RESCIS  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-RESCISSION         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-RESCISSION         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-RESCISSION-DATE NOT = LNCH-RESCISSION-DATE
             MOVE "14 "                   TO WORK-ERRLOG-FIELD
             MOVE "RESCISDA"              TO WORK-ERRLOG-DESC
             MOVE LNOR-RESCISSION-DATE    TO DATE-YYYYMMDD          
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-RESCISSION-DATE    TO DATE-YYYYMMDD        
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INTDATE NOT = LNCH-INTDATE
             MOVE "15 "                   TO WORK-ERRLOG-FIELD
             MOVE "INTDATE "              TO WORK-ERRLOG-DESC
             MOVE LNOR-INTDATE            TO DATE-YYYYMMDD          
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INTDATE            TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LOANDATE NOT = LNCH-LOANDATE
             MOVE "151"                   TO WORK-ERRLOG-FIELD
             MOVE "LOANDATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-LOANDATE           TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-LOANDATE           TO WORK-ERRLOG-CHG-ALP
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-EXTRA-PRIN-LFP NOT = LNCH-EXTRA-PRIN-LFP
             MOVE "16 "                   TO WORK-ERRLOG-FIELD
             MOVE "XPRINLFP"              TO WORK-ERRLOG-DESC
             MOVE LNOR-EXTRA-PRIN-LFP     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-EXTRA-PRIN-LFP     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE

           IF LNOR-REMAIN-INT-SINCE-LFP NOT = LNCH-REMAIN-INT-SINCE-LFP
             MOVE "17 "                     TO WORK-ERRLOG-FIELD
             MOVE "INT LFP "                TO WORK-ERRLOG-DESC
             MOVE LNOR-REMAIN-INT-SINCE-LFP TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-REMAIN-INT-SINCE-LFP TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-REMAIN-PRIN-SINCE-LFP NOT =
                                             LNCH-REMAIN-PRIN-SINCE-LFP
             MOVE "18 "                      TO WORK-ERRLOG-FIELD
             MOVE "PRIN LFP"                 TO WORK-ERRLOG-DESC
             MOVE LNOR-REMAIN-PRIN-SINCE-LFP TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-REMAIN-PRIN-SINCE-LFP TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PRIOR-RN-FOR10-DATE NOT = LNCH-PRIOR-RN-FOR10-DATE
             MOVE "19 "                    TO WORK-ERRLOG-FIELD
             MOVE "PRIOR RN"               TO WORK-ERRLOG-DESC
             MOVE LNOR-PRIOR-RN-FOR10-DATE TO DATE-YYYYMMDD        
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY              TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PRIOR-RN-FOR10-DATE TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY              TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LAST-RN-FOR10-DATE NOT = LNCH-LAST-RN-FOR10-DATE
             MOVE "20 "                   TO WORK-ERRLOG-FIELD
             MOVE "LAST RN "              TO WORK-ERRLOG-DESC
             MOVE LNOR-LAST-RN-FOR10-DATE TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-LAST-RN-FOR10-DATE TO DATE-YYYYMMDD         
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-WCBC-CREDITLIM NOT = LNCH-WCBC-CREDITLIM
             MOVE "21 "                   TO WORK-ERRLOG-FIELD
             MOVE "CRED LIM"              TO WORK-ERRLOG-DESC
             MOVE LNOR-WCBC-CREDITLIM     TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-WCBC-CREDITLIM     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INTERIM-COUPON NOT = LNCH-INTERIM-COUPON
             MOVE "22 "                   TO WORK-ERRLOG-FIELD
             MOVE "COUPON  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-INTERIM-COUPON     TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INTERIM-COUPON     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MIP-FG NOT = LNCH-MIP-FG
             MOVE "23 "                   TO WORK-ERRLOG-FIELD
             MOVE "MIP FG  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-MIP-FG             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-MIP-FG             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MIP-MTD-CLAHPAID NOT = LNCH-MIP-MTD-CLAHPAID
             MOVE "24 "                   TO WORK-ERRLOG-FIELD
             MOVE "CLAHPAID"              TO WORK-ERRLOG-DESC
             MOVE LNOR-MIP-MTD-CLAHPAID   TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-MIP-MTD-CLAHPAID   TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MIP-CLPDTH-DATE NOT = LNCH-MIP-CLPDTH-DATE
             MOVE "25 "                   TO WORK-ERRLOG-FIELD
             MOVE "CLPDTH-DATE  "         TO WORK-ERRLOG-DESC
             MOVE LNOR-MIP-CLPDTH-DATE    TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-MIP-CLPDTH-DATE    TO DATE-YYYYMMDD     
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MIP-TOT-CLPAID NOT = LNCH-MIP-TOT-CLPAID
             MOVE "26 "                   TO WORK-ERRLOG-FIELD
             MOVE "CLPAID  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-MIP-TOT-CLPAID     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-MIP-TOT-CLPAID     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MIP-AHPDTH-DATE NOT = LNCH-MIP-AHPDTH-DATE
             MOVE "27 "                   TO WORK-ERRLOG-FIELD
             MOVE "AHPDTH-DATE  "         TO WORK-ERRLOG-DESC
             MOVE LNOR-MIP-AHPDTH-DATE    TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-MIP-AHPDTH-DATE    TO DATE-YYYYMMDD     
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MIP-TOT-AHPAID NOT = LNCH-MIP-TOT-AHPAID
             MOVE "28 "                   TO WORK-ERRLOG-FIELD
             MOVE "AHPAID  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-MIP-TOT-AHPAID     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-MIP-TOT-AHPAID     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

      *************************************************
       LOG-FIELD-CHANGE SECTION.
      *************************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      *************************************************
       DISPLAY-FIELDS SECTION.
      *************************************************
           MOVE LN-OLDACCTNO             TO F01.
           MOVE LN-PRLNNO                TO F02.
           MOVE LN-PRBAL                 TO F03.
           MOVE LN-PRINTDUE              TO F04.
           MOVE LN-PRFINCHGRB            TO F05.
           MOVE LN-PRINSRB               TO F06.
           MOVE LN-PRPOCD                TO F07.
           MOVE LN-PRPODATE              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO F08.
           MOVE LN-PRCONPROFILE          TO F09.
           INSPECT F09 REPLACING ALL '9' BY '-'.
           MOVE LN-PRLNAMT               TO F10.
           MOVE LN-PRPDUE-DAYS           TO F11.
           MOVE LN-PRDLDISC-UNEARN       TO F12.
           MOVE LN-RESCISSION            TO F13.
           MOVE LN-RESCISSION-DATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO F14.
           MOVE LN-INTDATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO F15.
           MOVE LN-LOANDATE              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO F151.
           MOVE LN-EXTRA-PRIN-LFP        TO F16.
           MOVE LN-REMAIN-INT-SINCE-LFP  TO F17.
           MOVE LN-REMAIN-PRIN-SINCE-LFP TO F18.
           MOVE LN-PRIOR-RN-FOR10-DATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO F19.
           MOVE LN-LAST-RN-FOR10-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO F20.
           MOVE LN-WCBC-CREDITLIM        TO F21.
           MOVE LN-INTERIM-COUPON        TO F22.
           MOVE LN-MIP-FG                TO F23.
           MOVE LN-MIP-MTD-CLAHPAID      TO F24.
           MOVE LN-MIP-CLPDTH-DATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO F25.
           MOVE LN-MIP-TOT-CLPAID        TO F26.
           MOVE LN-MIP-AHPDTH-DATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO F27.
           MOVE LN-MIP-TOT-AHPAID        TO F28.
           MOVE LN-ORGBR TO WS-ORGBR.

           STRING LN-ORGST, "-", WS-ORGBR, "-", LN-SPRCLASS, "(",
                  LN-CLASS, ")", "-", LN-SUBCLASS, "-", LN-LAWCODE,
                  "-", LN-SMALLOAN, LN-SALEFIN, LN-SERVACCT, LN-LOANTYPE
                  DELIMITED BY "  " INTO FLNSTAT.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           MOVE EXT-CONAME-CONAME TO LNMANA-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNMANA-SYSDATE.
           DISPLAY LNMANA-SCREEN UPON LNMAIN-WINDOW-HANDLE.
           MOVE LNMANA-FORM-FIELDS TO WR-BUF.
           MOVE LNMANA-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE LNMANA-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNMANA-HELP--DIR TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LNMANA_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *************************************************
       MISC-PARAGRAPHS SECTION.
      *************************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.


      ******************************************
       IO-ROUTINES SECTION.
      ******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************
       END-ROUTINE SECTION.
      ******************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNMANA-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE LNMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
