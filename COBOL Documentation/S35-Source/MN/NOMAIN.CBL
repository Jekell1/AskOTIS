       IDENTIFICATION DIVISION.
       PROGRAM-ID. NOMAIN.
      ******************************************************************
      *
      *         NOTICE FILE MAINTENECE (NOFILE)
      *
      * REV.
      * 083095 KEC ADDED NO-TDUECONTR, NO-LDUECONTR, NO-NXDUECONTR
      * 083195 KEC ADDED LIBLP/LPNOCL; CLEARS NO-FILE.
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * BAH 050597 ADDED NO-CURRENT-AVAIL(WDT) AND NO-LATE-CHG (BHFC)
      *            AND RENAMED NO-LATECHG TO NO-LCHG-INTDUE
      * GLF 102497 REMOVED RESET OF FIELDS
      * BAH 000726 ADDED NO-PAST-DUE-DAYS  PR# 1170, SUPPORT
      * BAH 160318 EXPANDED RECORD PD0003
      *  CS 170503 ACTIVATE 8 DIGIT DATES
      * BAH 181004 GLOBAL BWFILE
      * BAH 190107 GLOBAL NOFILE
      * BAH 2024.0201 REMOVED NO-FMNO, NO-PRINTED AND NO-WAGE-ASSIGN-DELINQ
      *               CHANGED 20. TO NO-AVCR-RESTRICT-FG FROM NO-PRINTED
      *               CHANGED 32. TO NO-INDU-PENALTY FROM NO-WAGE-ASSIGN-DELINQ
      *               ADDED 34,35 AND 36 AS CPOT30, RDPOT30 AND CSUB
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/NOMAIN.CBL S35 02/21/24-12:35:41 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  ERRCD           PIC X VALUE " ".
       01  CHAN-DATA       VALUE SPACES.
           03  CHAN        PIC X(4).
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).

       01  HOLD-NO1-KEY.
           03 HOLD-NO-BRNO      PIC 9(4). 
           03 HOLD-NO-ACCTNO    PIC 9(8). 

       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-SCAN      PIC 9       VALUE 0.

      *=================================================================
      *   NOCH-REC - CHANGED-REC
      *   NOOR-REC - ORIGINAL-REC
      *=================================================================
       COPY "LIBLP/LP01NO.CPY" REPLACING LEADING "NO" BY "NOCH".
       COPY "LIBLP/LP01NO.CPY" REPLACING LEADING "NO" BY "NOOR".

       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  WORK-ERRLOG-NUMBER       PIC Z(7).
           03  FILLER                   PIC X(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(2).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(10).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(10).
               05  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(9)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(6).99-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(10).
               05  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(9)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(6).99-.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01          PIC 99/99/99.
           03  F02          PIC ZZZZ9.99-.
           03  F03          PIC ZZZZ9.99-.
           03  F04          PIC ZZZZ9.99-.
           03  F05          PIC ZZZZZZ9.99-.
           03  F06          PIC ZZZZZZ9.99-.
           03  F07          PIC 99/99/99.
           03  F08          PIC ZZZZ9.99-.
           03  F09          PIC ZZZZ9.99-.
           03  F10          PIC 99/99/99.
           03  F11          PIC ZZZZZZ9.99-.
           03  F12          PIC 99/99/99.
           03  F13          PIC ZZZZ9.99-.
           03  F14          PIC ZZZZZZ9.99-.
           03  F15          PIC 9999.
           03  F16          PIC 9999.
           03  F17          PIC 99/99/99.
           03  F18          PIC 99/99/99.
           03  F19          PIC ZZZ9.99.
           03  F20          PIC X.
           03  F21          PIC X(17).
           03  F22          PIC Z(4).
           03  F23          PIC ZZZZ9.99-.
           03  F24          PIC 99/99/99.
           03  F25          PIC ZZZZ9.99-.
           03  F26          PIC ZZZZ9.99-.
           03  F27          PIC ZZZZ9.99-.
           03  F28          PIC ZZZZ9.99-.
           03  F29          PIC ZZZZ9.99-.
           03  F30          PIC ZZZZ9.99-.
           03  F31          PIC ZZZZ9.99-.
           03  F32          PIC ZZZZ9.99-.
           03  F33          PIC Z(4).
           03  F34          PIC X.
           03  F35          PIC X.
           03  F36          PIC ZZZZ9.

        01  DISPLAY-LIST.
            03  FILLER       PIC X(52) VALUE
            "010 020 030 040 050 060 070 080 090 100 110 120 130 ".
            03  FILLER       PIC X(52) VALUE
            "140 150 160 170 180 190 200 210 220 230 240 250 260 ".
            03  FILLER       PIC X(52) VALUE
            "270 280 290 300 310 320 330 340 350 360.".
      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX              PIC 99   VALUE 36.
       01  SPEC-TABLE.
      * 1. DATE
            03  FILLER       PIC X(8) VALUE "E NM080N".
      * 2. LCHG
            03  FILLER       PIC X(8) VALUE "ENNS052N".
      * 3. AMOUNT
            03  FILLER       PIC X(8) VALUE "ENNS052N".
      * 4. PRINCIPAL
            03  FILLER       PIC X(8) VALUE "ENNS052N".
      * 5. TOTAL BAL
            03  FILLER       PIC X(8) VALUE "ENNS072N".
      * 6. PAY OFF
            03  FILLER       PIC X(8) VALUE "ENNS072N".
      * 7. DATE
            03  FILLER       PIC X(8) VALUE "ERNM080N".
      * 8. LATE CHG
            03  FILLER       PIC X(8) VALUE "ENNS052N".
      * 9. AMOUNT
            03  FILLER       PIC X(8) VALUE "ENNS052N".
      *10. LAST PAID
            03  FILLER       PIC X(8) VALUE "ERNM080N".
      *11. LAST AMT
            03  FILLER       PIC X(8) VALUE "ENNS072N".
      *12. DATE
            03  FILLER       PIC X(8) VALUE "ERNM080N".
      *13. LATE CHG
            03  FILLER       PIC X(8) VALUE "ENNS052N".
      *14. AMOUNT
            03  FILLER       PIC X(8) VALUE "ENNS072N".
      *15. CONTRUAL
            03  FILLER       PIC X(8) VALUE "ENNN040N".
      *16. RECENCY
            03  FILLER       PIC X(8) VALUE "ENNN040N".
      *17. PAID THRU
            03  FILLER       PIC X(8) VALUE "ERNM080N".
      *18. LCPDTH
            03  FILLER       PIC X(8) VALUE "ERNM080N".
      *19. LATE CHG
            03  FILLER       PIC X(8) VALUE "ENNE042N".
      *20. AVCR RESTRICT FLAG
            03  FILLER       PIC X(8) VALUE "E  A010N".
      *21. LST LPKEY
            03  FILLER       PIC X(8) VALUE "E  A170N".
      *22. BRNO
            03  FILLER       PIC X(8) VALUE "ENNN040N".
      *23. AVAILABLE CREDIT
            03  FILLER       PIC X(8) VALUE "ENNN052N".
      *24. TRUE DUE DATE
            03  FILLER       PIC X(8) VALUE "ERNM080N".
      *25. DEFERMENT AMOUNT
            03  FILLER       PIC X(8) VALUE "ENNN052N".
      *26. SCHEDULED PAYMENT AMOUNT
            03  FILLER       PIC X(8) VALUE "ENNN052N".
      *27. TO DATE DUE CONTR
            03  FILLER       PIC X(8) VALUE "ENNN052N".
      *28. LAST DUE CONTR
            03  FILLER       PIC X(8) VALUE "ENNN052N".
      *29. NEXT DUE CONTR
            03  FILLER       PIC X(8) VALUE "ENNN052N".
      *30. CURRENT AVAILABLE
            03  FILLER       PIC X(8) VALUE "ENNN052N".
      *31. LATE CHARGE ONLY
            03  FILLER       PIC X(8) VALUE "ENNN052N".
      *32. INDU PENALTY
            03  FILLER       PIC X(8) VALUE "ENNN052N".
      *33. PAST DUE DAYS
            03  FILLER       PIC X(8) VALUE "ENNN040N".
      *34. CPOT30
            03  FILLER       PIC X(8) VALUE "E  A010N".
      *35. CPOT30
            03  FILLER       PIC X(8) VALUE "E  A010N".
      *36. CSUB
            03  FILLER       PIC X(8) VALUE "ENNN050N".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
            03  SPEC-REC    OCCURS 36 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01  NOMAIN-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/NOMAIN_DEF.CPY".
       COPY "LIBMN/NOMAIN_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBMN/NOMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ****************************
      *      MAIN PROGRAM MODULE
      ****************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE "NOMAIN" TO VDU-FORM-NAME.
           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS NOMAIN-WINDOW-HANDLE.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
       MAIN-MODULE.
      D    STOP MESS.
           PERFORM FORM-RESET.

           PERFORM REC-MODULE1.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      *****************************************
       REC-MODULE1 SECTION.
      *****************************************
       REC-MODULE.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.
           MOVE "EN N060ACCTNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-MODULE.
           MOVE VDUNUM0 TO LN-ACCTNO HOLD-NO-ACCTNO.
       READ-LN1.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              PERFORM ERROR-ACCOUNT-CD
              GO TO REC-MODULE.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO REC-MODULE.

           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO REC-MODULE.

           IF ( LN-POCD-INRESCISSION )
              MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
              PERFORM SEND-MESS.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG = 9
              PERFORM ERR-SSNO
              GO TO REC-MODULE.

           MOVE "SN N090BWSSNO." TO VDU.
           MOVE BW-SSNO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A000BWLNAME." TO VDU.
           MOVE BW-LNAME TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000BWFNAME." TO VDU.
           MOVE BW-FNAME TO VDUBUF.
           PERFORM SCREENIO.

           MOVE HOLD-NO-ACCTNO TO NO-ACCTNO.
       READ-NO1FILE.
           PERFORM OPEN-NO1-FILE.
           MOVE NO-PATH-OWNBR TO HOLD-NO-BRNO.
           PERFORM READ-NO1-FILE.
           PERFORM CLOSE-NO1-FILE.
           IF IO-FG = 0
              MOVE NO-REC TO NOOR-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.
       END-REC-MODULE.
           EXIT.

      *****************************************
       ENTER-SCANKEY SECTION.
      *****************************************
           MOVE " ENTER ACCT NO OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-NO1-FILE.
           MOVE NO-PATH-OWNBR TO NO-BRNO.
           MOVE 0             TO NO-ACCTNO.

           PERFORM START-NO1-FILE.
       ENTER-KEY.
           MOVE "EN N060SCANKEY." TO VDU.
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
                 MOVE NO1-KEY TO HOLD-NO1-KEY
                 MOVE NO-REC TO NOOR-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE NO-PATH-OWNBR TO HOLD-NO-BRNO.
           MOVE VDUNUM0       TO HOLD-NO-ACCTNO.
           MOVE HOLD-NO1-KEY  TO NO1-KEY.

           PERFORM START-NO1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-NO1-FILE-NEXT.
           IF IO-FG = 9 OR (NO-BRNO NOT = NO-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

      *    UNLOCK NO-FILE.

           MOVE NO1-KEY TO HOLD-NO1-KEY.

           MOVE "SN N060ACCTNO." TO VDU.
           MOVE HOLD-NO-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-NO1-FILE.
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
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "18"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1:"
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
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07
           F-08 F-09 F-10 F-11 F-12 F-13 F-14 F-15
           F-16 F-17 F-18 F-19 F-20 F-21 F-22 F-23
           F-24 F-25 F-26 F-27 F-28 F-29 F-30 F-31
           F-32 F-33 F-34 F-35 F-36
                DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
      * ***********************************
      * SET RECORD ELEMENTS
      * ***********************************
       F-01.
           MOVE VDU-DATE-YYYYMMDD TO NO-TDUE-DATE.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM2 TO NO-TDUELC.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUNUM2 TO NO-TDUEAMT.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUNUM2 TO NO-TDUEPRIN.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM2 TO NO-TDUETBAL.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM2 TO NO-TDUEPOFF.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDU-DATE-YYYYMMDD TO NO-LDUE-DATE.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM2 TO NO-LDUELC.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM2 TO NO-LDUEAMT.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDU-DATE-YYYYMMDD TO NO-DATE-PAID-LAST.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUNUM2 TO NO-AMT-PAID-LAST.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDU-DATE-YYYYMMDD TO NO-NXDUE-DATE.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUNUM2 TO NO-NXDUELC.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUNUM2 TO NO-NXDUEAMT.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUNUM0 TO NO-TRUCONTR.
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE VDUNUM0 TO NO-RECENCY.
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDU-DATE-YYYYMMDD TO NO-PDTH-DATE.
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDU-DATE-YYYYMMDD TO NO-LCPDTH-DATE.
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUNUM2 TO NO-LCHG-INTDUE.
           GO TO CREATE-ROUTINE.
       F-20.
           IF VDUBUF NOT = "R"
              IF VDUBUF NOT = " "
                 MOVE "ENTER 'R' OR SPACE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           MOVE VDUBUF TO NO-AVCR-RESTRICT-FG.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUBUF TO NO-LSTLPKEY.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUNUM0 TO NO-BRNO.
           PERFORM OPEN-BR-FILE.
           MOVE NO-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDUNUM2 TO NO-AVAILCREDIT.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDU-DATE-YYYYMMDD TO NO-TRUE-DUE-DATE.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE VDUNUM2 TO NO-DEFERMENT.
           GO TO CREATE-ROUTINE.
       F-26.
           MOVE VDUNUM2 TO NO-SCHED-PAYMENT.
           GO TO CREATE-ROUTINE.

       F-27.
           MOVE VDUNUM2 TO NO-TDUECONTR.
           GO TO CREATE-ROUTINE.

       F-28.
           MOVE VDUNUM2 TO NO-LDUECONTR.
           GO TO CREATE-ROUTINE.

       F-29.
           MOVE VDUNUM2 TO NO-NXDUECONTR.
           GO TO CREATE-ROUTINE.
       F-30.
           MOVE VDUNUM2 TO NO-CURRENT-AVAIL.
           GO TO CREATE-ROUTINE.
       F-31.
           MOVE VDUNUM2 TO NO-LATE-CHG.
           GO TO CREATE-ROUTINE.
       F-32.
           IF VDUNUM2 < 0
              GO TO ENTER-ELE.
           MOVE VDUNUM2 TO NO-INDU-PENALTY.
           GO TO CREATE-ROUTINE.
       F-33.
           MOVE VDUNUM0 TO NO-PAST-DUE-DAYS.
           GO TO CREATE-ROUTINE.
       F-34.
           MOVE VDUBUF TO NO-CPOT30.
           GO TO CREATE-ROUTINE.
       F-35.
           MOVE VDUBUF TO NO-RDPOT30.
           GO TO CREATE-ROUTINE.
       F-36.
           MOVE VDUNUM0 TO NO-CSUB.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ************************************************
       WRITE-MODULE SECTION.
      ************************************************
           PERFORM OPEN-NO1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.
           MOVE NO-REC TO NOCH-REC.
           IF NOOR-REC = NOCH-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-NO1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF NO-REC NOT = NOOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE NOCH-REC TO NO-REC.

           PERFORM REWRITE-NO1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-NO1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-NO1-FILE.

      **********************************************
       LOG-MAINTENANCE-CHGS SECTION.
      **********************************************

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE NO-BRNO   TO WORK-ERRLOG-BR.
           MOVE NO-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF NOOR-TDUE-DATE NOT = NOCH-TDUE-DATE
             MOVE "01"               TO WORK-ERRLOG-FIELD
             MOVE "TDUE DATE "       TO WORK-ERRLOG-DESC
             MOVE NOOR-TDUE-DATE     TO WORK-ERRLOG-ORG-N0
             MOVE NOCH-TDUE-DATE     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-TDUELC NOT = NOCH-TDUELC
             MOVE "02"               TO WORK-ERRLOG-FIELD
             MOVE "TDUE LCHG "       TO WORK-ERRLOG-DESC
             MOVE NOOR-TDUELC     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-TDUELC     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-TDUEAMT NOT = NOCH-TDUEAMT
             MOVE "03"               TO WORK-ERRLOG-FIELD
             MOVE "TDUE AMT  "       TO WORK-ERRLOG-DESC
             MOVE NOOR-TDUEAMT     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-TDUEAMT     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-TDUEPRIN NOT = NOCH-TDUEPRIN
             MOVE "04"               TO WORK-ERRLOG-FIELD
             MOVE "TDUE PRIN "       TO WORK-ERRLOG-DESC
             MOVE NOOR-TDUEPRIN     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-TDUEPRIN     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-TDUETBAL NOT = NOCH-TDUETBAL
             MOVE "05"               TO WORK-ERRLOG-FIELD
             MOVE "TDUE TOBAL"       TO WORK-ERRLOG-DESC
             MOVE NOOR-TDUETBAL     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-TDUETBAL     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-TDUEPOFF NOT = NOCH-TDUEPOFF
             MOVE "06"              TO WORK-ERRLOG-FIELD
             MOVE "PAY OFF  "       TO WORK-ERRLOG-DESC
             MOVE NOOR-TDUEPOFF     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-TDUEPOFF     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-LDUE-DATE NOT = NOCH-LDUE-DATE
             MOVE "07"              TO WORK-ERRLOG-FIELD
             MOVE "LDUE DATE"       TO WORK-ERRLOG-DESC
             MOVE NOOR-LDUE-DATE     TO WORK-ERRLOG-ORG-N0
             MOVE NOCH-LDUE-DATE     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-LDUELC NOT = NOCH-LDUELC
             MOVE "08"              TO WORK-ERRLOG-FIELD
             MOVE "LDUE LCHG "       TO WORK-ERRLOG-DESC
             MOVE NOOR-LDUELC     TO WORK-ERRLOG-ORG-N0
             MOVE NOCH-LDUELC     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-LDUEAMT NOT = NOCH-LDUEAMT
             MOVE "09"               TO WORK-ERRLOG-FIELD
             MOVE "LDUE AMT  "       TO WORK-ERRLOG-DESC
             MOVE NOOR-LDUEAMT     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-LDUEAMT     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-DATE-PAID-LAST NOT = NOCH-DATE-PAID-LAST
             MOVE "10"               TO WORK-ERRLOG-FIELD
             MOVE "LASTPAIDDT"       TO WORK-ERRLOG-DESC
             MOVE NOOR-DATE-PAID-LAST     TO WORK-ERRLOG-ORG-N0
             MOVE NOCH-DATE-PAID-LAST     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-AMT-PAID-LAST NOT = NOCH-AMT-PAID-LAST
             MOVE "11"               TO WORK-ERRLOG-FIELD
             MOVE "LSTPAIDAMT"       TO WORK-ERRLOG-DESC
             MOVE NOOR-AMT-PAID-LAST     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-AMT-PAID-LAST     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-NXDUE-DATE NOT = NOCH-NXDUE-DATE
             MOVE "12"               TO WORK-ERRLOG-FIELD
             MOVE "NXDUE DATE"       TO WORK-ERRLOG-DESC
             MOVE NOOR-NXDUE-DATE     TO WORK-ERRLOG-ORG-N0
             MOVE NOCH-NXDUE-DATE     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-NXDUELC NOT = NOCH-NXDUELC
             MOVE "13"               TO WORK-ERRLOG-FIELD
             MOVE "NXDUE LCHG"       TO WORK-ERRLOG-DESC
             MOVE NOOR-NXDUELC     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-NXDUELC     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-NXDUEAMT NOT = NOCH-NXDUEAMT
             MOVE "14"               TO WORK-ERRLOG-FIELD
             MOVE "NXDUE AMT "       TO WORK-ERRLOG-DESC
             MOVE NOOR-NXDUEAMT     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-NXDUEAMT     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-TRUCONTR NOT = NOCH-TRUCONTR
             MOVE "15"               TO WORK-ERRLOG-FIELD
             MOVE "CONTRCTUAL"       TO WORK-ERRLOG-DESC
             MOVE NOOR-TRUCONTR     TO WORK-ERRLOG-ORG-N0
             MOVE NOCH-TRUCONTR     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-RECENCY NOT = NOCH-RECENCY
             MOVE "16"               TO WORK-ERRLOG-FIELD
             MOVE "RECENCY   "       TO WORK-ERRLOG-DESC
             MOVE NOOR-RECENCY     TO WORK-ERRLOG-ORG-N0
             MOVE NOCH-RECENCY     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-PDTH-DATE NOT = NOCH-PDTH-DATE
             MOVE "17"               TO WORK-ERRLOG-FIELD
             MOVE "PDTH THRU "       TO WORK-ERRLOG-DESC
             MOVE NOOR-PDTH-DATE     TO WORK-ERRLOG-ORG-N0
             MOVE NOCH-PDTH-DATE     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-LCPDTH-DATE NOT = NOCH-LCPDTH-DATE
             MOVE "18"               TO WORK-ERRLOG-FIELD
             MOVE "LCPDTH    "       TO WORK-ERRLOG-DESC
             MOVE NOOR-LCPDTH-DATE     TO WORK-ERRLOG-ORG-N0
             MOVE NOCH-LCPDTH-DATE     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-LCHG-INTDUE NOT = NOCH-LCHG-INTDUE
             MOVE "19"               TO WORK-ERRLOG-FIELD
             MOVE "LCHG/INTDU"       TO WORK-ERRLOG-DESC
             MOVE NOOR-LCHG-INTDUE     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-LCHG-INTDUE     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-AVCR-RESTRICT-FG NOT = NOCH-AVCR-RESTRICT-FG
             MOVE "20"               TO WORK-ERRLOG-FIELD
             MOVE "AVCR RESTRICT"    TO WORK-ERRLOG-DESC
             MOVE NOOR-AVCR-RESTRICT-FG     TO WORK-ERRLOG-ORG-ALP
             MOVE NOCH-AVCR-RESTRICT-FG     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-LSTLPKEY NOT = NOCH-LSTLPKEY
             MOVE "21"               TO WORK-ERRLOG-FIELD
             MOVE "LAST LPKEY"       TO WORK-ERRLOG-DESC
             MOVE NOOR-LSTLPKEY     TO WORK-ERRLOG-ORG-ALP
             MOVE NOCH-LSTLPKEY     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-BRNO NOT = NOCH-BRNO
             MOVE "22"               TO WORK-ERRLOG-FIELD
             MOVE "BRANCH NO "       TO WORK-ERRLOG-DESC
             MOVE NOOR-BRNO     TO WORK-ERRLOG-ORG-N0
             MOVE NOCH-BRNO     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-AVAILCREDIT NOT = NOCH-AVAILCREDIT
             MOVE "23"               TO WORK-ERRLOG-FIELD
             MOVE "AVAIL CRED"       TO WORK-ERRLOG-DESC
             MOVE NOOR-AVAILCREDIT     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-AVAILCREDIT     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-TRUE-DUE-DATE NOT = NOCH-TRUE-DUE-DATE
             MOVE "24"               TO WORK-ERRLOG-FIELD
             MOVE "TRU DUEDTE"       TO WORK-ERRLOG-DESC
             MOVE NOOR-TRUE-DUE-DATE     TO WORK-ERRLOG-ORG-N0
             MOVE NOCH-TRUE-DUE-DATE     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-DEFERMENT NOT = NOCH-DEFERMENT
             MOVE "25"               TO WORK-ERRLOG-FIELD
             MOVE "DEF AMOUNT"       TO WORK-ERRLOG-DESC
             MOVE NOOR-DEFERMENT     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-DEFERMENT     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-SCHED-PAYMENT NOT = NOCH-SCHED-PAYMENT
             MOVE "26"               TO WORK-ERRLOG-FIELD
             MOVE "SCHED PAY "       TO WORK-ERRLOG-DESC
             MOVE NOOR-SCHED-PAYMENT     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-SCHED-PAYMENT     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-TDUECONTR NOT = NOCH-TDUECONTR
             MOVE "27"               TO WORK-ERRLOG-FIELD
             MOVE "TDUE CONTR"       TO WORK-ERRLOG-DESC
             MOVE NOOR-TDUECONTR     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-TDUECONTR     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-LDUECONTR NOT = NOCH-LDUECONTR
             MOVE "28"               TO WORK-ERRLOG-FIELD
             MOVE "LDUE CONTR"       TO WORK-ERRLOG-DESC
             MOVE NOOR-LDUECONTR     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-LDUECONTR     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-NXDUECONTR NOT = NOCH-NXDUECONTR
             MOVE "29"               TO WORK-ERRLOG-FIELD
             MOVE "NDUE CONTR"       TO WORK-ERRLOG-DESC
             MOVE NOOR-NXDUECONTR     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-NXDUECONTR     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-CURRENT-AVAIL NOT = NOCH-CURRENT-AVAIL
             MOVE "30"               TO WORK-ERRLOG-FIELD
             MOVE "CURR AVAIL"       TO WORK-ERRLOG-DESC
             MOVE NOOR-CURRENT-AVAIL     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-CURRENT-AVAIL     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-LATE-CHG NOT = NOCH-LATE-CHG
             MOVE "31"               TO WORK-ERRLOG-FIELD
             MOVE "LCHG ONLY "       TO WORK-ERRLOG-DESC
             MOVE NOOR-LATE-CHG     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-LATE-CHG     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-INDU-PENALTY NOT = NOCH-INDU-PENALTY
             MOVE "32"                  TO WORK-ERRLOG-FIELD
             MOVE "INDU PENALTY "       TO WORK-ERRLOG-DESC
             MOVE NOOR-INDU-PENALTY     TO WORK-ERRLOG-ORG-N2
             MOVE NOCH-INDU-PENALTY     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-PAST-DUE-DAYS NOT = NOCH-PAST-DUE-DAYS
             MOVE "33"               TO WORK-ERRLOG-FIELD
             MOVE "PAST DUE D"       TO WORK-ERRLOG-DESC
             MOVE NOOR-PAST-DUE-DAYS     TO WORK-ERRLOG-ORG-N0
             MOVE NOCH-PAST-DUE-DAYS     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF NOOR-CPOT30 NOT = NOCH-CPOT30
             MOVE "34"               TO WORK-ERRLOG-FIELD
             MOVE "CPOT30"           TO WORK-ERRLOG-DESC
             MOVE NOOR-CPOT30     TO WORK-ERRLOG-ORG-ALP
             MOVE NOCH-CPOT30     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF NOOR-RDPOT30 NOT = NOCH-RDPOT30
             MOVE "35"               TO WORK-ERRLOG-FIELD
             MOVE "RDPOT30"           TO WORK-ERRLOG-DESC
             MOVE NOOR-RDPOT30     TO WORK-ERRLOG-ORG-ALP
             MOVE NOCH-RDPOT30     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF NOOR-CSUB NOT = NOCH-CSUB
             MOVE "36"               TO WORK-ERRLOG-FIELD
             MOVE "CSUB"       TO WORK-ERRLOG-DESC
             MOVE NOOR-CSUB     TO WORK-ERRLOG-ORG-N0
             MOVE NOCH-CSUB     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

      ************************************
       LOG-FIELD-CHANGE SECTION.
      ************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      ************************************************
       DELETE-MODULE SECTION.
      ************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.
           PERFORM OPEN-NO1-FILE.
           PERFORM READ-NO1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF NO-REC NOT = NOOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-NO1-FILE.
      *WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY
           STRING NO1-KEY, " ", "DELETED" DELIMITED BY SIZE
                INTO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-DELETE-MODULE.
           PERFORM CLOSE-NO1-FILE.

      ************************************************
       NEW-RECORD-SETUP SECTION.
      ************************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-NO-FILE.

           MOVE HOLD-NO-ACCTNO TO NO-ACCTNO.
           MOVE 1 TO NO-BRNO.
           MOVE 0 TO ELE.

      ************************************************
       DISPLAY-FIELDS SECTION.
      ************************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE NO-TDUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO F01.

           MOVE NO-TDUELC TO F02.
           MOVE NO-TDUEAMT TO F03.
           MOVE NO-TDUEPRIN TO F04.
           MOVE NO-TDUETBAL TO F05.
           MOVE NO-TDUEPOFF TO F06.

           MOVE NO-LDUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO F07.

           MOVE NO-LDUELC TO F08.
           MOVE NO-LDUEAMT TO F09.
           MOVE NO-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F10.

           MOVE NO-AMT-PAID-LAST TO F11.

           MOVE NO-NXDUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO F12.
           
           MOVE NO-NXDUELC TO F13.
           MOVE NO-NXDUEAMT TO F14.
           MOVE NO-TRUCONTR TO F15.
           MOVE NO-RECENCY TO F16.

           MOVE NO-PDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO F17.

           MOVE NO-LCPDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO F18.

           MOVE NO-LCHG-INTDUE TO F19.
           MOVE NO-AVCR-RESTRICT-FG TO F20.
           MOVE NO-LSTLPKEY    TO F21.
           MOVE NO-BRNO        TO F22.
           MOVE NO-AVAILCREDIT TO F23.

           MOVE NO-TRUE-DUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO F24.

           MOVE NO-DEFERMENT TO F25.
           MOVE NO-SCHED-PAYMENT TO F26.
           MOVE NO-TDUECONTR  TO F27.
           MOVE NO-LDUECONTR  TO F28.
           MOVE NO-NXDUECONTR TO F29.
           MOVE NO-CURRENT-AVAIL      TO F30.
           MOVE NO-LATE-CHG           TO F31.
           MOVE NO-INDU-PENALTY       TO F32.
           MOVE NO-PAST-DUE-DAYS      TO F33.
           MOVE NO-CPOT30             TO F34.
           MOVE NO-RDPOT30            TO F35.
           MOVE NO-CSUB               TO F36.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      *************************************
       FORM-RESET SECTION.
      *************************************
           MOVE EXT-CONAME-CONAME TO NOMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO NOMAIN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY NOMAIN-SCREEN UPON NOMAIN-WINDOW-HANDLE.
           MOVE NOMAIN-FORM-FIELDS TO WR-BUF.
           MOVE NOMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************
       SETUP-LINK-INFO SECTION.
      *************************************
           MOVE NOMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE NOMAIN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/NOMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPNOCL.CPY".
      
      ************************************************
       MISC-PARAGRAPHS SECTION.
      ************************************************
       COPY "LIBGB/ACCESS.CPY".
       ERROR-ACCOUNT-CD.
           MOVE " INVALID ACCOUNT NO " TO MESS.
           PERFORM SEND-MESS.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ERR-SSNO.
           MOVE " ERROR ON SOC NUMBER " TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.
       FMNO-DESCRIPTION.
           MOVE "S  A000FDDESC." TO VDU.
           MOVE FD-DESC TO VDUBUF.
           PERFORM SCREENIO.

      ************************************************
       IO-ROUTINE SECTION.
      ************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *******************************************
       END-ROUTINE SECTION.
      *******************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY NOMAIN-SCREEN.
           DESTROY NOMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
