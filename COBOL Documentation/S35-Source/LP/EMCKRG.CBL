       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EMCKRG.
       DATE-WRITTEN. 01-15-88
      ******************************************************************
      *                      (LP)
      *    DESCRIPTION:  END OF MONTH CHECK REGISTER
      *
      *        PRINTS ALL TRC TYPES EXCEPT;
      *               "CA", "BK", "RR", "RC" "RP" AND "RD"
      *               "ID", "IW"
      * REV:
      *  BAH 081795 IF LOAN OR SALES CHECK, ALWAYS PRINT ACCTNO IN REF
      *  KEC 091495 ADDED LOCAL-SPOOLING LOGIC.
      *  KEC 091595 CHANGE MILITARY TIME TO AM/PM ON HEADER.
      *  BAH 092895 RECORDS-FOUND INITIALIZED TO "N" INSTEAD OF " "
      *  KEC 101295 COMMENTED OUT DEFAULT BRANCH SETTING IN INITIZATION.
      *  KEC 101695 MADE CHANGE TO HEADER TO PRINT VALID BRANCH NAME.
      *  JFF 111095 CHANGED FORM TO INCLUDE LOCAL-SPOOLING QUESTION.
      *  KEC 113095 FIXED LOCAL-SPOOL FOR IF A BRANCH HAS NO DATA.
      *  KEC 113095 FIXED LOCAL-SPOOL TO NOT PRINT THE RANGE-LINES.
      *  JFF 121395 CHANGED "IF LOCAL-PRINT-FG = N" TO
      *             "IF LOCAL-PRINT-FG NOT = Y" IN INITIALIZATION.
      *  KEC 121395 ADDED LOGIC TO PRINT ERROR MESSAGE IF ERROR ON
      *             READING THE GI-FILE.
      *  BLV 060597 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      *  GLF 090297 REMOVED ALL USE OF GLOBAL
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  GLF 022798 DELETED "END OF MONTH" FROM TITLE
      *  BLV 031598 ADDED GRAND TOTAL OF CHECKS (WORLD)
      *  MJD 990115 ADDED NOTES TRFILE AND CHECK TO PROGRAM
      *  MJD 990401 ADDED ACCESS CALL FOR NI FILE.
      *  GLB 010702 TEST TO SKIP NEW TC-TYPE "RD" RECEIPT PROCEEDS
      *  BLV 020605 FIX TO GET CORRECT GL # FROM GI FOR NEW TYPE "AC",
      *             DEALER ACH DIRECT DEPOSIT, REGACC PR# 1797
      *  BAH 021008 FIXED RECORDS-FOUND TO NOT GIVE "NO RECORDS FOUND"
      *             ON A F7 EXIT
      *  BAH 040301 ADDED MISC CHECKS SKIP, INCLUDE OR ONLY FOR WORLD
      *             PR# 343, WILL NOT AFFECT BATCHES !!
      *  BAH 040903 REPLACE BR-NAME IN HEADING WITH BR-REGION-NUMBER
      *             FOR GP-I-AM-REGACC, REGACC PR# 77
      *  BLF 071017 IF TC-PAYEE-NO NOT 0 OR SPACES, PRINT PAYEE # &
      *             SHORTENED PAYEE NAME, HOME PR# 3078
      *   CS 120316 ADD "ID", "IW" TO TC-TYPES TO SKIP  PR#790
      *  BAH 170526 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181023 REMOVED NOTES
      *  BAH 20190930 REMOVED ACCESS-CALL TRCFILE
      *  BAH 2021.0318 SKIP TC-TYPE "DB" DEBIT CARD, #1490
      *  RMZ 2023.1023 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *  RMZ 2024.0116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      *  BAH 2024.0624 WAS NOT DISPLAY MISC CHECKS IN DISPLAY PARAMETERS
      *                                                #1661
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
      *    SELECT WK2-FILE ASSIGN WK2-PATH
      *           ORGANIZATION INDEXED
      *           ACCESS DYNAMIC
      *           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
      *           RECORD KEY WK2-KEY
      *           FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BR         PIC 9(4).
               05  WK-GLNO       PIC 9(11).
           03  WK-DRGLAMT        PIC S9(9)V99    COMP-3.
           03  WK-CRGLAMT        PIC S9(9)V99    COMP-3.

      *FD  WK2-FILE
      *    LABEL RECORDS ARE STANDARD.
      *01  WK2-REC.
      *    03  WK2-KEY.
      *        05  WK2-BRANCH          PIC 9(4).
      *        05  WK2-CKNO            PIC 9(6).
      *        05  WK2-DATE            PIC 9(8).
      *        05  WK2-CLASS           PIC 9(3).
      *        05  WK2-SEQ             PIC 9(6).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/EMCKRG.CBL S35 06/24/24-13:35:51 >".
      ****************************
      *   F I L E   P A T H S    *
      ****************************
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       01  PR-IFN               PIC X(2)    VALUE "PR".
      *01  WK2-IFN              PIC X(3)    VALUE "WK2".
       01  WK-IFN               PIC X(2)    VALUE "WK".

      *01  WK2-PATH             PIC X(35).

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS-MISC-CHECKS      PIC X        VALUE SPACE. 
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  TRC1-CONN-OPEN-SW    PIC X        VALUE "N".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
      **********************
      *   MISC WORKERS     *
      **********************
       01  SUBX                 PIC 9(6) COMP-3.
       01  FIRST-TIME           PIC X               VALUE "Y".
       01  RECORDS-FOUND        PIC X               VALUE " ".
           88  NO-RECORDS-FOUND                     VALUE "N".
       01  GL-SUM-FG            PIC X               VALUE SPACES.
       01  MSEL-LIST            PIC X(5)            VALUE "MSEL.".
       01  MESS-LIST            PIC X(5)            VALUE "MESS.".
       01  WORK-GLNO            PIC 9(11).
       01  WORK-GL              REDEFINES WORK-GLNO.
           03  WORK-GLBR        PIC 9(4).
           03  WORK-GLNUM       PIC 9(7).
       01  HOLD-BRNO            PIC 9(4)            VALUE 0.
       01  GLAMT                PIC S9(9)V99 COMP-3 VALUE 0.
       01  GLNO                 PIC 9(11)    COMP-3 VALUE 0.
       01  CK-TOTAL             PIC S9(9)V99 COMP-3 VALUE 0.
       01  CK-TOTDR             PIC S9(9)V99 COMP-3 VALUE 0.
       01  CK-TOTCR             PIC S9(9)V99 COMP-3 VALUE 0.
       01  GRAND-TOTAL          PIC S9(9)V99 COMP-3 VALUE 0.


      *-----------------------------------------------------------------
      * GI-FILE ERROR WORKERS
      *-----------------------------------------------------------------
       01  WS-FIELDS.
           03  WS-ERROR-GI-FILE              PIC X(1)     VALUE "N".
           03  WS-ERROR-GI-MESSAGE.
               05  FILLER                    PIC X(10)    VALUE SPACES.
               05  FILLER                    PIC X(20)    VALUE ALL "*".
               05  FILLER                    PIC X(45)    VALUE
               "ERROR OCCURED TRYING TO ACCESS GI-RECORD --- ".
               05  FILLER                    PIC X(28)    VALUE
               "THIS BRANCH HAS BEEN SKIPPED".
               05  FILLER                    PIC X(20)    VALUE ALL "*".
               05  FILLER                    PIC X(9)     VALUE SPACES.

      *-----------------------------------------------------------------
       COPY "LIBLP/EMSFRG_CMD.CPY".
      *-----------------------------------------------------------------
       01  LEGEND               PIC X(70).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  END-DATE.
               05  END-MONTH   PIC 99.
               05  END-YEAR    PIC 99.
           03  EDATE REDEFINES END-DATE PIC 9999.
           03  DAY-OF-MONTH    PIC 99.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  LOCAL-PRINT-FG  PIC X(1).
      * PLEASE LEAVE THIS "NOTES" FIELD HERE. IF YOU REMOVE IT THEIR BATCH
      * PARAMETERS WONT WORK AND MISC-CHECKS WILL GET A "S" IN IT.
           03  NOTES           PIC X.
           03  MISC-CHECKS     PIC X.

       01  VERSION-CONTROL     PIC X VALUE "B".
       01  BEGINNING-DATE       PIC 9(8).
       01  ENDING-DATE          PIC 9(8).

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-LOCAL-PRINT-FG   PIC X     VALUE SPACES.
       COPY "LIBGB/PASSWDW.CPY".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(39)    VALUE
           "      - - - CHECK REGISTER - - -       ".
           03  FILLER           PIC X(39)    VALUE  " ".
           03  FILLER           PIC X(8)  VALUE "* = VOID".

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC X(2)     VALUE " ".
           03  HD-BRNO     PIC 9(4).
           03  FILLER      PIC X        VALUE " ".
           03  HD-BRNAME   PIC X(20).
           03  FILLER      PIC X(68)    VALUE " ".
           03  FILLER      PIC X(29)    VALUE
               "(B-BATCH, M-MANUAL, S-SYSTEM)".

       01  HEAD4.
           03  FILLER      PIC X(17) VALUE "   CHECK --DATE--".
           03  FILLER      PIC X(7)  VALUE " ".
           03  FILLER      PIC X(14) VALUE "AMOUNT   PAYEE".
           03  FILLER      PIC X(24) VALUE " ".
           03  FILLER      PIC X(7)  VALUE " TYPE  ".
           03  FILLER      PIC X(2)  VALUE " ".
           03  FILLER      PIC X(14) VALUE "REF #   REASON".
           03  FILLER      PIC X(24) VALUE " ".
           03  FILLER      PIC X(9)  VALUE "GL-NUMBER".
           03  FILLER      PIC X(7)  VALUE " ".
           03  FILLER      PIC X(7)  VALUE "AMOUNT".

       01  HEAD5.
           03  FILLER      PIC X(35) VALUE
           "------------GENERAL LEDGER SUMMARY:".

       01  HEAD6.
           03  FILLER      PIC X(53) VALUE
           " GL ACCOUNT   NAME".
           03  FILLER      PIC X(35) VALUE
           "DEBIT         CREDIT            NET".

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

      *-----------------------------------------------------------------
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-PAID      PIC X.
           03  D-VOID      PIC X.
           03  D-CKNO      PIC ZZZZZ9.
           03  FILLER      PIC X.
           03  D-CKDATE    PIC 99/99/99.
           03  FILLER      PIC X.
           03  D-CKAMT     PIC -,---,---.99.
           03  FILLER      PIC X.
           03  D-PAYEE     PIC X(30).
           03  D-RESERVE-STUFF REDEFINES D-PAYEE.
               05  D-DLPAYEE PIC X(22).
               05  FILLER    PIC X.
               05  D-DLNO    PIC Z(4).99 BLANK ZERO.
           03  D-PAYEE-NO-NAME REDEFINES D-PAYEE.
               05  D-PAYEE-NO PIC Z(6) BLANK ZERO.
               05  FILLER    PIC X.
               05  D-PAYEE-NM PIC X(23).
           03  FILLER      PIC XX.
           03  D-TYPE      PIC X(2).
           03  FILLER      PIC X(2).
           03  D-DLRVNO    PIC 9(1).
           03  FILLER      PIC X(2).
           03  D-REFNO     PIC ZZZZZ9.
           03  FILLER      PIC X.
           03  D-REASON    PIC X(30).
           03  FILLER      PIC X(1).
           03  D-GLBR      PIC 9(4).
           03  D-DASH      PIC X.
           03  D-GLNUM     PIC 9(7).
           03  FILLER      PIC X.
           03  D-GLAMT     PIC ----,---.99.

      *-----------------------------------------------------------------
       01  T-LINE REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(16).
           03  T-CKTOTAL   PIC ZZZ,ZZZ,ZZZ.99-.

      *-----------------------------------------------------------------
       01  S-LINE REDEFINES DETAIL-LINE.
           03  S-GLBR      PIC 9(4).
           03  S-DASH      PIC X.
           03  S-GLNUM     PIC 9(7).
           03  FILLER      PIC X(3).
           03  S-GLNAME    PIC X(30).
           03  S-DRGLAMT   PIC ZZZ,ZZZ,ZZZ.99-.
           03  S-CRGLAMT   PIC ZZZ,ZZZ,ZZZ.99-.
           03  S-NET       PIC ZZZ,ZZZ,ZZZ.99-.

      *-----------------------------------------------------------------
       01  ERROR-GI-LINE REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(20).
           03  D-ERR-PROMPT1           PIC X(5).
           03  D-ERR-DAY               PIC 9(2).
           03  FILLER                  PIC X(5).
           03  D-ERR-PROMPT2           PIC X(8).
           03  D-ERR-BRNO              PIC 9(4).
           03  FILLER                  PIC X(5).
           03  D-ERR-PROMPT3           PIC X(6).
           03  D-ERR-CODE              PIC X(1).
           03  FILLER                  PIC X(5).
           03  D-ERR-PROMPT4           PIC X(11).
           03  D-ERR-CKNO              PIC 9(6).
           03  FILLER                  PIC X(5).
           03  D-ERR-PROMPT5           PIC X(7).
           03  D-ERR-CLASS             PIC 9(2).
           03  FILLER                  PIC X(5).
           03  D-ERR-PROMPT6           PIC X(10).
           03  D-ERR-SEQ               PIC X(4).
           03  FILLER                  PIC X(21).

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(5)9.
               04  FILLER REDEFINES R-NUM.
                   05  FILLER   PIC X(5).
                   05  R-FG     PIC X.

      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EMCKRG_DEF.CPY".
       COPY "LIBLP/EMCKRG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/EMCKRG_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
      *        PR-FILE WK2-FILE WK-FILE.
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-TRC1-CONNECTION.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
      *    CLOSE WK2-FILE WK-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************


           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EMCKRG" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           IF ( LOCAL-PRINT-FG = "Y" )
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF ( STAT NOT = "00" )
                 GO TO NEXT-GROUP-LOC.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "CHECK REGISTER IN PROGRESS....", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO TRC-PATH-OVERRIDE.

           PERFORM OPEN-TRC1-FILE.

      *    PERFORM SORT-ROUTINE.

           IF ( EXT-EOBUF-ERRCD = "X" )
              GO TO NEXT-GROUP-LOC.

      *    PERFORM OPEN-WK2-FILE.
           MOVE "N"                 TO WS-ERROR-GI-FILE.

           MOVE BR-NO               TO TC-BRNO
                                       QTRC1-WBEG-BRNO
                                       QTRC1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE.
           MOVE ENDING-DATE         TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WEND-KEY-DATE.
           MOVE 0                   TO TC-CKNO TC-CLASS TC-SEQ
                                       QTRC1-WBEG-CKNO
                                       QTRC1-WBEG-CLASS
                                       QTRC1-WBEG-SEQ.
           MOVE ALL "9"             TO QTRC1-WEND-CKNO
                                       QTRC1-WEND-CLASS
                                       QTRC1-WEND-SEQ.


           PERFORM START-TRC1-FILE. *> SSP - TRC,GI


       NEXT-CK.
           PERFORM READ-TRC1-FILE-NEXT. *> EMBEDDED - TRC,GI
           IF (IO-FG NOT = 0) OR (TRC-PATH-OWNBR NOT = TC-BRNO)
              GO TO END-OF-GROUP.

           IF TC-KEY-DATE > ENDING-DATE
              GO TO END-OF-GROUP.

           IF TC-TYPE = "CA" OR "BK" OR "RR" OR "RC" OR "RP" OR "RD"
                             OR "ID" OR "IW" OR "DB"
              GO TO NEXT-CK.

           IF MISC-CHECKS = "S"
              IF TC-TYPE = "MS"
                 GO TO NEXT-CK.

           IF MISC-CHECKS = "O"
              IF TC-TYPE NOT = "MS"
                 GO TO NEXT-CK. 

      *NEXT-CK.
      *    PERFORM READ-WK2-FILE-NEXT.
      *    IF IO-FG = 9
      *       GO TO END-OF-GROUP.

      *    MOVE WK2-BRANCH TO TC-BRNO.
      *    MOVE WK2-DATE   TO TC-KEY-DATE.
      *    MOVE WK2-CKNO   TO TC-CKNO.
      *    MOVE WK2-CLASS  TO TC-CLASS.
      *    MOVE WK2-SEQ    TO TC-SEQ.
      *    PERFORM READ-TRC1-FILE.
      *    IF IO-FG NOT = 0
      *       MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO IO-ERROR.

           IF FIRST-TIME = "Y"
              MOVE TC-BRNO TO HOLD-BRNO
              PERFORM SET-BRANCH-HEADINGS.

           IF TC-BRNO NOT = HOLD-BRNO
              PERFORM BRANCH-TOTALS
              MOVE TC-BRNO TO HOLD-BRNO
              PERFORM SET-BRANCH-HEADINGS
              PERFORM HEAD-PAGE.

           PERFORM DETAIL-LINE-PROCESS.

           IF ( WS-ERROR-GI-FILE = "Y" )
              PERFORM WRITE-ERROR-LINE
              PERFORM CLOSE-TRC1-FILE
      *       PERFORM CLOSE-WK2-FILE
      *       PERFORM OPEN-WK2-FILE-OUTPUT
              GO TO NEXT-GROUP-LOC
           ELSE
              GO TO NEXT-CK.

      *-----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-TRC1-FILE.
      *    PERFORM CLOSE-WK2-FILE.
      *    PERFORM OPEN-WK2-FILE-OUTPUT.

           IF ( LOCAL-PRINT-FG = "Y" )
              IF ( FIRST-TIME NOT = "Y" )
                 PERFORM BRANCH-TOTALS
                 MOVE "Y" TO FIRST-TIME
                 MOVE 0 TO PAGE-NUMBER
              ELSE
                 MOVE 0 TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF LOCAL-PRINT-FG = "N"
              PERFORM BRANCH-TOTALS
              PERFORM GRAND-TOTALS
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
      *   I N I T I A L I Z A T I O N   *
      ***********************************
       INITIALIZATION SECTION.
           MOVE EMCKRG-QUEUE-POS TO Q-POS.
           MOVE EMCKRG-COPIES-POS TO C-POS.
           MOVE EMCKRG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.

      * CREATE SORT KEY FILE FOR TRFILE OUTPUT - IN STORED PROCEDURE NOW
      *    MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
      *    PERFORM MKTEMP-CALL.
      *    IF STAT NOT = "00"
      *       MOVE "CANNOT MAKE SORT KEY FILE" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENDIT.
      *    MOVE MKTEMP-BUF TO WK2-PATH.
      *    PERFORM OPEN-WK2-FILE-OUTPUT.

      * CREATE WORK FILE FOR GL SUMMARY:
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE GL WORKFILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE LOCAL-PRINT-FG TO SV-LOCAL-PRINT-FG
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG LOCAL-PRINT-FG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "CHECK REGISTER IN PROGRESS...." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      * SET DATE RANGE FOR TESTING TR FILES
           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 1      TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 31      TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO ENDING-DATE.

           IF ( LOCAL-PRINT-FG NOT = "Y" )
              PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-GI1-FILE.
           PERFORM OPEN-GL1-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      **********************************************
       EO-EXEC-AUDIT SECTION.
      **********************************************
           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.
           MOVE S-MM TO END-MONTH.
           MOVE S-YY TO END-YEAR.
           IF EXT-EOBUF-DATE1 NOT = EXT-EOBUF-DATE2
              MOVE 99 TO DAY-OF-MONTH
           ELSE
              MOVE S-DD TO DAY-OF-MONTH.

      **********************************************
       ENTER-PARAMETERS SECTION.
      **********************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO MISC-ISO.

       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       DTE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO DA-01.
           MOVE LEGEND-DTE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           MOVE VDUNUM0 TO EDATE.
           IF END-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DTE.

       DA-01.
           MOVE LEGEND-DA-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE SPACES TO VDUSTATUS.
           MOVE "ENNN020DA1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16"
              MOVE "S  A020DA1." TO VDU
              MOVE "**" TO VDUBUF
              PERFORM SCREENIO
              MOVE 99 TO DAY-OF-MONTH
              GO TO MISC-ISO.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO DTE
              ELSE
                 GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           IF VDUNUM0 = 0 GO TO DA-01.
           MOVE VDUNUM0 TO DAY-OF-MONTH.
           IF DAY-OF-MONTH > 31  OR
              (DAY-OF-MONTH > 30 AND
              (END-MONTH = 2 OR 4 OR 6 OR 9 OR 11)) OR
              (DAY-OF-MONTH > 29 AND END-MONTH = 2)
              MOVE "DAY IS PAST LAST DAY OF MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DA-01.

      *-----------------------------------------------------------------
       MISC-ISO.
           MOVE LEGEND-NOTES-ISO TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010MISC." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO MISC-ISO.

           MOVE VDUBUF TO MISC-CHECKS.

           IF NOT (MISC-CHECKS = "I" OR "S" OR "O")
              GO TO MISC-ISO.

      *-----------------------------------------------------------------
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO END-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO END-PARM.

      *-----------------------------------------------------------------
       ASK-LOCAL.

           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.
           MOVE LEGEND-LOCAL-SPOOL TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

      *-----------------------------------------------------------------
       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-PARM.
           EXIT.

      **********************************************
       DISPLAY-PARAMETERS SECTION.
      **********************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           MOVE "SNNN020DA1." TO VDU.
           MOVE DAY-OF-MONTH TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010MISC." TO VDU.
           MOVE MISC-CHECKS TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ***************************************
      *SORT-ROUTINE SECTION.
      ***************************************

      *    MOVE BR-NO          TO TC-BRNO
      *                           QTRC1-WBEG-BRNO
      *                           QTRC1-WEND-BRNO.
      *    MOVE BEGINNING-DATE TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE.
      *    MOVE ENDING-DATE TO SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WEND-KEY-DATE.
      *    MOVE 0                   TO TC-CKNO TC-CLASS TC-SEQ
      *                                QTRC1-WBEG-CKNO
      *                                QTRC1-WBEG-CLASS
      *                                QTRC1-WBEG-SEQ.
      *    MOVE ALL "9"             TO QTRC1-WEND-CKNO
      *                                QTRC1-WEND-CLASS
      *                                QTRC1-WEND-SEQ.

      *    PERFORM START-TRC1-FILE.
      *SORT-TR-NEXT.
      *    PERFORM READ-TRC1-FILE-NEXT.
      *    IF (IO-FG NOT = 0) OR (TRC-PATH-OWNBR NOT = TC-BRNO)
      *       GO TO EXIT-SORT-ROUTINE.

      *    IF TC-KEY-DATE > ENDING-DATE
      *       GO TO EXIT-SORT-ROUTINE.

      *    IF TC-TYPE = "CA" OR "BK" OR "RR" OR "RC" OR "RP" OR "RD"
      *                      OR "ID" OR "IW" OR "DB"
      *       GO TO SORT-TR-NEXT.

      *    IF MISC-CHECKS = "S"
      *       IF TC-TYPE = "MS"
      *          GO TO SORT-TR-NEXT.

      *    IF MISC-CHECKS = "O"
      *       IF TC-TYPE NOT = "MS"
      *          GO TO SORT-TR-NEXT.

      *    MOVE TC-KEY-DATE TO WK2-DATE.
      *    MOVE TC-BRNO     TO WK2-BRANCH.
      *    MOVE TC-CKNO     TO WK2-CKNO.
      *    MOVE TC-CLASS    TO WK2-CLASS.
      *    MOVE TC-SEQ      TO WK2-SEQ.

      *    PERFORM WRITE-WK2-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO IO-ERROR.

      *    GO TO SORT-TR-NEXT.

      *EXIT-SORT-ROUTINE.
      *    PERFORM CLOSE-WK2-FILE.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************
           MOVE TC-PAID TO D-PAID.

           IF TC-VOID = "Y"
              MOVE "*" TO D-VOID.

           MOVE TC-CKNO TO D-CKNO.
           MOVE TC-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-CKDATE.
           MOVE TC-AMT TO D-CKAMT.
           ADD TC-AMT TO CK-TOTAL GRAND-TOTAL.
           MOVE TC-PAYEE TO D-PAYEE.
           IF TC-PAYEE-NO NOT = 0
              IF TC-PAYEE-NO NOT = 202020
                 MOVE SPACES      TO D-PAYEE
                 MOVE TC-PAYEE-NO TO D-PAYEE-NO
                 MOVE TC-PAYEE    TO D-PAYEE-NM.
           MOVE TC-TYPE TO D-TYPE.

           IF TC-TYPE = "DR" OR "DC" OR "DP"
              MOVE TC-PAYEE TO D-DLPAYEE
              MOVE TC-DLNO TO D-DLNO
              IF TC-TYPE = "DR" OR "DC"
                 MOVE TC-DLRVNO TO D-DLRVNO.

           IF TC-REF NOT = 0
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * 990001 IS SET IN 2ONPI0 FOR LONPM2 TO BE ABLE TO FIND THE CX-REC
      * IT JUST CONFUSES THE CUSTOMER, DONT PRINT IT......
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF TC-REF NOT = 990001
                 MOVE TC-REF TO D-REFNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * IF LOAN OR SALES CHECK, ALWAYS PRINT ACCTNO IN REF
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TC-TYPE = "LN" OR "DL"
              MOVE TC-ACCTNO TO D-REFNO.

           MOVE TC-REASON TO D-REASON.
           PERFORM UPDATE-CHECK-TYPE.

           IF ( WS-ERROR-GI-FILE = "Y" )
              GO TO END-DETAIL-LN-PROCESS.

           MOVE 1 TO SUBX.

           IF TC-GLAMT(SUBX) NOT = 0
              MOVE TC-GLNO(SUBX) TO GLNO
              MOVE TC-GLAMT(SUBX) TO GLAMT
              PERFORM UPDATE-GL-SUMMARY
              MOVE TC-GLNO(SUBX) TO WORK-GLNO
              MOVE WORK-GLBR TO D-GLBR
              MOVE "-" TO D-DASH
              MOVE WORK-GLNUM TO D-GLNUM
              MOVE TC-GLAMT(SUBX) TO D-GLAMT
              MOVE 2 TO SUBX.

           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------------
       PRINT-GLDIST.
           IF SUBX > 5
              GO TO END-DETAIL-LN-PROCESS.

           IF TC-GLAMT(SUBX) = 0
              ADD 1 TO SUBX
              GO TO PRINT-GLDIST.

           MOVE TC-GLNO(SUBX) TO GLNO.
           MOVE TC-GLAMT(SUBX) TO GLAMT.
           PERFORM UPDATE-GL-SUMMARY.
           MOVE TC-GLNO(SUBX) TO WORK-GLNO.
           MOVE WORK-GLBR TO D-GLBR.
           MOVE "-" TO D-DASH.
           MOVE WORK-GLNUM TO D-GLNUM.
           MOVE TC-GLAMT(SUBX) TO D-GLAMT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           ADD 1 TO SUBX.
           GO TO PRINT-GLDIST.

       END-DETAIL-LN-PROCESS.
           EXIT.

      ******************************************************************
      *    UPDATE CHECK TYPES
      *       ALL TRC TYPES EXCEPT;
      *        "CA", "BK", "RR", "RC", "RP"  AND "RD"
      *
      *    NOTE: THESE UPDATE BY CHECK TYPE
      *          MUST MATCH LPEOD5.C
      ******************************************************************
       UPDATE-CHECK-TYPE SECTION.
           IF ( TC-CLASS = 0 )
              MOVE BR-GLBANK   TO GI-BANK WK-GLNO
              MOVE BR-GLONHAND TO GI-CASH
           ELSE
      *       MOVE TC-BRNO  TO GI-BRANCH
      *       MOVE TC-CLASS TO GI-CLASS
      *       PERFORM READ-GI1-FILE
      *       IF ( IO-FG NOT = 0 )
	      IF GI-BRANCH = 0
                 MOVE "Y" TO WS-ERROR-GI-FILE
                 GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "DL"
              MOVE GI-BANK TO GLNO
              SUBTRACT TC-AMT FROM GLAMT
              PERFORM UPDATE-GL-SUMMARY
              MOVE GI-DLPAYABLE TO GLNO
              MOVE TC-AMT TO GLAMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "DR"
              MOVE GI-BANK TO GLNO
              SUBTRACT TC-AMT FROM GLAMT
              PERFORM UPDATE-GL-SUMMARY
              MOVE GI-DLRVPRIN(TC-DLRVNO) TO GLNO
              MOVE TC-AMT TO GLAMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "DC"
              MOVE GI-BANK TO GLNO
              SUBTRACT TC-AMT FROM GLAMT
              PERFORM UPDATE-GL-SUMMARY
              MOVE GI-DLRVCHGS(TC-DLRVNO) TO GLNO
              MOVE TC-AMT TO GLAMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "DP"
              MOVE GI-BANK TO GLNO
              SUBTRACT TC-AMT FROM GLAMT
              PERFORM UPDATE-GL-SUMMARY
              MOVE GI-DLPARV TO GLNO
              MOVE TC-AMT TO GLAMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "LN"
              IF TC-PGM = "D0"
                 MOVE GI-BANK TO GLNO
                 SUBTRACT TC-AMT FROM GLAMT
                 PERFORM UPDATE-GL-SUMMARY
                 MOVE GI-DIRECT-PAY TO GLNO
                 MOVE TC-AMT TO GLAMT
                 PERFORM UPDATE-GL-SUMMARY
                 GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "LN"
              IF TC-PGM = "F2"
                 MOVE GI-BANK TO GLNO
                 SUBTRACT TC-AMT FROM GLAMT
                 PERFORM UPDATE-GL-SUMMARY
                 MOVE GI-DIRECT-PAY TO GLNO
                 MOVE TC-AMT TO GLAMT
                 PERFORM UPDATE-GL-SUMMARY
                 GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "LN"
              IF TC-PGM = "I0"
                 MOVE GI-BANK TO GLNO
                 SUBTRACT TC-AMT FROM GLAMT
                 PERFORM UPDATE-GL-SUMMARY
                 GO TO UPDATE-CHECK-TYPE-END.

           IF TC-TYPE = "AC"
              MOVE GI-ACH-BANK TO GLNO
              SUBTRACT TC-AMT FROM GLAMT
              PERFORM UPDATE-GL-SUMMARY
              MOVE GI-DLPAYABLE TO GLNO
              MOVE TC-AMT TO GLAMT
              PERFORM UPDATE-GL-SUMMARY
              GO TO UPDATE-CHECK-TYPE-END.

           MOVE GI-BANK TO GLNO.
           SUBTRACT TC-AMT FROM GLAMT.
           PERFORM UPDATE-GL-SUMMARY.

      *-----------------------------------------------------------------
       UPDATE-CHECK-TYPE-END.
           EXIT.

      ***********************************
       UPDATE-GL-SUMMARY SECTION.
      ***********************************
       UPDATE-GL-SUMMARY-REC.
           MOVE TC-BRNO TO WK-BR.
           MOVE GLNO TO WK-GLNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              GO TO UPDATE-GL-SUMMARY-CREATE.

           IF GLAMT > 0
              ADD GLAMT TO WK-DRGLAMT
           ELSE
              ADD GLAMT TO WK-CRGLAMT.

           PERFORM REWRITE-WK-FILE.
           GO TO UPDATE-GL-SUMMARY-EXIT.

       UPDATE-GL-SUMMARY-CREATE.
           MOVE TC-BRNO TO WK-BR.
           MOVE GLNO TO WK-GLNO.
           MOVE 0 TO WK-DRGLAMT WK-CRGLAMT.
           PERFORM WRITE-WK-FILE.
           GO TO UPDATE-GL-SUMMARY-REC.

       UPDATE-GL-SUMMARY-EXIT.
           MOVE 0 TO GLAMT.

      ****************************
       BRANCH-TOTALS SECTION.
      ****************************
           MOVE "BRANCH TOTAL:" TO DETAIL-LINE.
           MOVE CK-TOTAL TO T-CKTOTAL.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "Y" TO GL-SUM-FG.
           MOVE 99 TO LN-FEED.

           PERFORM PRINT-BR-GL-SUMMARY.

           MOVE 0 TO CK-TOTAL.
           MOVE 99 TO LN-FEED.
           MOVE SPACES TO GL-SUM-FG.
       END-BRANCH-TOTALS.
           EXIT.

      ***********************************
       PRINT-BR-GL-SUMMARY SECTION.
      ***********************************
           MOVE "Y" TO GL-SUM-FG.
           MOVE 0 TO CK-TOTDR CK-TOTCR.
           MOVE HOLD-BRNO TO WK-BR.
           MOVE 0 TO WK-GLNO.
           PERFORM START-WK-FILE.
           IF IO-FG NOT = 0
              GO TO PRINT-BR-GL-SUM-END.
       PRINT-BR-GL-SUM-NEXT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO PRINT-BR-GL-SUM-LINE.
           IF WK-BR NOT = HOLD-BRNO
              GO TO PRINT-BR-GL-SUM-LINE.
           MOVE WK-GLNO TO WORK-GLNO.
           MOVE WORK-GLBR TO S-GLBR.
           MOVE "-" TO S-DASH.
           MOVE WORK-GLNUM TO S-GLNUM.

           MOVE WK-GLNO TO GL-BR-ACCOUNT.
           PERFORM READ-GL1-FILE. *> EMBEDDED - GLFILE
           IF IO-FG NOT = 0
              MOVE "****NOT ON FILE***" TO S-GLNAME
           ELSE
              MOVE GL-DESCRIPTION TO S-GLNAME.

           IF WK-DRGLAMT NOT = 0
              MOVE WK-DRGLAMT TO S-DRGLAMT
              ADD WK-DRGLAMT TO CK-TOTDR.

           IF WK-CRGLAMT NOT = 0
              MOVE WK-CRGLAMT TO S-CRGLAMT
              ADD WK-CRGLAMT TO CK-TOTCR.

           COMPUTE S-NET = WK-DRGLAMT + WK-CRGLAMT.

           PERFORM WRITE-DETAIL-LINE.
           GO TO PRINT-BR-GL-SUM-NEXT.
       PRINT-BR-GL-SUM-LINE.
           MOVE SPACES TO DETAIL-LINE.
           MOVE "TOTAL:" TO S-GLNAME.
           MOVE CK-TOTDR TO S-DRGLAMT.
           MOVE CK-TOTCR TO S-CRGLAMT.
           COMPUTE S-NET = CK-TOTDR + CK-TOTCR.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       PRINT-BR-GL-SUM-END.
           MOVE 99 TO LN-FEED.
           MOVE SPACES TO GL-SUM-FG.

      ****************************
       GRAND-TOTALS SECTION.
      ****************************
           MOVE "GRAND TOTAL:" TO DETAIL-LINE.
           MOVE GRAND-TOTAL TO T-CKTOTAL.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       END-GRAND-TOTALS.
           EXIT.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
       WRITE-DET-LINE.
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       SET-BRANCH-HEADINGS SECTION.
      ******************************************************************
           MOVE HOLD-BRNO TO HD-BRNO.
           MOVE BR-NAME   TO HD-BRNAME.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO H-TIME.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *--->MOVE HOLD-BRNO TO H-BRNO.
           MOVE BR-GLBANK TO WK-GLNO.
      *--->MOVE BR-NAME TO H-BRNAME.
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GL-SUM-FG NOT = "Y"
               MOVE HEAD4 TO PR-REC
               MOVE 2 TO LN-FEED
               PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            IF GL-SUM-FG = "Y"
               MOVE 2 TO LN-FEED
               MOVE HEAD5 TO PR-REC
               PERFORM WRITE-PR-REC
               MOVE HEAD6 TO PR-REC
               MOVE 2 TO LN-FEED
               PERFORM WRITE-PR-REC
               MOVE 9 TO LN-COUNT
           ELSE
              MOVE 5 TO LN-COUNT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 2 TO LN-FEED.
      ******************************************************************
      *
      *    W R I T E - E R R O R - L I N E
      *
      ******************************************************************
       WRITE-ERROR-LINE SECTION.
           MOVE WS-ERROR-GI-MESSAGE TO DETAIL-LINE
           MOVE 3                   TO LN-FEED
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DAY: "             TO D-ERR-PROMPT1
           MOVE TC-KEY-DATE TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-DD    TO D-ERR-DAY.
           MOVE "BRANCH: "          TO D-ERR-PROMPT2
           MOVE TC-BRNO             TO D-ERR-BRNO
           MOVE "CODE: "            TO D-ERR-PROMPT3
           MOVE "C"                 TO D-ERR-CODE.
           MOVE "CHECK NO.: "       TO D-ERR-PROMPT4
           MOVE TC-CKNO             TO D-ERR-CKNO
           MOVE "CLASS: "           TO D-ERR-PROMPT5
           MOVE TC-CLASS            TO D-ERR-CLASS
           MOVE "SEQUENCE: "        TO D-ERR-PROMPT6
           MOVE TC-SEQ              TO D-ERR-SEQ.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------------
       EXIT-WRITE-ERROR-LINE.
           EXIT.
      ******************************************************************
      *       RANGE LINES           *
      ******************************************************************
       PRINT-RANGE-LINES SECTION.
           MOVE 4 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE.

           MOVE "MONTH-END DATE" TO RANGE-LINE.
           MOVE EDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DAY OF MONTH" TO RANGE-LINE.
           MOVE DAY-OF-MONTH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MISC (ISO)" TO RANGE-LINE.
           MOVE MISC-CHECKS TO R-FG.
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME TO EMCKRG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EMCKRG-SYSDATE.
           DISPLAY EMCKRG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EMCKRG-FORM-FIELDS TO WR-BUF.
           MOVE EMCKRG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

       FORM-RESET-EXIT.
           EXIT.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE EMCKRG-HELP--FILE TO HELP-LINK-FILE.
           MOVE EMCKRG-HELP--DIR TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EMCKRG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************
       MISC-ROUTINES SECTION.
      ***************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      * COPY "LIBLP/LPTRC1RN.CPY".
      * COPYMEMBER: LIBLP/LPTR1RN
       LOAD-TRC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRC-ALL-DATA
              MOVE TRC-ALL-PATH             TO TRC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRC-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRC-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRC-ALL-DATA
              MOVE TRC-ALL-PATH             TO TRC-PATH.

      *-----------------------------------------------------------------
       OPEN-TRC1-FILE.
           PERFORM LOAD-TRC1-FILE.
           PERFORM OPEN-IT.
           MOVE TRC-PATH-SQL TO E-FILE.

           IF TRC1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO TRC1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRC1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-TRC1-FILE. *> SSP - TRC,GI
           PERFORM START-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.

           IF ( TRC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRC1-FILE.

           IF ( QTRC1-WEND-SEQ = ZEROES    ) OR
              ( QTRC1-WEND-SEQ NOT NUMERIC )
              MOVE TRC-PATH-OWNBR       TO QTRC1-WBEG-BRNO
              MOVE TRC-PATH-OWNBR       TO QTRC1-WEND-BRNO
              MOVE TC-KEY-DATE          TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRC1-WBEG-KEY-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRC1-WEND-KEY-DATE
              MOVE TC-CKNO              TO QTRC1-WBEG-CKNO
              MOVE TC-CLASS             TO QTRC1-WBEG-CLASS
              MOVE TC-SEQ               TO QTRC1-WBEG-SEQ
              MOVE ALL "9"              TO QTRC1-WEND-CKNO
                                           QTRC1-WEND-CLASS
                                           QTRC1-WEND-SEQ.

      * STORED PROCEDURE:
      *
      *   SELECT  TRCFILE.TC_BRNO,
      *           CAST(TRCFILE.TC_KEY_DATE AS VARCHAR(10)),
      *           TRCFILE.TC_CKNO,
      *           TRCFILE.TC_CLASS,
      *           TRCFILE.TC_SEQ,
      *           TRCFILE.TC_TYPE,
      *           CAST(TRCFILE.TC_DATE     AS VARCHAR(10)),
      *           TRCFILE.TC_AMT,
      *           TRCFILE.TC_PAYEE,
      *           TRCFILE.TC_REF,
      *           TRCFILE.TC_REASON,
      *           TRCFILE.TC_GLNO_1,
      *           TRCFILE.TC_GLNO_2,
      *           TRCFILE.TC_GLNO_3,
      *           TRCFILE.TC_GLNO_4,
      *           TRCFILE.TC_GLNO_5,
      *           TRCFILE.TC_GLAMT_1,
      *           TRCFILE.TC_GLAMT_2,
      *           TRCFILE.TC_GLAMT_3,
      *           TRCFILE.TC_GLAMT_4,
      *           TRCFILE.TC_GLAMT_5,
      *           TRCFILE.TC_VOID,
      *           TRCFILE.TC_ACCTNO,
      *           TRCFILE.TC_PAID,
      *           TRCFILE.TC_PGM,
      *           TRCFILE.TC_DLRVNO,
      *           TRCFILE.TC_DLNO,
      *           TRCFILE.TC_PAYEE_NO,
      *           ISNULL(GIFILE.GI_BRANCH, 0),
      *           ISNULL(GIFILE.GI_CLASS, 0),
      *           ISNULL(GIFILE.GI_CASH, 0),
      *           ISNULL(GIFILE.GI_BANK, 0),
      *           ISNULL(GIFILE.GI_DLPAYABLE, 0),
      *           ISNULL(GIFILE.GI_DLRVPRIN_1, 0),
      *           ISNULL(GIFILE.GI_DLRVPRIN_2, 0),
      *           ISNULL(GIFILE.GI_DLRVPRIN_3, 0),
      *           ISNULL(GIFILE.GI_DLRVCHGS_1, 0),
      *           ISNULL(GIFILE.GI_DLRVCHGS_2, 0),
      *           ISNULL(GIFILE.GI_DLRVCHGS_3, 0),
      *           ISNULL(GIFILE.GI_DLPARV, 0),
      *           ISNULL(GIFILE.GI_DIRECT_PAY, 0),
      *           ISNULL(GIFILE.GI_ACH_BANK, 0)
      * 
      *     FROM  DBO.TRCFILE TRCFILE
      * 
      *     LEFT OUTER JOIN
      *           DBO.GIFILE GIFILE
      *       ON  ( TRCFILE.TC_BRNO           = GIFILE.GI_BRANCH
      *      AND    TRCFILE.TC_CLASS          = GIFILE.GI_CLASS )
      * 
      *    WHERE    TRCFILE.TC_BRNO           = @QTRC1_WBEG_BRNO
      *      AND    TRCFILE.TC_KEY_DATE BETWEEN @QTRC1_WBEG_KEY_DATE  
      *                                     AND @QTRC1_WEND_KEY_DATE
      *      AND    TRCFILE.TC_TYPE      NOT IN 
      *                               ( 'CA', 'BK', 'RR', 'RC', 'RP', 
      *                                 'RD', 'ID', 'IW', 'DB ' )
      *      AND (( @QWS_MISC_CHECKS          = 'I' )
      *       OR  ( @QWS_MISC_CHECKS          = 'S'
      *      AND    TRCFILE.TC_TYPE          != 'MS' )
      *       OR  ( @QWS_MISC_CHECKS          = 'O'
      *      AND    TRCFILE.TC_TYPE           = 'MS' ))
      * 
      *    ORDER BY TRCFILE.TC_BRNO,
      *             TRCFILE.TC_CKNO,
      *             TRCFILE.TC_KEY_DATE,
      *             TRCFILE.TC_CLASS,
      *             TRCFILE.TC_SEQ;  
 
           MOVE MISC-CHECKS  TO QWS-MISC-CHECKS. 
 
           EXEC SQL SET CONNECTION TRC1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRC1_CURSOR CURSOR FOR
                :TRC1-RETURN-CODE = EXEC SSP_EMCKRG_TRLFILE_SELECT 
              ( :QTRC1-WBEG-BRNO,    
                :QTRC1-WBEG-KEY-DATE, 
                :QTRC1-WEND-KEY-DATE, 
                :QWS-MISC-CHECKS )      
           END-EXEC.
 
           EXEC SQL
               OPEN TRC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRC1-WBEG-BRNO
                          QTRC1-WEND-BRNO
                          QTRC1-WBEG-CKNO
                          QTRC1-WEND-CKNO
                          QTRC1-WBEG-CLASS
                          QTRC1-WEND-CLASS
                          QTRC1-WBEG-SEQ
                          QTRC1-WEND-SEQ.
           MOVE SPACES TO QTRC1-WBEG-KEY-DATE
                          QTRC1-WEND-KEY-DATE.

           MOVE STAT---GOOD TO TRC1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRC1-FILE-NEXT. *> EMBEDDED - TRC,GI
           PERFORM READ-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.
           INITIALIZE QTRC-REC.
           INITIALIZE QGI-REC.

           EXEC SQL SET CONNECTION TRC1_CONNECT END-EXEC.

           IF ( TRC1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH TRC1_CURSOR 
                 INTO 
                  :QTC-BRNO,
                  :QTC-KEY-DATE,
                  :QTC-CKNO,
                  :QTC-CLASS,
                  :QTC-SEQ,
                  :QTC-TYPE,
                  :QTC-DATE,
                  :QTC-AMT,
                  :QTC-PAYEE,
                  :QTC-REF,
                  :QTC-REASON,
                  :QTC-GLNO-1,
                  :QTC-GLNO-2,
                  :QTC-GLNO-3,
                  :QTC-GLNO-4,
                  :QTC-GLNO-5,
                  :QTC-GLAMT-1,
                  :QTC-GLAMT-2,
                  :QTC-GLAMT-3,
                  :QTC-GLAMT-4,
                  :QTC-GLAMT-5,
                  :QTC-VOID,
                  :QTC-ACCTNO,
                  :QTC-PAID,
                  :QTC-PGM,
                  :QTC-DLRVNO,
                  :QTC-DLNO,
                  :QTC-PAYEE-NO,
                  :QGI-BRANCH,  
                  :QGI-CLASS, 
                  :QGI-CASH,  
                  :QGI-BANK,  
                  :QGI-DLPAYABLE,  
                  :QGI-DLRVPRIN-1, 
                  :QGI-DLRVPRIN-2, 
                  :QGI-DLRVPRIN-3, 
                  :QGI-DLRVCHGS-1, 
                  :QGI-DLRVCHGS-2, 
                  :QGI-DLRVCHGS-3, 
                  :QGI-DLPARV,  
                  :QGI-DIRECT-PAY, 
                  :QGI-ACH-BANK 
              END-EXEC
           ELSE
              IF ( TRC1-CURSOR-SC-STAT-GOOD )
                 EXEC SQL
                      FETCH TRC1_CURSOR_SC INTO :QTRC-REC
                 END-EXEC
              ELSE
                 MOVE 9        TO SQLCODE
                 MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRC-FIELDS
              PERFORM GET-GI-FIELDS.

              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRC1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRC1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION TRC1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRC1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO TRC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-TRC1-CONNECTION.

           IF TRC1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION TRC1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT TRC1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO TRC1-CONN-OPEN-SW.

      *-----------------------------------------------------------------

       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".

      * COPY "LIBGL/GLGL1RN.CPY".
      * COPYMEMBER: LIBGL/GLGL1RN
       LOAD-GL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GL-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GL-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GL-ALL-MACHINE
              MOVE GL-ALL-PATH             TO GL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GL-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GL-ALL-BASE
              MOVE FILPATH-MACHINE         TO GL-ALL-MACHINE
              MOVE GL-ALL-PATH             TO GL-PATH.

      *-----------------------------------------------------------------
       OPEN-GL1-FILE.
           PERFORM LOAD-GL1-FILE.
           PERFORM OPEN-IT.
           MOVE GL-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-GL1-FILE. *> EMBEDDED - GLFILE
           PERFORM READ-IT.
           MOVE GL-PATH-SQL TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.
           INITIALIZE QGL-REC.

           MOVE GL-BR-ACCOUNT TO QGL-BR-ACCOUNT.

           EXEC SQL
            SELECT GLFILE.GL_BR_ACCOUNT,
                   GLFILE.GL_DESCRIPTION 
              INTO 
                  :QGL-BR-ACCOUNT,  
                  :QGL-DESCRIPTION
              FROM DBO.GLFILE
            WHERE GLFILE.GL_BR_ACCOUNT = :QGL-BR-ACCOUNT
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GL-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GL1-FILE.
           PERFORM CLOSE-IT.
 

       COPY "LIBGB/GBWKIN.CPY".
      *COPY "LIBGB/GBWK2IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
      *    MOVE WK2-PATH TO ACCESS-BUF.
      *    PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
                 MOVE SV-LOCAL-PRINT-FG TO LOCAL-PRINT-FG
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY EMCKRG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
