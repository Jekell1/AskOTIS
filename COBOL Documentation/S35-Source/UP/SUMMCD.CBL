       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SUMMCD.
       AUTHOR.        KEC.
       DATE-WRITTEN.  11/01/2018.
      ******************************************************************
      *
      *   PROGRAM NAME:  UP/SUMMCD
      *
      *   DESCRIPTION :  SUMMARY OF CODE FILE - FOR A32 & A34
      *                  COUNT THE NUMBER OF RECORDS:
      *                  - EACH TYPE (CD-TYPE)
      *                  - EACH DUPLICATE KEYS
      *                  - EACH DUPLICATE KEYS WITH DIFFERENT DATA
      *
      *
      * REV:
      * KEC 2018-1101 ***NEW***
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBSP/SPFSREPORT.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       
       COPY "LIBSP/SPFDREPORT.CPY".
       01  REPORT-REC.
       
           03  REPORT-KEY.
       
               05  REPORT-MACHINE                    PIC X(02).

               05  REPORT-TYPE                       PIC X(03).
                   88  REPORT-TYPE-TOTAL                 VALUE "ZZZ".

               05  REPORT-STAT                       PIC X(01).
                   88  REPORT-STAT-DUP-SAME              VALUE "A".
                   88  REPORT-STAT-DUP-DIFF              VALUE "B".
                   88  REPORT-STAT-UNIQUE                VALUE "C".
                   88  REPORT-STAT-TOTAL                 VALUE "Z".

               05  REPORT-CD1-KEY                    PIC X(20).

           03  REPORT-COUNT                          PIC 9(11).

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/SUMMCD.CBL S34 06/10/21-12:33:33 >".

       77  SUMMCD-WINDOW-HANDLE        PIC X(10).

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBSP/SPWSREPORT.CPY".
       
       01  PR-IFN                      PIC X(02)     VALUE "PR".
 
      *-----------------------------------------------------------------
      *   CURR-KEY IS A COPY OF REPORT-KEY FOR READ/WRITE REPORT FILE
      *   HOLD-KEY IS A COPY OF REPORT-KEY FOR UPDATE-REPORT-ROUTINE
      *-----------------------------------------------------------------
       01  CURR-KEY.
           03  CURR-MACHINE                        PIC X(02).
           03  CURR-TYPE                           PIC X(03).
           03  CURR-STAT                           PIC X(01).
           03  CURR-CD1-KEY                        PIC X(20).

       01  HOLD-KEY.
           03  HOLD-MACHINE                        PIC X(02).
           03  HOLD-TYPE                           PIC X(03).
           03  HOLD-STAT                           PIC X(01).
               88  HOLD-STAT-DUP-SAME                  VALUE "A".
               88  HOLD-STAT-DUP-DIFF                  VALUE "B".
               88  HOLD-STAT-UNIQUE                    VALUE "C".
               88  HOLD-STAT-TOTAL                     VALUE "Z".
           03  HOLD-CD1-KEY                        PIC X(20).
           03  FILLER                    REDEFINES HOLD-CD1-KEY.
               05  HOLD-CD-TYPE                    PIC X(02).
               05  HOLD-CD-CODE                    PIC X(03).
               05  HOLD-CD-FILLER                  PIC X(15).
           03  HOLD-CD-ACTION-KEY        REDEFINES HOLD-CD1-KEY.
               05  HOLD-CD-ACTION-TYPE             PIC X(01).
               05  HOLD-CD-ACTION-CODE             PIC X(04).
               05  HOLD-CD-ACTION-FILLER           PIC X(15).

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ERRCD                       PIC X(01).
       01  EXITPROG                    PIC X(09)     VALUE "LP/LPLSMU".

       01  LEGEND                      PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

       01  PRINT-LINE-WORKERS.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.
           03  PAGE-NUMBER             PIC 9(04)     VALUE ZEROES.
 
       01  RECORDS-FOUND               PIC X(01)     VALUE "X".
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

      ******************************************************************
      *
      *    H E A D E R - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-01.
           03  H01-DATE                PIC X(08)     VALUE SPACES.
           03  FILLER                  PIC X(02)     VALUE SPACES.
           03  H01-HR                  PIC 9(02)     VALUE ZEROES.
           03  FILLER                  PIC X(01)     VALUE ":".
           03  H01-MIN                 PIC 9(02)     VALUE ZEROES.
           03  FILLER                  PIC X(31)     VALUE SPACES.
           03  H01-NAME                PIC X(40)     VALUE SPACES.
           03  FILLER                  PIC X(37)     VALUE SPACES.
           03  FILLER                  PIC X(05)     VALUE "PAGE ".
           03  H01-PAGE-NO             PIC ZZZ9.
 
      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10)     VALUE "USER ID:".
           03  H02-USERID              PIC X(10).
           03  FILLER                  PIC X(92)     VALUE
                                         "SUMMARY OF CODE FILE LISTING".
           03  FILLER                  PIC X(20)     VALUE SPACES.
 
      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER PIC X(30) VALUE "MACH TYPE  DESCRIPTION        ".
           03  FILLER PIC X(30) VALUE "       COUNT/KEY              ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.
 
      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                 PIC X(132)    VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01              REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(01).
           03  D01-MACHINE             PIC X(02).
           03  FILLER                  PIC X(02).
           03  D01-TYPE                PIC X(05).
           03  FILLER                  PIC X(03).
           03  D01-STAT                PIC X(25).
           03  FILLER                  PIC X(01).
           03  D01-CD1-KEY             PIC X(93).
           03  FILLER                  REDEFINES D01-CD1-KEY.
               05  D01-REC-COUNT       PIC ZZ,ZZZ,ZZZ,ZZ9.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
 
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUP/SUMMCD_DEF.CPY".
       COPY "LIBUP/SUMMCD_WKS.CPY".

      ******************************************************************
      *
      *    L I N K A G E - S C R E E N - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBUP/SUMMCD_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE REPORT-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CD1-FILE.
           CLOSE REPORT-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM INITIALIZATION.

           IF ( ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM SUMMARY-R1-MODULE.

           PERFORM PRINT-REPORT-MODULE.

       
       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
       
       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SUMMCD" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SUMMCD-WINDOW-HANDLE.

           PERFORM FORM-RESET.
       
           PERFORM ENTER-PARAMETERS.
           IF ( ERRCD NOT = SPACES )
              GO TO INITIALIZATION-EXIT.

           MOVE "N" TO RECORDS-FOUND.
       
           MOVE "LISTING IN PROGRESS........................" TO LEGEND.
           PERFORM SEND-LEGEND.
       
           MOVE EXT-FILPATH-BASE        TO CD-ALL-BASE
           MOVE "R1"                    TO CD-ALL-MACHINE
           CALL "C$TOLOWER" USING CD-ALL-MACHINE, VALUE 2.
           MOVE CD-ALL-PATH             TO CD-PATH
           MOVE "Y"                     TO CD-PATH-OVERRIDE.
           PERFORM OPEN-CD1-FILE.

           PERFORM LOAD-REPORT-FILE.
           PERFORM OPEN-REPORT-FILE.
       
           PERFORM OPEN-PR-FILE.

           MOVE SPACES      TO ERRCD.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       
       ENTER-PARAMETERS-BEGIN.

           MOVE SPACES TO ERRCD.

       
       ENTER-PRINT-OPTIONS.

           MOVE SUMMCD-QUEUE-POS           TO Q-POS.
           MOVE SUMMCD-COPIES-POS          TO C-POS.
           MOVE SUMMCD-PRU-POS             TO P-POS.
           MOVE SPACES                     TO Q-BUF  C-BUF  P-BUF.
           PERFORM PRU-IN.

           IF ( STAT-BAD )
              MOVE "1>" TO VDUSTATUS
              GO TO ENTER-PARAMETERS-END.

       
       ENTER-ACCEPT-YN.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-ACCEPT-YN.

       
           IF ( VDUBUF = "Y" ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( VDUBUF = "N" ) GO TO ENTER-PARAMETERS-BEGIN.

           GO TO ENTER-ACCEPT-YN.

       
       ENTER-PARAMETERS-END.

           IF ( F7-KEY ) MOVE "X" TO ERRCD.

       
       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       SUMMARY-R1-MODULE SECTION.
      ******************************************************************

           MOVE SPACES TO CD1-KEY.
           PERFORM START-CD1-FILE. 
       
       SUMMARY-R1-READ-NEXT.

           PERFORM READ-CD1-FILE-NEXT.
           IF ( IO-BAD )
              MOVE "C"                     TO HOLD-STAT
              GO TO SUMMARY-R1-MODULE-EXIT.

      * SET HOLD-KEY FOR USE IN UPDATE-REPORT-ROUTINE
       
           MOVE "A"                        TO HOLD-STAT.

           MOVE "R1"                       TO HOLD-MACHINE.
           MOVE CD1-KEY                    TO HOLD-CD1-KEY.

           IF ( HOLD-CD-ACTION-TYPE = "{" )
              MOVE HOLD-CD-ACTION-TYPE     TO HOLD-TYPE
           ELSE
              MOVE HOLD-CD-TYPE            TO HOLD-TYPE.

           PERFORM UPDATE-REPORT-ROUTINE.

           GO TO SUMMARY-R1-READ-NEXT.
       
       SUMMARY-R1-MODULE-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-REPORT-ROUTINE SECTION.
      ******************************************************************
       
           IF ( HOLD-STAT-DUP-DIFF )
              MOVE HOLD-MACHINE            TO CURR-MACHINE
              MOVE HOLD-TYPE               TO CURR-TYPE
              MOVE HOLD-STAT               TO CURR-STAT
              MOVE HOLD-CD1-KEY            TO CURR-CD1-KEY
              PERFORM UPDATE-REPORT-RWR.

       
           MOVE HOLD-MACHINE               TO CURR-MACHINE.
           MOVE HOLD-TYPE                  TO CURR-TYPE.
           MOVE HOLD-STAT                  TO CURR-STAT.
           MOVE ALL "Z"                    TO CURR-CD1-KEY.
           PERFORM UPDATE-REPORT-RWR.

           MOVE HOLD-MACHINE               TO CURR-MACHINE.
           MOVE HOLD-TYPE                  TO CURR-TYPE.
           MOVE ALL "Z"                    TO CURR-STAT.
           MOVE ALL "Z"                    TO CURR-CD1-KEY.
           PERFORM UPDATE-REPORT-RWR.

       
           MOVE HOLD-MACHINE               TO CURR-MACHINE.
           MOVE ALL "Z"                    TO CURR-TYPE.
           MOVE ALL "Z"                    TO CURR-STAT.
           MOVE ALL "Z"                    TO CURR-CD1-KEY.
           PERFORM UPDATE-REPORT-RWR.

       
       UPDATE-REPORT-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-REPORT-RWR SECTION.
      ******************************************************************

      * READ THEN WRITE/REWRITE OF THE REPORT-FILE

           MOVE CURR-KEY                   TO REPORT-KEY.
           PERFORM READ-REPORT-FILE.
           IF ( IO-BAD )
              MOVE SPACES                  TO REPORT-REC
              MOVE CURR-KEY                TO REPORT-KEY
              MOVE ZEROES                  TO REPORT-COUNT
              PERFORM WRITE-REPORT-FILE
              IF ( IO-BAD )
                 GO TO IO-ERROR.

           ADD 01 TO REPORT-COUNT.
           PERFORM REWRITE-REPORT-FILE.

       UPDATE-REPORT-RWR-EXIT.
           EXIT.

      ******************************************************************
       PRINT-REPORT-MODULE SECTION.
      ******************************************************************

           MOVE SPACES TO REPORT-KEY.
           PERFORM START-REPORT-FILE.

       PRINT-REPORT-MODULE-NEXT.

           PERFORM READ-REPORT-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PRINT-REPORT-MODULE-EXIT.

           MOVE REPORT-MACHINE             TO D01-MACHINE.

           IF ( REPORT-TYPE-TOTAL )
              MOVE "TOTAL"                 TO D01-TYPE
           ELSE
              MOVE REPORT-TYPE             TO D01-TYPE.

           IF ( REPORT-STAT-DUP-SAME )
              MOVE "DUPLICATE & SAME RECORD  " TO D01-STAT
           ELSE
           IF ( REPORT-STAT-DUP-DIFF )
              MOVE "DUPLICATE & DIFF RECORD  " TO D01-STAT
           ELSE
           IF ( REPORT-STAT-UNIQUE )
              MOVE "UNIQUE RECORD            " TO D01-STAT
           ELSE
              MOVE "TOTAL                    " TO D01-STAT.

           IF ( REPORT-CD1-KEY = ALL "Z" )
              MOVE REPORT-COUNT            TO D01-REC-COUNT
           ELSE
              MOVE REPORT-CD1-KEY          TO D01-CD1-KEY.

           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-MODULE-NEXT.

       
       PRINT-REPORT-MODULE-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( LN-WK > EXT-CONAME-RPT-LINES)
              PERFORM WRITE-HEADER-MODULE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD  LN-FEED TO LN-COUNT.
           MOVE 1       TO LN-FEED.
           MOVE SPACES  TO DETAIL-LINE.

       
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       WRITE-HEADER-MODULE SECTION.
      ******************************************************************

           ADD 1 TO PAGE-NUMBER.
           PERFORM GET-TIME.

       
           MOVE EXT-CONAME-CONAME          TO H01-NAME.
           MOVE EXT-CONAME-SYSDATE         TO H01-DATE.
           MOVE PAGE-NUMBER                TO H01-PAGE-NO.
           MOVE T-HH                       TO H01-HR.
           MOVE T-MM                       TO H01-MIN.

           MOVE HEADER-LINE-01             TO PR-REC.
           MOVE 99                         TO LN-FEED.
           PERFORM WRITE-PR-REC.
 
       
           MOVE EXT-CONAME-USER-ID         TO H02-USERID.
           MOVE HEADER-LINE-02             TO PR-REC.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.
 
       
           MOVE HEADER-LINE-03             TO PR-REC.
           MOVE 02                         TO LN-FEED.
           PERFORM WRITE-PR-REC.
 
       
           MOVE ALL "="                    TO PR-REC.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
       WRITE-HEADER-MODULE-END.

           MOVE 05 TO LN-COUNT.
           MOVE 01 TO LN-FEED.

       
       WRITE-HEADER-MODULE-EXIT.
           EXIT.

      ******************************************************************
       FORM-RESET SECTION.
      ******************************************************************

           MOVE EXT-CONAME-CONAME          TO SUMMCD-CONAME.
           MOVE EXT-CONAME-SYSDATE         TO SUMMCD-SYSDATE.
           DISPLAY SUMMCD-SCREEN         UPON SUMMCD-WINDOW-HANDLE.
           MOVE SUMMCD-FORM-FIELDS         TO WR-BUF.
           MOVE SUMMCD-FORM-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

       
       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************************

           MOVE SUMMCD-HELP--FILE          TO HELP-LINK-FILE.
           MOVE SUMMCD-HELP--DIR           TO HELP-LINK-DIR.

       
       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************************

           MOVE STAT---GOOD                TO STAT.
           MOVE VDU-FIELD-TO-CONVERT       TO VDU-ALP-FIELD.

           EVALUATE TRUE
              COPY "LIBUP/SUMMCD_EVA.CPY".
              WHEN OTHER         MOVE "2A" TO STAT
           END-EVALUATE.

       
       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       
       SEND-LEGEND.
           MOVE LEGEND-LIST                TO WR-LIST.
           MOVE LEGEND                     TO WR-BUF.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBSP/SPIOREPORT.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

           PERFORM PRU-OUT.

       EXIT-PROG.
           MOVE EXITPROG  TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY SUMMCD-WINDOW-HANDLE.
           EXIT PROGRAM.
