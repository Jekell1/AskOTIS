       IDENTIFICATION DIVISION.
       PROGRAM-ID.  QUEPRU.
      ******************************************************************
      *  DIR : GB
      *  DESC: PRINT FILE RESERVATION REMOVAL
      *        FOR SPOOLED PRINTS QUEUE TEMP FILE
      *        FOR DIRECT PRINTS ENABLE SPOOLER,
      *        REMOVE PRINTER LOCK FILE
      *
      *=================================================================
      * REV:
      * BLV 981113 CHANGED PRU-PATHX FROM 65 BYTES TO 90 BYTES
      *             TO MATCH PRU-PATH IN GETPR (FIELD IS PASSED)
      * MG  991008 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * BAH 130102 CHANGED SYSTEM-CALL TO C$FILEINFO FOR CHECKING
      *             IF REPORT WAS CREATED OR NOT
      *
      * JKC 2020-1026 ADDED WORKERS AND LOGIC FOR DISPLAYING TIMER
      *         DATA. THE ROUTINES ARE ALL IN PRINT-HOLD PARAGRAPH.
      *         WORKS WITH GB/TIMER.CBL TO GET DATA.
      * BAH 2022.0323 INCREASED THE TIMER-MESS-DESC BY 20 CHARACTERS SO THE
      *               FULL PATH OF A BATCH REPORT WILL DISPLAY
      *
      * SMR 2024-0401 ADDED TIMER-BEG-DATE TO BE USED FOR CALCULATING
      *          THE ELAPSED TIME IF THE END TIME CROSS OVER TO A NEW
      *          DAY.
      *
      * JKC 2024-0501 ADDED USE OF EXT-API-SCREEN-DISABLE AND
      *         EXT-API-MESS FOR SUPPRESS SCREEN INTERACTIONS FOR
      *         USE IN API PROGRAMS WITHOUT SCREENS.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/QUEPRU.CBL S35 05/01/24-21:38:17 >".

       01  FILEINFO.
           03  FILESIZE        PIC X(8) COMP-X.
           03  FILEDATE        PIC 9(8) COMP-X.
           03  FILETIME        PIC 9(8) COMP-X.

      *=================================================================
      * START   : 01:00:00 AM
      * FINISH  : 12:59:59 PM
      * ELAPSTED: 00:11:22:33 (DD:HH:MM:SS)
      * MS EXCEL: 12.3456789
      *=================================================================
       01  TIMER-DISPLAY-BUF.
           04  TIMER-DISPLAY-MESS      PIC X(60) OCCURS 6 TIMES.

       01  TIMER-MESS.
           03  TIMER-MESS-TEXT         PIC X(10).
           03  TIMER-MESS-TIME         PIC X(11).
           03  TIMER-MESS-EXCEL        REDEFINES TIMER-MESS-TIME
                                       PIC 99.9(7).
           03  TIMER-MESS-DESC         PIC X(39).

      *-----------------------------------------------------------------
       01  ELE                         PIC 9(02)     VALUE ZEROES.
       01  HOLD-P-TIMER-TYPE           PIC X(01)     VALUE SPACES.

      *=================================================================
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".

       COPY "LIBGB/FILEWK.CPY".
       01  EXT-EOBUF-MESS       REDEFINES MESS PIC X(61).
      * PRINT OFF:
       01  CMD-TPUT-MC4  PIC X(8)  VALUE "TPUT MC4".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".

       COPY "LIBGB/GETFMW.CPY".

      * MUST MATCH SIZE OF PRU-PATH IN LIBGB/GETPRW
       01  PRU-PATHX    PIC X(90).
       01  STATX        PIC XX.
       COPY "LIBGB/PRU_LNK.CPY".

       PROCEDURE DIVISION
           USING STATX PRU-PATHX FORM-PATHNAME PRU-LINK EXIT-PATHNAME.

      *********************************************
       MAIN-PROGRAM SECTION.
      *********************************************
      D    STOP MESS.

           MOVE PRU-PATHX TO PRU-PATH.

      * PRINT FILE RESERVED ?
           IF PRU-PATH = " "
              GO TO EXIT-PROG.

           IF NOT Q-NONDISP
              IF NOT Q-POS-VALID
                 GO TO EXIT-PROG.
           IF NOT P-NONDISP
              IF NOT P-POS-VALID
                 GO TO EXIT-PROG.

      * PRINT
           IF PRU-LP = "PRU"
              GO TO PRINT-PRU.

      * V TO VIEW, CREATES FILE IN /USR/TMP
           IF PRU-LP = "HLD"
              GO TO PRINT-HOLD.

      *******************************************
      *  VIEW PRINT FILE, CREATES IN /USR/TMP
      *******************************************
       PRINT-HOLD.
      *    MOVE PRU-FILE TO TEST-0-FILE.
      *    MOVE TEST-0 TO SYSTEM-BUF.
           CALL "C$FILEINFO" USING PRU-FILE, FILEINFO RETURNING STAT-99.

           IF STAT-BAD OR FILESIZE = 0
              MOVE "NO REPORT GENERATED" TO MESS
           ELSE
              MOVE SPACES TO MESS
              STRING "REPORT HELD IN: " PRU-FILE
                     DELIMITED BY SIZE INTO MESS.

           IF ( P-TIMER-DAYSEC-BEG NOT NUMERIC ) OR  *> TIMER NOT USED
              ( P-TIMER-DAYSEC-BEG = ZEROES    ) OR  *> TIMER NOT USED
              ( NOT P-TIMER-TYPE-VALID         ) OR  *> TIMER NOT USED 
              ( P-TIMER-TYPE-UTILITY           )     *> DO NOT DISPLAY
              PERFORM SEND-MESS
              GO TO EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    PERFORM TIMER-STOP FUNCTION (GET END AND ELAPSED TIMES).
      *    SAVE P-TIMER-TYPE THEN FORCE IT TO "U" (UTILITY; NO DISPLAY,
      *    RETURN DATA) SO NOT TO DISPLAY RESULTS IN TIMER PROGRAM;
      *    NEED TO DISPLAY RESULTS WITH REPORT STATUS WITHIN HERE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE P-TIMER-TYPE TO HOLD-P-TIMER-TYPE.
           MOVE "U" TO P-TIMER-TYPE.     *> UTILITY OPTION; RETURN DATA
           MOVE "GB/TIMER" TO FORM-NAM.
           CALL FORM-PROGX USING P-TIMER-LINK-BUF.
           CANCEL FORM-PROGX.
           MOVE HOLD-P-TIMER-TYPE TO P-TIMER-TYPE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY RESULTS IN BOX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES                     TO ELE.
           MOVE SPACES                     TO TIMER-DISPLAY-BUF.

           ADD 01 TO ELE.
           MOVE SPACES                     TO TIMER-MESS
           MOVE "START   :"                TO TIMER-MESS-TEXT.
           MOVE P-TIMER-BEG-EDIT           TO TIMER-MESS-TIME.
           MOVE P-TIMER-BEG-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMSDDSYY.
           STRING " | ",DATE-MMSDDSYY INTO TIMER-MESS-DESC.
           MOVE TIMER-MESS                 TO TIMER-DISPLAY-MESS(ELE).

           ADD 01 TO ELE.
           MOVE SPACES                     TO TIMER-MESS
           MOVE "FINISH  :"                TO TIMER-MESS-TEXT.
           MOVE P-TIMER-END-EDIT           TO TIMER-MESS-TIME.
           ACCEPT DATE-YYYYMMDD FROM CENTURY-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMSDDSYY.
           STRING " | ",DATE-MMSDDSYY INTO TIMER-MESS-DESC. 
           MOVE TIMER-MESS                 TO TIMER-DISPLAY-MESS(ELE).

           ADD 01 TO ELE.
           MOVE SPACES                     TO TIMER-MESS
           MOVE "ELAPSED :"                TO TIMER-MESS-TEXT.
           MOVE P-TIMER-ELAPSED-EDIT       TO TIMER-MESS-TIME.
           MOVE " (DD:HH:MM:SS)"           TO TIMER-MESS-DESC.
           MOVE TIMER-MESS                 TO TIMER-DISPLAY-MESS(ELE).

           IF ( P-TIMER-TYPE-EXCEL )
              ADD 01 TO ELE
              MOVE SPACES                  TO TIMER-MESS
              MOVE "MS EXCEL:"             TO TIMER-MESS-TEXT
              MOVE P-TIMER-DAYSEC-ELAPSED  TO TIMER-MESS-EXCEL
              MOVE TIMER-MESS              TO TIMER-DISPLAY-MESS(ELE).

           ADD 02 TO ELE.
           MOVE MESS                      TO TIMER-DISPLAY-MESS(ELE).

           IF ( EXT-API-SCREEN-DISABLE = "Y" )
              GO TO EXIT-PROG.          *> SKIP DISPLAY OF MESSAGE BOX

           IF ( P-TIMER-TYPE-EXCEL )
              DISPLAY MESSAGE BOX TIMER-DISPLAY-MESS(1), X"0A"
                                  TIMER-DISPLAY-MESS(2), X"0A"
                                  TIMER-DISPLAY-MESS(3), X"0A"
                                  TIMER-DISPLAY-MESS(4), X"0A"
                                  TIMER-DISPLAY-MESS(5), X"0A"
                                  TIMER-DISPLAY-MESS(6), X"0A"
           ELSE
              DISPLAY MESSAGE BOX TIMER-DISPLAY-MESS(1), X"0A"
                                  TIMER-DISPLAY-MESS(2), X"0A"
                                  TIMER-DISPLAY-MESS(3), X"0A"
                                  TIMER-DISPLAY-MESS(4), X"0A"
                                  TIMER-DISPLAY-MESS(5), X"0A".

           GO TO EXIT-PROG.

      *********************************
      * PRINT
      *******************************
       PRINT-PRU.

      *    CALL "C$SLEEP" USING 2.

      * STTY -IXON;TPUT MC4
           CALL "C$TOLOWER" USING CMD-TPUT-MC4, VALUE 8.
           MOVE CMD-TPUT-MC4 TO SYSTEM-BUF.
      * THIS MUST STAY C$SYSTEM OR GARBAGE LOCKS UP PRINTER, NO IDEA WHY
      *    PERFORM SYSTEM-CALL.
           CALL "C$SYSTEM" USING SYSTEM-BUF 2 GIVING STAT-99.

      *    CALL "C$SLEEP" USING 2.

           GO TO EXIT-PROG.

      **********************************
       MISC SECTION.
      **********************************
       COPY "LIBGB/SENDMESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/DATER.CPY".
       EXIT-PROG.
           EXIT PROGRAM.
