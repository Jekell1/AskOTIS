      * A30 CONVERSION  , STRIPPED OUT BRANCH FILE DEFAULTS AND QUEUEING
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TSTPRU.
      *********************************************************
      *  DIR : GB
      *
      *          PRU-IN
      *
      *  DESC: PRINT FILE RESERVATION
      *        FOR SPOOLED PRINTS CREATE TEMP NAME
      *        FOR DIRECT PRINTS DISABLE SPOOLER,
      *        ATTEMPT EXCLUSIVE RESERVATION
      *
      * REV:
      *  MJD 102396 ADDED PRU-QFG = 'S'  TO RESTRICT QUEUE OPTIONS TO
      *             'Y' OR 'N' IF CALLED FROM PERUSE.
      *             ADDED PRU-QFG = 'U'  TO BLOCK PIGGYBACK PRINTING.
      *  BLV 981113 CHANGED PRU-PATHX FROM 65 BYTES TO 90 BYTES SO IT
      *             MATCHES PRU-PATH IN GETPR (THIS IS PASSED)
      *  MJD 990504 ADDED PRU-QFG = "V" TO FORCE PRINT TO DISK.
      *             (USED IN SP/BPBKBR)
      *  MG  991008 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  MJD 121023 FORCE PRU TO BE 00 - A30
      *  BAH 121218 ONLY VALUE QUEUE BUF FLAGS NOW ARE "P" FOR PRINT,
      *             "N" WHICH IS OUR OLD DIRECT PRINT AND WILL DO THE SAME
      *             AS "P", JUST WASNT READY TO REMOVE IT, AND "V" VIEW
      *  BAH 20210208 ADDED "S" TO THIS 'IF' SO GB/PERUSE WOULD DEFAULT TO
      *               QUEUE "Y" LIKE A15 DOES.
      *               IF PRU-QFG = "N" OR "D" OR "P" OR "S"
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.

       INPUT-OUTPUT SECTION.
       I-O-CONTROL.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)GB/TSTPRU S35 04/19/24-08:52:21 >".


       01  SUB1         PIC 99.
       01  SUB2         PIC 99.
       01  STAT2        PIC XX.
       01  PRU-FLAGS.
           03  PRU-QFG  PIC X.
           03  FILLER   PIC X.

       COPY "LIBGB/CONAME_EXT.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       01  EXT-EOBUF-MESS   REDEFINES MESS PIC X(61).
       COPY "LIBGB/PRINT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
      *COPY "LIBGB/GETPRUW.CPY".
       COPY "LIBGB/GETPRW.CPY".
      *    SIZE OF THE FOLLOWING ITEM LIMITED TO 29
      *    BY RM COBOL EXTERNAL FILE NAME CONVENTION;
      *    ALSO SEE PRU-PATHX BELOW.
           03  PRU-CHAR REDEFINES PRU-FILE.
               05  PRU-C   PIC X OCCURS 29.
       01  PRU-PATH-MAX    PIC 99 COMP VALUE 29.

       COPY "LIBGB/GETFMW.CPY".

      * MUST MATCH SIZE OF PRU-PATH IN LIBGB/GETPRW
       01  PRU-PATHX    PIC X(90).
       01  STATX        PIC XX.
       COPY "LIBGB/PRU_LNK.CPY".

       PROCEDURE DIVISION
           USING STATX PRU-PATHX FORM-PATHNAME PRU-LINK EXIT-PATHNAME.

      ****************************************
       MAIN-PROGRAM SECTION.
      ****************************************
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE PRU-PATHX TO PRU-PATH.


      * SAVE FORCED PRINT OPTIONS
           MOVE STATX TO PRU-FLAGS.

      *    -------------------------------------------------------------
      * THIS IS CHECKING IF THE PRINTER FIELDS WERE NON DISPLAY FIELDS
      * LIKE IN BATCH OR CHECK WRITING
      *    -------------------------------------------------------------
           IF NOT Q-NONDISP
              IF NOT Q-POS-VALID
                 GO TO  END-PROG
              END-IF
           ELSE
              IF NOT Q-BUF-VALID
                 GO TO END-PROG.

           IF NOT C-NONDISP
              IF NOT C-POS-VALID
                 GO TO  END-PROG
              END-IF
           ELSE
              IF NOT C-BUF-VALID
                 GO TO END-PROG.

           IF NOT P-NONDISP
              IF NOT P-POS-VALID
                 GO TO  END-PROG
              END-IF
           ELSE
              IF NOT PRU-VALID
                 GO TO END-PROG.


      * DEFAULT QUEUE & COPIES
           MOVE "V" TO Q-BUF.
           MOVE "1" TO C-BUF.

      * F1 BACK TO PRINTER #:
      * IF PRINT, CLEAR PRU-PATH
           IF PRU-LP = "PRU"
              MOVE " " TO PRU-PATH.

      *****************************************
      * FORCED PRINT OPTIONS:
      * N - DIRECT OR TRANSPARENT ONLY  "N" 00 = PIGGYBACK
      * D - DIRECT ONLY
      * P - TRANSPARENT ONLY  (PIGGY BACK)
      * Y - SPOOL ONLY
      * T - TERMINAL ONLY
      * V - WRITE TO DISK
      * S - QUEUE OR PIGGYBACK ONLY (CALLED FROM PERUSE PRINT)
      * U - PIGGYBACK PRINTING NOT ALLOWED (CANNOT USE PR# 00).
      * * - ANY PRINT OPTION
      * CHECK FORCED DIRECT PRINT
      *****************************************
           IF PRU-QFG = "N" OR "D" OR "P" OR "S"
              MOVE "N" TO Q-BUF.
           IF PRU-QFG = "V"
              MOVE "V" TO Q-BUF.

           IF NOT Q-NONDISP
              DISPLAY Q-BUF
                 AT Q-POS
                 SIZE 1
                 WITH NO ADVANCING
              END-DISPLAY.

           GO TO READ-P.
       READ-Q.
           IF Q-NONDISP
              IF NOT Q-BUF-VALID
                 GO TO END-PROG
              END-IF
           ELSE
      *       DISPLAY "_"
      *          AT Q-POS
      *       END-DISPLAY
              MOVE "00" TO VDUSTATUS
              ACCEPT Q-BUF
                 AT Q-POS
                 PROTECTED SIZE 1
                 DEFAULT Q-BUF
                 UPPER
      *          ECHO
                 ON EXCEPTION VDU-KEY-DEST PERFORM SET-STATUS
              END-ACCEPT
              IF NOT ENTER-KEY
                 GO TO READ-Q
              END-IF
              IF NOT Q-BUF-VALID
                 GO TO READ-Q.

      *    ONLY ALLOW SPOOL TO DISK
           IF PRU-QFG = "V"
              IF Q-BUF NOT = "V"
                 GO TO READ-Q.
      *    IF CALLED FROM PERUSE ONLY ALLOW PIGGYBACK OR LOCAL PRINTER
           IF PRU-QFG = "S"
              IF Q-BUF NOT = "N" AND NOT = "P"
                 GO TO READ-Q.
           MOVE "1" TO C-BUF.
           IF NOT Q-NONDISP
              DISPLAY Q-BUF
                 AT Q-POS
                 SIZE 1
                 WITH NO ADVANCING
              END-DISPLAY.

           GO TO READ-P.

       RESET-P.
           IF NOT P-POS-VALID
              GO TO END-PROG.

           IF NOT P-NONDISP
              DISPLAY "_"
                 AT P-POS
              END-DISPLAY.

       READ-P.
           IF P-NONDISP
              IF NOT PRU-VALID
                 GO TO END-PROG
              END-IF
           ELSE
              MOVE "00" TO VDUSTATUS

              MOVE "00" TO P-BUF
              ACCEPT P-BUF
                 AT P-POS
                 PROTECTED SIZE 2
                 DEFAULT P-BUF
                 UPPER
      *          ECHO
                 ON EXCEPTION VDU-KEY-DEST PERFORM SET-STATUS
              END-ACCEPT
              IF UP-KEY OR F2-KEY
      * IF QUEUE STATUS IS FORCED, RETURN STAT (18/12) SO THAT
      * CALLING PROGRAM MAY ACT ON IT
                 IF PRU-QFG = "Y" OR PRU-QFG = "N" OR PRU-QFG = "D" OR
                    PRU-QFG = "P" OR PRU-QFG = "T" OR PRU-QFG = "V"
                    MOVE " " TO PRU-PATH
                    GO TO END-PROG
                 ELSE
                    GO TO READ-Q
                 END-IF
              END-IF
              IF F7-KEY
                 MOVE " " TO PRU-PATH
                 GO TO END-PROG
              END-IF
              IF F8-KEY
                 MOVE P-CALLING-DIR TO HELP-LINK-DIR
                 MOVE P-CALLING-PROG TO HELP-LINK-FILE
                 MOVE "PRU" TO HELP-LINK-FIELD
                 MOVE "GB/HELP" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATHNAME HELP-LINK-BUF
                 CANCEL FORM-PROGX
                 GO TO READ-P
              END-IF
              IF NOT ENTER-KEY
                 GO TO READ-P
              END-IF
              IF NOT PRU-VALID
                 GO TO READ-P.

           IF PRU NOT NUMERIC
              GO TO RESET-P.

      * PRU-QFG (FORCE) U
           IF PRU-QFG = "U"
              IF Q-BUF = "N"
                 IF P-BUF = "00"
                    GO TO RESET-P.
      * PRU-QFG (FORCE) S
           IF PRU-QFG = "S"
              IF Q-BUF = "N"
                 IF P-BUF NOT = "00"
                    GO TO RESET-P.
      * PRU-QFG (FORCE) V
           IF PRU-QFG = "V"
              IF Q-BUF = "V"
                 GO TO PRINT-HOLD
              ELSE
                 GO TO RESET-P.

      * Q-BUF OF V = PREPARE TO DISK (FOR PERUSE)
           IF Q-BUF = "V"
              GO TO PRINT-HOLD.

      * PRU-QFG (FORCE) N, D, P
           IF PRU-QFG = "P" AND P-BUF NOT = "00"
              GO TO RESET-P.

           IF PRU-QFG = "D" AND P-BUF = "00"
              GO TO RESET-P.


      * IF NOT Q-BUF OF A "V", IT WILL FALL INTO HERE

      ************************************
      *  PRINTER 00
      ************************************
       PRINT-PRU.
           MOVE "TTYDEV" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT-BAD
              MOVE TTY-DEVICE TO PRU-FILE
           ELSE
              MOVE GETENV-BUF TO PRU-FILE.
           MOVE "PRU" TO PRU-LP.
           GO TO END-PROG.


      ********************************
      *  PRINT & HOLD  "V" VIEW, CREATE /USR/TMP/XXXX
      ********************************
       PRINT-HOLD.
           IF PRU-DIR = " "
              MOVE SPOOL-PATH-DEFAULT TO PRU-DIR.
           PERFORM SHIFT-PATH.
           MOVE "XXXXXX" TO PRU-X.
           PERFORM SHIFT-PATH.
           MOVE PRU-FILE TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT-BAD
              MOVE "ERROR IN REPORT FILE CREATE" TO MESS
              PERFORM SEND-MESS
              GO TO RESET-P.
           MOVE MKTEMP-BUF TO PRU-FILE.
           MOVE "HLD" TO PRU-LP.
           GO TO END-PROG.


      ********************************
       SHIFT-PATH SECTION.
      ********************************
       SHIFT-BEGIN.
           MOVE 0 TO SUB1.
       SHIFT-FIND-SP.
           ADD 1 TO SUB1.
           IF SUB1 > PRU-PATH-MAX
              GO TO SHIFT-EXIT.
           IF PRU-C(SUB1) NOT = " "
              GO TO SHIFT-FIND-SP.
           MOVE SUB1 TO SUB2.
       SHIFT-FIND-FILE.
           ADD 1 TO SUB2.
           IF SUB2 > PRU-PATH-MAX
              GO TO SHIFT-EXIT.
           IF PRU-C(SUB2) = " "
              GO TO SHIFT-FIND-FILE.
       SHIFT-NEXT-CHAR.
           MOVE PRU-C(SUB2) TO PRU-C(SUB1).
           MOVE " " TO PRU-C(SUB2).
           ADD 1 TO SUB1.
           ADD 1 TO SUB2.
           IF SUB2 NOT > PRU-PATH-MAX
              GO TO SHIFT-NEXT-CHAR.
       SHIFT-EXIT.
           EXIT.


      ******************************************************************
      * CONVERT ACUCOBOL FUNCTION KEYS TO CURRENT PARADATA CODES
      ******************************************************************
       SET-STATUS SECTION.
      * ENTER KEY
              MOVE "00" TO STAT.

           EVALUATE VDU-KEY-DEST
      * ENTER KEY
              WHEN 13 MOVE "00" TO STAT
      * F1 "10"
              WHEN  1 MOVE "10" TO STAT
      * F2 "12"
              WHEN  2 MOVE "12" TO STAT
      * F3 "16"
              WHEN  3 MOVE "16" TO STAT
      * F4 "18"
              WHEN  4 MOVE "18" TO STAT
      * F5 "1:"
              WHEN  5 MOVE "1:" TO STAT
      * F6 "1<"
              WHEN  6 MOVE "1<" TO STAT
      * F7 "1>"
              WHEN  7 MOVE "1>" TO STAT
      * F8 "1 "
              WHEN  8 MOVE "1 " TO STAT
      * F9 "1("
              WHEN  9 MOVE "1(" TO STAT
      * F10 "1)"
              WHEN 10 MOVE "1)" TO STAT
      * F11 "1!"
              WHEN 11 MOVE "1!" TO STAT
      * F12 "1#"
              WHEN 12 MOVE "1#" TO STAT
      * UP ARROW "1U"
              WHEN 52 MOVE "1U" TO STAT
      * DOWN ARROW "1D"
              WHEN 53 MOVE "1D" TO STAT
           END-EVALUATE.
           MOVE STAT TO VDUSTATUS.
           INITIALIZE VDU-KEY-DEST.

      **********************************
      *  MISC
      **********************************
       MISC SECTION.
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SENDMESS.CPY".
           DISPLAY OMITTED WITH BEEP.
       NOT-AVAILABLE.
           MOVE "NOT AVAILABLE" TO MESS.
           PERFORM SEND-MESS.

       END-PROGRAM.
       END-PROG.
           MOVE STAT TO STATX.
           MOVE PRU-PATH TO PRU-PATHX.
       EXIT-PROG.
           EXIT PROGRAM.
