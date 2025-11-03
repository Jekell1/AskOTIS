       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LCFLD.
       AUTHOR.          MJD.
       DATE-WRITTEN.    2016-AUG-23.
      **************************************************************************
      *  DESCRIPTION:  READ LOAN CLASS (LC) FILE.
      *                USED TO OBTAIN A VALUE OF A FIELD IN THE LC
      *                FILE WHEN THE CALLING PROGRAM DOES NOT HAVE
      *                THE LCFILE DEFINED AND DOES NOT CONTAIN THE
      *                LCFILE COPYBOOKS.  INITIALLY CALLED FROM LPAMTS.
      *                NEEDED TO CHECK VALUE OF A LC FIELD BUT THE LPAMYS
      *                COPYBOOK IS USED IN MANY PLACES WHERE THE LCFILE IS NOT
      *                DEFINED. RATHER THAN ADD THE LCFILE TO ALL OF THESE
      *                PLACES A CALL TO THIS PROGRAM WILL ACCOMPLISH THE SAME
      *                THING.
      *
      *        INPUT:  LOAN CLASS
      *                LC FIELD NAME
      *
      *        OUTPUT  VALUE OF LC FIELD
      *                IF FIELD NAME IS NOT RECOGNIZED THE FIELD NAME WILL BE
      *                PASSED BACK TO CALLING PROGRAM.
      *
      *        SAMPLE CALL FROM LPAMTS:
      *
      *              MOVE "CL/LCFIELD" TO FORM-NAM.
      *              MOVE "LC-CALC-HI-CREDIT" TO MESS
      *              CALL FORM-PROGX USING FORM-PATH STAT LN-CLASS MESS.
      *              CANCEL FORM-PROGX.
      *              IF STAT NOT = "00"
      *                 IF MESS = "L" SUBTRACT LIVE-CHECK-FEES FROM HIGH-CREDIT.
      *
      *
      * REVISIONS:
      *  BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      **************************************************************************
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
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)CL/LCFLD.CBL S35 01/31/22-15:34:28 >".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".

       01  ERRCD           PIC X     VALUE SPACES.
       01  HOLD-NUMBER3      PIC 9(8).
       01  HOLD-NUMBER3X REDEFINES HOLD-NUMBER3.
           03  FILLER        PIC X(3).
       01  HOLD-NUMBER5      PIC 9(8).
       01  HOLD-NUMBER5X REDEFINES HOLD-NUMBER5.
           03  FILLER        PIC X(5).
       01  HOLD-NUMBER8      PIC 9(8).
       01  HOLD-NUMBER8X REDEFINES HOLD-NUMBER8.
           03  FILLER        PIC X(8).

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
      * LINKAGE PROGRAM SECTION:
       COPY "LIBGB/GETFMW.CPY".
       01  IO-STAT         PIC X(2).
       01  INPUT-CLASS     PIC 9(3).
       01  IO-BUFFER       PIC X(61).

       PROCEDURE DIVISION
             USING FORM-PATHNAME IO-STAT INPUT-CLASS IO-BUFFER.

      *********************************************************
       MAIN-PROGRAM SECTION.
      *********************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE "00" TO IO-STAT.
           PERFORM OPEN-LC1-FILE.
           MOVE INPUT-CLASS TO LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-BAD
              MOVE IO-FG TO IO-STAT
              GO TO END-ROUTINE.
           CALL "C$TOUPPER" USING IO-BUFFER, VALUE 61.
           EVALUATE IO-BUFFER
              WHEN  "LC-LTOUCH-DATE"
                    MOVE LC-LTOUCH-DATE TO HOLD-NUMBER8
                    MOVE HOLD-NUMBER8X TO IO-BUFFER
              WHEN  "LC-DESC"
                    MOVE LC-DESC TO IO-BUFFER
              WHEN  "LC-SMALLOAN"
                    MOVE LC-SMALLOAN TO IO-BUFFER
              WHEN  "LC-SALEFIN  "
                    MOVE LC-SALEFIN TO IO-BUFFER
              WHEN  "LC-SERVACCT"
                    MOVE LC-SERVACCT TO IO-BUFFER
              WHEN  "LC-LOANTYPE"
                    MOVE LC-LOANTYPE TO IO-BUFFER
              WHEN  "LC-PROF"
                    MOVE LC-PROF TO IO-BUFFER
              WHEN  "LC-PROF1"
                    MOVE LC-PROF1 TO IO-BUFFER
              WHEN  "LC-PROF2 "
                    MOVE LC-PROF2 TO IO-BUFFER
              WHEN  "LC-SKIP-TRW "
                    MOVE LC-SKIP-TRW TO IO-BUFFER
              WHEN  "LC-DISPLAY-ERN"
                    MOVE LC-DISPLAY-ERN TO IO-BUFFER
              WHEN  "LC-LCARDFG"
                    MOVE LC-LCARDFG TO IO-BUFFER
              WHEN  "LC-CALC-HI-CREDIT"
                    MOVE LC-CALC-HI-CREDIT TO IO-BUFFER
              WHEN  "LC-STOP-ON-DLNO"
                    MOVE LC-STOP-ON-DLNO TO IO-BUFFER
              WHEN  "LC-SALES-FORCED-ENTRY"
                    MOVE LC-SALES-FORCED-ENTRY TO IO-BUFFER
              WHEN  "LC-AFP-CLASS"
                    MOVE LC-AFP-CLASS TO IO-BUFFER
              WHEN  "LC-PASTDUE"
                    MOVE LC-PASTDUE TO IO-BUFFER
              WHEN  "LC-CON-PDUE-DAYS"
                    MOVE LC-CON-PDUE-DAYS TO IO-BUFFER
              WHEN  "LC-REC-PDUE-DAYS"
                    MOVE LC-REC-PDUE-DAYS TO IO-BUFFER
              WHEN  "LC-GIFTNET"
                    MOVE LC-GIFTNET TO IO-BUFFER
              WHEN  "LC-APP-REQ-FOR-LOAN"
                    MOVE LC-APP-REQ-FOR-LOAN TO IO-BUFFER
              WHEN  "LC-POOLID"
                    MOVE LC-POOLID TO HOLD-NUMBER5
                    MOVE HOLD-NUMBER5X TO IO-BUFFER
              WHEN  "LC-USE-ALT-GL-FEE"
                    MOVE LC-USE-ALT-GL-FEE TO IO-BUFFER
              WHEN  "LC-PROGRP-CLASS"
                    MOVE LC-PROGRP-CLASS TO HOLD-NUMBER3
                    MOVE HOLD-NUMBER3X TO IO-BUFFER
              WHEN  "LC-ALLOW-CASH-PROCEEDS"
                    MOVE LC-ALLOW-CASH-PROCEEDS TO IO-BUFFER
              WHEN OTHER
                    MOVE "**" TO IO-STAT
           END-EVALUATE.
           PERFORM CLOSE-LC1-FILE.

           GO TO END-ROUTINE.

       COPY "LIBGB/DATER.CPY".
      *******************************************************
       IO-ROUTINE SECTION.
      *******************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LC1-FILE.
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

       END-ROUTINE SECTION.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.

