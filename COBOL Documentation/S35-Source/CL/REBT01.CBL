      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.       REBT01.
       DATE-WRITTEN.   09/02/92.
      *********************************************************
      *                      (CL)
      *    DESCRIPTION:  TABLE REBATE ROUTINE
      *                  CALLED FROM REBATE ROUTINE
      *
      * REV:
      *  MG  991004 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      *********************************************
       WORKING-STORAGE SECTION.
      *********************************************
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)CL/REBT01 A30 06/06/13-12:22:09 09/18/14-13:47:09 >".
       COPY "LIBLP/LP01RB.CPY".
       COPY "LIBLP/LP01RB_SQL.CPY".
       COPY "LIBLP/LPWSRB.CPY".
       01  ELAPSED-HOLD        PIC S9(4).
       01  ERRCD               PIC X VALUE " ".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       01  REB-COMMON.
           03  REB-COMMON-RTNRB       PIC X.
           03  REB-COMMON-REBATE      PIC S9(7)V99 COMP-3.
           03  REB-COMMON-AREA        PIC X(40).

      * USED FOR SP-RBFRMLA = 'T', TABLE REFUND FORMULA:
           03  REB-T01                REDEFINES REB-COMMON-AREA.
               05  T01-TBL-NO         PIC 99.
               05  T01-ORGTERM        PIC 9(3).
               05  T01-ELAPSED-DAYS   PIC S9(4).
               05  T01-ELAPSED-MONTHS PIC S9(4).
               05  T01-REFUND-RATE    PIC 999V9.
               05  FILLER             PIC X(23).
       COPY "LIBLP/LP01LN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME REB-COMMON LN-REC EXIT-PATHNAME.

      *********************************************
       MAIN-MODULE SECTION.
      *********************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
      D    STOP MESS.

           IF T01-TBL-NO = 0
              GO TO END-PROGRAM.

           PERFORM OPEN-RB1-FILE.

           MOVE T01-TBL-NO  TO RB-TBL-NO.
           MOVE T01-ORGTERM TO RB-TERM.

           PERFORM START-RB1-FILE.
           PERFORM READ-RB1-FILE-NEXT.
           PERFORM CLOSE-RB1-FILE.
           IF NOT (IO-GOOD AND T01-TBL-NO = RB-TBL-NO)
              GO TO END-PROGRAM.

           IF RB-MON-DAY-CD = "M"
              MOVE T01-ELAPSED-MONTHS TO ELAPSED-HOLD
           ELSE
              MOVE T01-ELAPSED-DAYS TO ELAPSED-HOLD.
           IF ELAPSED-HOLD < 0
              GO TO END-PROGRAM.

           SET RBIX TO 1.
           SEARCH RB-TBL
               END MOVE 0 TO T01-REFUND-RATE
                   GO TO END-PROGRAM
               WHEN RB-AMT(RBIX) NOT < ELAPSED-HOLD
                    MOVE RB-RATE(RBIX) TO T01-REFUND-RATE.
           GO TO END-PROGRAM.

       COPY "LIBGB/DATER.CPY".

      *********************************************
       IO-ROUTINE SECTION.
      *********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-RB1-FILE.
       COPY "LIBLP/LPRBGS_SQL.CPY".
       COPY "LIBLP/LPRB1RN.CPY".

      *********************************************
       END-PROGRAM SECTION.
      *********************************************
       ENDIT.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           IF T01-REFUND-RATE = 99.9
              MOVE 100 TO T01-REFUND-RATE.
           EXIT PROGRAM.
