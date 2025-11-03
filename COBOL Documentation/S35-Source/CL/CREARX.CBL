       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CREARX.
       DATE-WRITTEN.    120496.
      *************************************************************
      *   DIRECTORY  : CL
      *
      *   DESCRIPTION: CALLED PROGRAM TO CREATE / MODIFY / DELETE
      *                RXFILE RECORDS
      *     RESCISSION TRAILER RECORDS
      *
      *   OUTPUT:
      *     IF AN ERROR OCCURS THE CREARX-ERRCD IS SET NON SPACES.
      *
      *   COPY: LIBCL/CREARXW
      *
      *   REV:
      *   MG  990927 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *************************************************************
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
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)CL/CREARX A30 06/06/13-11:36:04 09/18/14-13:46:53 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01RX.CPY".
       COPY "LIBLP/LP01RX_SQL.CPY".
       COPY "LIBLP/LPWSRX.CPY".
       01  ERRCD               PIC X.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBCL/CREARXW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
       01  CREARX-LINK       PIC X(65).
       PROCEDURE DIVISION
            USING FORM-PATHNAME CREARX-LINK EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE CREARX-LINK TO CREARX-AREA.

           MOVE SPACES TO ERRCD.
           IF NOT (CREARX-ACTION = "W" OR "R" OR "D")
              MOVE "E" TO ERRCD
              GO TO END-ROUTINE.

           PERFORM OPEN-RX1-FILE.

           PERFORM SETUP-RX-RECORD.

           IF CREARX-ACTION = "W"
              PERFORM WRITE-RX1-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              ELSE
                 GO TO END-ROUTINE.

           IF CREARX-ACTION = "R"
              PERFORM READ-RX1-FILE
              IF IO-BAD
                 PERFORM SETUP-RX-RECORD
                 PERFORM WRITE-RX1-FILE
                 IF IO-BAD
                    GO TO IO-ERROR
                 ELSE
                    GO TO END-ROUTINE
              ELSE
                 PERFORM REWRITE-RX1-FILE
                 GO TO END-ROUTINE.

           PERFORM READ-RX1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           IF CREARX-ACTION = "D"
              PERFORM DELETE-RX1-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              ELSE
                 GO TO END-ROUTINE.


       UPDATE-RX-RESCISSION-EXIT.
           PERFORM CLOSE-RX1-FILE.

      ****************************************
       MISC SECTION.
      ****************************************
       SETUP-RX-RECORD.
           MOVE SPACES TO RX-REC.

           MOVE CREARX-RX-BRNO     TO RX-BRNO.
           MOVE CREARX-RX-ACCTNO   TO RX-ACCTNO.
           MOVE CREARX-RX-DLNO     TO RX-DLNO.
           MOVE CREARX-RX-INTDATE  TO RX-INTDATE.
           MOVE CREARX-RX-LNAME    TO RX-LNAME.
           MOVE CREARX-RX-FNAME    TO RX-FNAME.
           MOVE CREARX-RX-LOANDATE TO RX-LOANDATE.
           MOVE CREARX-RX-LOANAMT  TO RX-LOANAMT.
           MOVE CREARX-RX-PROCEEDS TO RX-PROCEEDS.
           MOVE CREARX-RX-STATUS   TO RX-STATUS.

       COPY "LIBGB/DATER.CPY".

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-RX1-FILE.
       COPY "LIBLP/LPRXGS_SQL.CPY".
       COPY "LIBLP/LPRX1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           MOVE ERRCD TO CREARX-ERRCD.
           MOVE CREARX-AREA TO CREARX-LINK.
           EXIT PROGRAM.
